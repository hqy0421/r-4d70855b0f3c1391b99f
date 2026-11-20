# Event Rate Analysis -------------------------------------------------------

#' Build an event rate panel from interval and event data
#'
#' @description
#'
#' Device failure counts cannot be compared directly: online records may
#' overlap, and maintenance (or other excluded) periods should not count as
#' exposure. `build_event_panel()` computes effective exposure and event
#' counts for event rate analysis:
#'
#' 1. The online coverage intervals of each device are merged (unioned), so
#'    overlapping records are not double-counted.
#' 2. The exclusion intervals (e.g. maintenance periods) are subtracted from
#'    the merged coverage.
#' 3. The remaining coverage is clipped to the reporting `period`.
#' 4. For each device and remaining segment, the effective exposure and the
#'    number of events are computed. All boundaries are treated as
#'    half-open, `[start, end)`: an event exactly at a segment start is
#'    counted, while an event exactly at a segment end is not.
#'
#' Events are deduplicated by their stable ID: retries of the same event are
#' counted only once. If the same event ID is recorded with conflicting
#' device or time values, `build_event_panel()` errors.
#'
#' Devices with positive effective exposure but no events are retained with
#' an event count of zero. Devices with no effective exposure at all are
#' dropped---no zero rate is fabricated for them---and are listed in the
#' `dropped` component.
#'
#' @param online A data frame of online coverage intervals with columns
#'   giving the device, the interval start, and the interval end. Intervals
#'   may overlap.
#' @param events A data frame of events with columns giving the device, a
#'   stable event ID, and the event time. Rows sharing an event ID are
#'   treated as retries of the same event and counted once; conflicting
#'   device or time values for the same ID raise an error.
#' @param exclude An optional data frame of exclusion intervals (e.g.
#'   maintenance periods), structured like `online`, to subtract from the
#'   merged online coverage.
#' @param period A vector of length 2, `c(start, end)`, giving the reporting
#'   period. Only coverage inside this window counts as exposure, and only
#'   events inside it are counted. Boundaries are half-open.
#' @param exposure_unit The unit in which exposure is expressed. For `Date`
#'   or `POSIXct` time variables, one of `"seconds"`, `"minutes"`,
#'   `"hours"`, `"days"` (the default), or `"weeks"`. For numeric time
#'   variables, a single positive number giving the number of native time
#'   units per exposure unit (default `1`).
#' @param device,id,time,start,end Strings naming the device column (shared
#'   by all inputs), the event ID column, the event time column, and the
#'   interval start and end columns.
#' @param x For the print method, an `event_panel` object.
#' @param ... For the print method, additional arguments (currently
#'   ignored).
#'
#' @return An object of class `event_panel`, a list with components:
#'
#' * `contributions`: A tibble with one row per device and effective
#'   segment: the device, a `segment` index, the segment `start` and `end`,
#'   the effective `exposure` (in `exposure_unit`s), and the number of
#'   `events` attributed to the segment.
#' * `attribution`: A tibble with one row per unique event: the event ID,
#'   device, and time, the `segment` it was attributed to (`NA` if it fell
#'   outside every effective segment), and whether it was `counted`.
#' * `duplicates`: Rows of `events` removed as retries of an already
#'   counted event ID.
#' * `dropped`: Devices with no effective exposure in the reporting period.
#'
#' @seealso [specify_event_rate()] to declare a model for the panel, and
#'   [fit_event_rate()] for the one-shot workflow.
#'
#' @family event rate functions
#' @export
#' @examples
#' online <- data.frame(
#'   device = c("a", "a", "b"),
#'   start = c(0, 5, 0),
#'   end = c(10, 15, 8)
#' )
#' events <- data.frame(
#'   device = c("a", "a", "b"),
#'   event_id = c("e1", "e1", "e2"),
#'   time = c(3, 3, 7)
#' )
#'
#' # `a` is covered on [0, 15) after merging, `b` on [0, 8);
#' # the retry of `e1` is counted once
#' build_event_panel(online, events, period = c(0, 20))
build_event_panel <- function(
  online,
  events,
  exclude = NULL,
  period,
  exposure_unit = NULL,
  device = "device",
  id = "event_id",
  time = "time",
  start = "start",
  end = "end"
) {
  call <- caller_env()

  # -- validate structure ---------------------------------------------------
  check_type(online, is.data.frame, call = call)
  check_type(events, is.data.frame, call = call)
  check_type(exclude, is.data.frame, allow_null = TRUE, call = call)

  check_column(online, device, "online", call = call)
  check_column(online, start, "online", call = call)
  check_column(online, end, "online", call = call)
  check_column(events, device, "events", call = call)
  check_column(events, id, "events", call = call)
  check_column(events, time, "events", call = call)
  if (!is.null(exclude)) {
    check_column(exclude, device, "exclude", call = call)
    check_column(exclude, start, "exclude", call = call)
    check_column(exclude, end, "exclude", call = call)
  }

  if (!is.atomic(period) || !is.null(dim(period)) || length(period) != 2) {
    cli_abort(
      "`period` must be a vector of length 2, `c(start, end)`.",
      call = call
    )
  }

  # -- validate contents ----------------------------------------------------
  check_no_missing(online[[device]], "`online` device", call = call)
  check_no_missing(online[[start]], "`online` interval start", call = call)
  check_no_missing(online[[end]], "`online` interval end", call = call)
  check_no_missing(events[[device]], "`events` device", call = call)
  check_no_missing(events[[id]], "`events` event ID", call = call)
  check_no_missing(events[[time]], "`events` event time", call = call)
  check_no_missing(period, "`period`", call = call)
  if (!is.null(exclude)) {
    check_no_missing(exclude[[device]], "`exclude` device", call = call)
    check_no_missing(exclude[[start]], "`exclude` interval start", call = call)
    check_no_missing(exclude[[end]], "`exclude` interval end", call = call)
  }

  # -- deduplicate events ---------------------------------------------------
  # retries of the same event (same stable ID) are counted once; the same
  # ID recorded with conflicting device or time values is an error
  dedup <- deduplicate_events(events, id, device, time, call = call)
  events <- dedup$events
  duplicates <- dedup$duplicates

  # -- normalize time variables to a numeric timeline -----------------------
  time_inputs <- list(
    online_start = online[[start]],
    online_end = online[[end]],
    events_time = events[[time]],
    period = period
  )
  if (!is.null(exclude)) {
    time_inputs$exclude_start <- exclude[[start]]
    time_inputs$exclude_end <- exclude[[end]]
  }
  normalized <- normalize_time_inputs(time_inputs, call = call)
  tv <- normalized$values

  period_start <- tv$period[1]
  period_end <- tv$period[2]
  if (!(period_start < period_end)) {
    cli_abort(
      "The start of `period` must be before its end.",
      call = call
    )
  }

  unit <- resolve_exposure_unit(exposure_unit, normalized$out_kind, call = call)

  # -- validate and filter intervals ----------------------------------------
  if (any(tv$online_end < tv$online_start)) {
    cli_abort(
      "Some intervals in `online` end before they start.",
      call = call
    )
  }
  online_keep <- tv$online_end > tv$online_start
  online_device <- online[[device]][online_keep]
  online_start <- tv$online_start[online_keep]
  online_end <- tv$online_end[online_keep]

  exclude_device <- character(0)
  exclude_start <- numeric(0)
  exclude_end <- numeric(0)
  if (!is.null(exclude)) {
    if (any(tv$exclude_end < tv$exclude_start)) {
      cli_abort(
        "Some intervals in `exclude` end before they start.",
        call = call
      )
    }
    exclude_keep <- tv$exclude_end > tv$exclude_start
    exclude_device <- as.character(exclude[[device]][exclude_keep])
    exclude_start <- tv$exclude_start[exclude_keep]
    exclude_end <- tv$exclude_end[exclude_keep]
  }

  # -- effective coverage per device ----------------------------------------
  # merge online coverage, subtract exclusions, clip to the period
  online_key <- as.character(online_device)
  device_keys <- unique(online_key)
  segments <- list()
  for (key in device_keys) {
    in_device <- online_key == key
    in_exclude <- exclude_device == key
    segs <- compute_segments(
      online_start[in_device],
      online_end[in_device],
      exclude_start[in_exclude],
      exclude_end[in_exclude],
      period_start,
      period_end
    )
    if (length(segs$starts) > 0) {
      segments[[key]] <- segs
    }
  }

  # -- attribute events to segments -----------------------------------------
  # an event counts for the (unique) segment with `start <= time < end`
  event_device <- as.character(events[[device]])
  event_time <- to_timeline(events[[time]])
  event_segment <- rep(NA_integer_, nrow(events))
  event_counted <- rep(FALSE, nrow(events))
  for (key in names(segments)) {
    segs <- segments[[key]]
    in_device <- which(event_device == key)
    if (length(in_device) == 0) {
      next
    }
    idx <- findInterval(event_time[in_device], segs$starts)
    hit <- idx >= 1L & event_time[in_device] < segs$ends[pmax(idx, 1L)]
    event_segment[in_device[hit]] <- idx[hit]
    event_counted[in_device[hit]] <- TRUE
  }

  # -- assemble the panel -----------------------------------------------------
  device_values <- online_device[match(device_keys, online_key)]
  contribution_list <- list()
  for (key in names(segments)) {
    segs <- segments[[key]]
    in_device <- which(event_device == key & event_counted)
    counts <- tabulate(event_segment[in_device], nbins = length(segs$starts))
    contribution_list[[key]] <- tibble::tibble(
      segment = seq_along(segs$starts),
      start = from_timeline(segs$starts, normalized$out_kind, normalized$tz),
      end = from_timeline(segs$ends, normalized$out_kind, normalized$tz),
      exposure = (segs$ends - segs$starts) / unit$divisor,
      events = as.integer(counts)
    )
  }

  if (length(contribution_list) > 0) {
    rows_per_device <- vapply(contribution_list, nrow, integer(1))
    contributions <- dplyr::bind_cols(
      tibble::tibble(
        !!device := rep(
          device_values[match(names(contribution_list), device_keys)],
          times = rows_per_device
        )
      ),
      dplyr::bind_rows(contribution_list)
    )
  } else {
    contributions <- tibble::tibble(
      !!device := online[[device]][0],
      segment = integer(0),
      start = from_timeline(numeric(0), normalized$out_kind, normalized$tz),
      end = from_timeline(numeric(0), normalized$out_kind, normalized$tz),
      exposure = numeric(0),
      events = integer(0)
    )
  }

  attribution <- tibble::tibble(
    !!id := events[[id]],
    !!device := events[[device]],
    !!time := events[[time]],
    segment = event_segment,
    counted = event_counted
  )

  dropped_keys <- setdiff(device_keys, names(segments))
  dropped <- device_values[match(dropped_keys, device_keys)]

  structure(
    list(
      contributions = contributions,
      attribution = attribution,
      duplicates = tibble::as_tibble(duplicates),
      dropped = dropped
    ),
    class = "event_panel",
    device = device,
    id = id,
    time = time,
    exposure_unit = unit$label,
    time_kind = normalized$out_kind
  )
}

#' @rdname build_event_panel
#' @export
print.event_panel <- function(x, ...) {
  device <- attr(x, "device")
  n_segments <- nrow(x$contributions)
  n_devices <- length(unique(x$contributions[[device]]))
  n_events <- nrow(x$attribution)
  n_counted <- sum(x$attribution$counted)
  n_duplicates <- nrow(x$duplicates)

  cat("# Event rate panel\n")
  cat(
    "Devices with exposure: ", n_devices,
    " (", n_segments, " segment", if (n_segments == 1) "" else "s", ")\n",
    sep = ""
  )
  cat("Exposure unit: ", attr(x, "exposure_unit"), "\n", sep = "")
  cat(
    "Events: ", n_counted, " counted / ", n_events, " unique",
    if (n_duplicates > 0) {
      paste0(" (", n_duplicates, " duplicate retries removed)")
    } else {
      ""
    },
    "\n",
    sep = ""
  )
  if (length(x$dropped) > 0) {
    cat(
      "Dropped (no effective exposure): ",
      paste(x$dropped, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  cat("\n")

  print(x$contributions)

  invisible(x)
}

#' Specify an event rate model
#'
#' @description
#'
#' `specify_event_rate()` brings an event rate panel (from
#' [build_event_panel()]) and optional fixed device covariates into infer's
#' native [specify()], declaring the model formula for the event counts and
#' setting `log(exposure)`---with exposure expressed in the panel's
#' `exposure_unit`---as the model offset. Passing the result to [fit()]
#' fits a Poisson rate model via [stats::glm()]; devices with positive
#' exposure and zero events remain in the model.
#'
#' @param panel An `event_panel` object, the output of
#'   [build_event_panel()].
#' @param formula A model formula with the panel's `events` column on the
#'   left-hand side and device covariates on the right-hand side, e.g.
#'   `events ~ region`. Use `events ~ 1` for an intercept-only (overall
#'   rate) model.
#' @param covariates An optional data frame of fixed device covariates, with
#'   exactly one row per device and the same device column as the inputs to
#'   [build_event_panel()]. Every device with effective exposure must be
#'   present.
#'
#' @return An infer object (see [specify()]) carrying an `offset`
#'   attribute; pass it to [fit()] to fit the Poisson rate model.
#'
#' @family event rate functions
#' @export
#' @examples
#' online <- data.frame(
#'   device = c("a", "a", "b"),
#'   start = c(0, 5, 0),
#'   end = c(10, 15, 8)
#' )
#' events <- data.frame(
#'   device = c("a", "a", "b"),
#'   event_id = c("e1", "e1", "e2"),
#'   time = c(3, 3, 7)
#' )
#' covariates <- data.frame(
#'   device = c("a", "b"),
#'   region = c("north", "south")
#' )
#'
#' panel <- build_event_panel(online, events, period = c(0, 20))
#' specify_event_rate(panel, events ~ region, covariates = covariates)
specify_event_rate <- function(panel, formula, covariates = NULL) {
  call <- caller_env()

  if (!inherits(panel, "event_panel")) {
    cli_abort(
      "`panel` must be the output of {.fun build_event_panel}.",
      call = call
    )
  }
  if (!rlang::is_formula(formula)) {
    cli_abort(
      "`formula` must be a formula, e.g. `events ~ region`.",
      call = call
    )
  }

  response <- all.vars(f_lhs(formula))
  if (!identical(response, "events")) {
    cli_abort(
      c(
        "The left-hand side of `formula` must be `events`.",
        i = "Event counts are stored in the panel's `events` column, e.g. \\
             `events ~ region`."
      ),
      call = call
    )
  }

  device <- attr(panel, "device")
  data <- panel$contributions

  if (nrow(data) == 0) {
    cli_abort(
      c(
        "The event rate model is not estimable.",
        x = "No device has effective exposure in the reporting period."
      ),
      call = call
    )
  }

  if (!is.null(covariates)) {
    check_type(covariates, is.data.frame, call = call)
    check_column(covariates, device, "covariates", call = call)

    if (anyDuplicated(covariates[[device]])) {
      cli_abort(
        c(
          "`covariates` must contain exactly one row per device.",
          x = "Device(s) {unique(covariates[[device]][duplicated(covariates[[device]])])} \\
               appear more than once."
        ),
        call = call
      )
    }

    reserved <- c("segment", "start", "end", "exposure", "events", "log_exposure")
    collision <- intersect(setdiff(names(covariates), device), reserved)
    if (length(collision) > 0) {
      cli_abort(
        "`covariates` column name(s) {collision} conflict with reserved \\
         panel column names.",
        call = call
      )
    }

    missing_devices <- setdiff(
      as.character(unique(data[[device]])),
      as.character(covariates[[device]])
    )
    if (length(missing_devices) > 0) {
      cli_abort(
        c(
          "Some devices with effective exposure have no row in `covariates`.",
          x = "Missing device(s): {missing_devices}."
        ),
        call = call
      )
    }

    data <- dplyr::left_join(data, covariates, by = device)

    covariate_vars <- setdiff(
      intersect(all.vars(f_rhs(formula)), names(covariates)),
      device
    )
    has_missing <- vapply(
      data[covariate_vars],
      anyNA,
      logical(1)
    )
    if (any(has_missing)) {
      cli_abort(
        c(
          "Covariate values are missing for some exposed devices.",
          x = "Column(s) {names(has_missing)[has_missing]} contain missing \\
               values."
        ),
        call = call
      )
    }
  }

  # the offset is log(exposure), with exposure in the panel's exposure unit
  data$log_exposure <- log(data$exposure)

  x <- specify(data, formula, offset = !!rlang::sym("log_exposure"))
  attr(x, "exposure_unit") <- attr(panel, "exposure_unit")

  x
}

#' Fit an event rate model in one step
#'
#' @description
#'
#' `fit_event_rate()` chains [build_event_panel()], [specify_event_rate()],
#' and [fit()]: it computes effective exposures, attributes events to
#' segments, and fits a Poisson rate model to the count/exposure panel with
#' `log(exposure)` (in `exposure_unit`s) as the offset. The model is fitted
#' with infer's native machinery rather than by aggregating rates
#' externally; if the model is not estimable, the function errors.
#'
#' @inheritParams build_event_panel
#' @inheritParams specify_event_rate
#' @param ... Additional arguments passed to [fit()], e.g. `family`.
#' @param x For the print method, an `event_rate_fit` object.
#'
#' @return An object of class `event_rate_fit`, a list with components:
#'
#' * `contributions`: The interval contributions---per device and segment,
#'   the effective exposure and event count entering the model.
#' * `attribution`: The event attribution---per unique event, the segment
#'   it was counted towards, if any.
#' * `ids`: The IDs actually entering the model: `devices` (devices with
#'   effective exposure) and `events` (unique event IDs counted).
#' * `fit`: The native model coefficients, as returned by [fit()].
#' * `object`: The infer object produced by [specify_event_rate()].
#'
#' @family event rate functions
#' @export
#' @examples
#' online <- data.frame(
#'   device = c("a", "a", "b"),
#'   start = c(0, 5, 0),
#'   end = c(10, 15, 8)
#' )
#' events <- data.frame(
#'   device = c("a", "a", "b"),
#'   event_id = c("e1", "e1", "e2"),
#'   time = c(3, 3, 7)
#' )
#' covariates <- data.frame(
#'   device = c("a", "b"),
#'   region = c("north", "south")
#' )
#'
#' fit_event_rate(
#'   online,
#'   events,
#'   period = c(0, 20),
#'   covariates = covariates,
#'   formula = events ~ region
#' )
fit_event_rate <- function(
  online,
  events,
  exclude = NULL,
  period,
  covariates = NULL,
  formula,
  exposure_unit = NULL,
  device = "device",
  id = "event_id",
  time = "time",
  start = "start",
  end = "end",
  ...
) {
  panel <- build_event_panel(
    online = online,
    events = events,
    exclude = exclude,
    period = period,
    exposure_unit = exposure_unit,
    device = device,
    id = id,
    time = time,
    start = start,
    end = end
  )

  object <- specify_event_rate(
    panel,
    formula = formula,
    covariates = covariates
  )

  fit <- fit(object, ...)

  counted <- panel$attribution$counted
  structure(
    list(
      contributions = panel$contributions,
      attribution = panel$attribution,
      ids = list(
        devices = unique(panel$contributions[[device]]),
        events = panel$attribution[[id]][counted]
      ),
      fit = fit,
      object = object
    ),
    class = "event_rate_fit"
  )
}

#' @rdname fit_event_rate
#' @export
print.event_rate_fit <- function(x, ...) {
  cat("# Event rate model fit\n")
  cat("Exposure unit: ", attr(x$object, "exposure_unit"), "\n", sep = "")
  cat("Devices in model: ", length(x$ids$devices), "\n", sep = "")
  cat("Events in model: ", length(x$ids$events), "\n", sep = "")
  cat("\n")

  print(x$fit)

  invisible(x)
}

# Internal helpers ----------------------------------------------------------

check_column <- function(df, column, df_arg, call = caller_env()) {
  if (!rlang::is_string(column)) {
    cli_abort(
      "Column name arguments must be strings.",
      call = call
    )
  }
  if (!(column %in% names(df))) {
    cli_abort(
      "The column `{column}` is not present in `{df_arg}`.",
      call = call
    )
  }

  invisible(TRUE)
}

check_no_missing <- function(x, label, call = caller_env()) {
  if (anyNA(x)) {
    cli_abort(
      "The {label} column contains missing values, which are not supported.",
      call = call
    )
  }

  invisible(TRUE)
}

# classify a time variable
time_kind <- function(x) {
  if (inherits(x, "POSIXt")) {
    "posixct"
  } else if (inherits(x, "Date")) {
    "date"
  } else if (is.numeric(x)) {
    "numeric"
  } else {
    "invalid"
  }
}

# convert a time variable to the numeric timeline (seconds for date-like
# inputs, native units for numeric inputs)
to_timeline <- function(x) {
  if (inherits(x, "POSIXt")) {
    as.numeric(x)
  } else if (inherits(x, "Date")) {
    as.numeric(as.POSIXct(x, tz = "UTC"))
  } else {
    as.numeric(x)
  }
}

# convert values on the numeric timeline back to the input's time class
from_timeline <- function(x, out_kind, tz) {
  switch(
    out_kind,
    numeric = x,
    date = as.Date(.POSIXct(x, tz = "UTC")),
    posixct = .POSIXct(x, tz = tz)
  )
}

# normalize all time inputs to a common numeric timeline, checking that
# they are of a consistent kind
normalize_time_inputs <- function(times, call = caller_env()) {
  kinds <- vapply(times, time_kind, character(1))
  nonempty <- lengths(times) > 0
  check_kinds <- kinds[nonempty]

  if (any(check_kinds == "invalid")) {
    cli_abort(
      "Time variables must be numeric, Date, or POSIXct.",
      call = call
    )
  }

  is_datetime <- check_kinds %in% c("date", "posixct")
  if (any(is_datetime) && !all(is_datetime)) {
    cli_abort(
      c(
        "Time variables must be all numeric or all date-like.",
        x = "Mixing numeric and Date/POSIXct time variables is not supported."
      ),
      call = call
    )
  }

  out_kind <- "numeric"
  tz <- "UTC"
  if (length(check_kinds) > 0 && all(is_datetime)) {
    out_kind <- if (any(check_kinds == "posixct")) "posixct" else "date"
    for (x in times) {
      if (inherits(x, "POSIXt")) {
        tz <- attr(x, "tzone")
        if (is.null(tz) || length(tz) == 0 || !nzchar(tz[1])) {
          tz <- "UTC"
        }
        tz <- tz[1]
        break
      }
    }
  }

  list(
    values = lapply(times, to_timeline),
    out_kind = out_kind,
    tz = tz
  )
}

# resolve the exposure unit to a divisor on the numeric timeline and a
# label for messages
resolve_exposure_unit <- function(exposure_unit, out_kind, call = caller_env()) {
  if (out_kind == "numeric") {
    if (is.null(exposure_unit)) {
      return(list(divisor = 1, label = "native time units"))
    }
    if (
      !is_single_number(exposure_unit, min_val = 0, include_min_val = FALSE)
    ) {
      cli_abort(
        c(
          "For numeric time variables, `exposure_unit` must be a single \\
           positive number.",
          i = "It gives the number of native time units per exposure unit."
        ),
        call = call
      )
    }
    list(
      divisor = unname(exposure_unit),
      label = paste0("units of ", exposure_unit, " native time units")
    )
  } else {
    if (is.null(exposure_unit)) {
      exposure_unit <- "days"
    }
    seconds <- c(
      seconds = 1,
      secs = 1,
      minutes = 60,
      mins = 60,
      hours = 3600,
      days = 86400,
      weeks = 604800
    )
    canonical <- c(
      seconds = "seconds",
      secs = "seconds",
      minutes = "minutes",
      mins = "minutes",
      hours = "hours",
      days = "days",
      weeks = "weeks"
    )
    if (
      !rlang::is_string(exposure_unit) || !(exposure_unit %in% names(seconds))
    ) {
      cli_abort(
        c(
          "For date-like time variables, `exposure_unit` must be one of \\
           \"seconds\", \"minutes\", \"hours\", \"days\", or \"weeks\".",
          x = "You supplied {.val {exposure_unit}}."
        ),
        call = call
      )
    }
    list(
      divisor = unname(seconds[exposure_unit]),
      label = unname(canonical[exposure_unit])
    )
  }
}

# merge half-open [start, end) intervals: union overlapping or touching
# intervals. inputs need not be sorted; outputs are sorted and disjoint.
merge_intervals <- function(starts, ends) {
  if (length(starts) == 0) {
    return(list(starts = numeric(0), ends = numeric(0)))
  }

  o <- order(starts, ends)
  starts <- starts[o]
  ends <- ends[o]

  # a new merged interval begins wherever an interval starts after the
  # running maximum end of all previous intervals
  running_max <- cummax(ends)
  new_group <- starts > c(-Inf, running_max[-length(running_max)])
  group <- cumsum(new_group)

  list(
    starts = unname(as.vector(tapply(starts, group, min))),
    ends = unname(as.vector(tapply(ends, group, max)))
  )
}

# subtract merged, sorted half-open hole intervals from merged, sorted
# half-open base intervals
subtract_intervals <- function(base_starts, base_ends, hole_starts, hole_ends) {
  out_starts <- numeric(0)
  out_ends <- numeric(0)
  hole <- 1L
  n_holes <- length(hole_starts)

  for (i in seq_along(base_starts)) {
    current <- base_starts[i]
    base_end <- base_ends[i]

    # skip holes entirely before this base interval
    while (hole <= n_holes && hole_ends[hole] <= current) {
      hole <- hole + 1L
    }

    j <- hole
    while (j <= n_holes && hole_starts[j] < base_end) {
      if (hole_starts[j] > current) {
        out_starts <- c(out_starts, current)
        out_ends <- c(out_ends, min(hole_starts[j], base_end))
      }
      current <- max(current, hole_ends[j])
      if (current >= base_end) {
        break
      }
      j <- j + 1L
    }

    if (current < base_end) {
      out_starts <- c(out_starts, current)
      out_ends <- c(out_ends, base_end)
    }
  }

  list(starts = out_starts, ends = out_ends)
}

# effective segments for one device: merge online coverage, subtract
# exclusions, clip to the reporting period
compute_segments <- function(
  online_starts,
  online_ends,
  exclude_starts,
  exclude_ends,
  period_start,
  period_end
) {
  segments <- merge_intervals(online_starts, online_ends)

  if (length(exclude_starts) > 0) {
    holes <- merge_intervals(exclude_starts, exclude_ends)
    segments <- subtract_intervals(
      segments$starts,
      segments$ends,
      holes$starts,
      holes$ends
    )
  }

  starts <- pmax(segments$starts, period_start)
  ends <- pmin(segments$ends, period_end)
  keep <- starts < ends

  list(starts = starts[keep], ends = ends[keep])
}

# deduplicate events by stable ID: retries (identical device and time) are
# dropped; conflicting values for the same ID are an error
deduplicate_events <- function(events, id, device, time, call = caller_env()) {
  ids <- events[[id]]
  id_key <- as.character(ids)
  is_duplicate <- duplicated(id_key)
  duplicates <- events[0, , drop = FALSE]

  if (any(is_duplicate)) {
    first <- match(id_key, id_key)
    same_device <- as.character(events[[device]]) ==
      as.character(events[[device]])[first]
    same_time <- events[[time]] == events[[time]][first]
    conflict <- is_duplicate & !(same_device & same_time)

    if (any(conflict)) {
      bad_ids <- unique(ids[conflict])
      cli_abort(
        c(
          "Conflicting values were recorded for the same event ID.",
          x = "Event ID(s) {bad_ids} appear with differing device or time \\
               values.",
          i = "Retries of the same event must record identical values; each \\
               unique event is counted only once."
        ),
        call = call
      )
    }

    duplicates <- events[is_duplicate, , drop = FALSE]
    events <- events[!is_duplicate, , drop = FALSE]
  }

  list(events = events, duplicates = duplicates)
}
