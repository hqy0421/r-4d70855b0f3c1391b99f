# Event Rate Analysis -------------------------------------------------------

# shared fixture:
# * d1: online [0,10) + [5,15) (overlapping -> merged [0,15)), excluded
#   [3,6) -> segments [0,3) and [6,12) after clipping to the period
# * d2: online [0,8), no events recorded for it in some tests
# * d3: online [20,30), fully outside the period -> dropped
# * d4: online [0,10), fully excluded -> dropped
online <- data.frame(
  device = c("d1", "d1", "d2", "d3", "d4"),
  start = c(0, 5, 0, 20, 0),
  end = c(10, 15, 8, 30, 10)
)
exclude <- data.frame(
  device = c("d1", "d4"),
  start = c(3, 0),
  end = c(6, 10)
)
events <- data.frame(
  device = c("d1", "d1", "d1", "d1", "d1", "d2", "d2", "d9"),
  event_id = c("e1", "e2", "e3", "e4", "e1", "e5", "e6", "e7"),
  time = c(1, 3, 6, 12, 1, 8, 7.5, 1)
)
covariates <- data.frame(
  device = c("d1", "d2"),
  region = c("north", "south")
)

panel <- build_event_panel(
  online,
  events,
  exclude = exclude,
  period = c(0, 12)
)

test_that("online coverage is merged before exclusions are subtracted", {
  # overlapping online intervals are unioned, not double-counted:
  # d1 contributes 3 + 6 = 9 (not 18), d2 contributes 8
  d1_exposure <- panel$contributions$exposure[panel$contributions$device == "d1"]
  expect_equal(sum(d1_exposure), 9)
  expect_equal(nrow(panel$contributions), 3)

  expect_equal(
    panel$contributions$start[panel$contributions$device == "d1"],
    c(0, 6)
  )
  expect_equal(
    panel$contributions$end[panel$contributions$device == "d1"],
    c(3, 12)
  )
})

test_that("segments are clipped to the reporting period", {
  # d1's merged coverage is [0, 15); the period ends at 12
  expect_true(all(panel$contributions$end <= 12))
  # d3 is only online on [20, 30), outside the period
  expect_false("d3" %in% panel$contributions$device)
})

test_that("devices without exposure are dropped, not given a zero rate", {
  # d3 (outside period) and d4 (fully excluded) have no effective exposure
  expect_setequal(panel$dropped, c("d3", "d4"))
  expect_false(any(panel$contributions$device %in% c("d3", "d4")))
  # no zero-exposure rows are fabricated
  expect_true(all(panel$contributions$exposure > 0))
})

test_that("devices with exposure but zero events are retained", {
  zero_events <- events[0, ]
  zero_panel <- build_event_panel(online, zero_events, period = c(0, 12))

  expect_true(all(zero_panel$contributions$events == 0))
  # d3 has no exposure in the period and is still dropped
  expect_setequal(
    zero_panel$contributions$device,
    c("d1", "d2", "d4")
  )
  expect_setequal(zero_panel$dropped, "d3")
})

test_that("event attribution uses half-open boundaries", {
  attribution <- panel$attribution

  expect_true(attribution$counted[attribution$event_id == "e1"]) # inside
  expect_false(attribution$counted[attribution$event_id == "e2"]) # at exclusion start
  expect_true(attribution$counted[attribution$event_id == "e3"]) # at segment start
  expect_false(attribution$counted[attribution$event_id == "e4"]) # at period end
  expect_false(attribution$counted[attribution$event_id == "e5"]) # at segment end
  expect_true(attribution$counted[attribution$event_id == "e6"]) # inside
  expect_false(attribution$counted[attribution$event_id == "e7"]) # unknown device

  # segment indices are recorded for counted events
  expect_equal(attribution$segment[attribution$event_id == "e1"], 1)
  expect_equal(attribution$segment[attribution$event_id == "e3"], 2)
  expect_true(is.na(attribution$segment[attribution$event_id == "e2"]))
})

test_that("events are deduplicated by stable ID", {
  # e1 appears twice with identical values and is counted once
  expect_equal(sum(panel$attribution$event_id == "e1"), 1)
  expect_equal(nrow(panel$duplicates), 1)
  expect_equal(sum(panel$contributions$events), 3)
})

test_that("conflicting values for the same event ID are an error", {
  conflict_time <- data.frame(
    device = c("d1", "d1"),
    event_id = c("e1", "e1"),
    time = c(1, 2)
  )
  expect_error(
    build_event_panel(online, conflict_time, period = c(0, 12)),
    "Conflicting values"
  )

  conflict_device <- data.frame(
    device = c("d1", "d2"),
    event_id = c("e1", "e1"),
    time = c(1, 1)
  )
  expect_error(
    build_event_panel(online, conflict_device, period = c(0, 12)),
    "Conflicting values"
  )
})

test_that("input validation is explicit", {
  expect_error(
    build_event_panel(online[, -1], events, period = c(0, 12)),
    "not present"
  )
  expect_error(
    build_event_panel(online, events, period = c(12, 0)),
    "before its end"
  )
  expect_error(
    build_event_panel(online, events, period = 1),
    "length 2"
  )

  bad_interval <- data.frame(device = "d1", start = 5, end = 2)
  expect_error(
    build_event_panel(bad_interval, events, period = c(0, 12)),
    "end before they start"
  )

  na_events <- events
  na_events$event_id[1] <- NA
  expect_error(
    build_event_panel(online, na_events, period = c(0, 12)),
    "missing values"
  )

  na_online <- online
  na_online$end[1] <- NA
  expect_error(
    build_event_panel(na_online, events, period = c(0, 12)),
    "missing values"
  )
})

test_that("interval helpers merge and subtract half-open intervals", {
  merged <- merge_intervals(c(0, 5, 20), c(10, 15, 30))
  expect_equal(merged$starts, c(0, 20))
  expect_equal(merged$ends, c(15, 30))

  # touching intervals merge ([0,10) and [10,20) are adjacent)
  touching <- merge_intervals(c(0, 10), c(10, 20))
  expect_equal(touching$starts, 0)
  expect_equal(touching$ends, 20)

  subtracted <- subtract_intervals(0, 10, c(2, 8), c(4, 12))
  expect_equal(subtracted$starts, c(0, 4))
  expect_equal(subtracted$ends, c(2, 8))

  # a hole that covers the base removes it entirely
  expect_equal(length(subtract_intervals(0, 10, 0, 10)$starts), 0)
  # a hole touching the boundary does not remove anything
  expect_equal(subtract_intervals(0, 10, 10, 12)$ends, 10)
})

test_that("exposure units are applied to the offset scale", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  online_dt <- data.frame(
    device = c("a", "b"),
    start = c(t0, t0 + 86400),
    end = c(t0 + 2 * 86400, t0 + 3 * 86400)
  )
  events_dt <- data.frame(
    device = c("a", "b"),
    event_id = c("e1", "e2"),
    time = c(t0 + 3600, t0 + 2 * 86400)
  )
  period_dt <- c(t0, t0 + 4 * 86400)

  panel_days <- build_event_panel(
    online_dt,
    events_dt,
    period = period_dt,
    exposure_unit = "days"
  )
  expect_equal(panel_days$contributions$exposure, c(2, 2))
  expect_s3_class(panel_days$contributions$start, "POSIXct")

  panel_hours <- build_event_panel(
    online_dt,
    events_dt,
    period = period_dt,
    exposure_unit = "hours"
  )
  expect_equal(panel_hours$contributions$exposure, c(48, 48))

  # the offset is log(exposure) in the declared unit: intercepts differ
  # by log(24) between per-day and per-hour rates
  fit_days <- fit(specify_event_rate(panel_days, events ~ 1))
  fit_hours <- fit(specify_event_rate(panel_hours, events ~ 1))
  expect_equal(
    fit_days$estimate - fit_hours$estimate,
    log(24),
    tolerance = 1e-8
  )

  # numeric timelines take a numeric divisor
  panel_native <- build_event_panel(
    data.frame(device = "a", start = 0, end = 48),
    data.frame(device = "a", event_id = "e1", time = 5),
    period = c(0, 100),
    exposure_unit = 24
  )
  expect_equal(panel_native$contributions$exposure, 2)

  # Date inputs round-trip to Date columns
  panel_date <- build_event_panel(
    data.frame(device = "a", start = as.Date("2024-01-01"), end = as.Date("2024-01-11")),
    data.frame(device = "a", event_id = "e1", time = as.Date("2024-01-05")),
    period = as.Date(c("2024-01-01", "2024-02-01"))
  )
  expect_s3_class(panel_date$contributions$start, "Date")
  expect_equal(panel_date$contributions$exposure, 10)

  # mixed numeric and date-like times are rejected
  expect_error(
    build_event_panel(
      data.frame(device = "a", start = as.Date("2024-01-01"), end = as.Date("2024-01-11")),
      data.frame(device = "a", event_id = "e1", time = as.Date("2024-01-05")),
      period = c(0, 10)
    ),
    "all numeric or all date-like"
  )

  expect_error(
    build_event_panel(
      data.frame(device = "a", start = 0, end = 48),
      data.frame(device = "a", event_id = "e1", time = 5),
      period = c(0, 100),
      exposure_unit = "days"
    ),
    "positive number"
  )
})

test_that("specify_event_rate enters native specify with an offset", {
  obj <- specify_event_rate(panel, events ~ region, covariates = covariates)

  expect_s3_class(obj, "infer")
  expect_equal(response_name(obj), "events")
  expect_equal(explanatory_name(obj), "region")
  expect_equal(attr(obj, "offset"), as.name("log_exposure"))
  expect_equal(attr(obj, "exposure_unit"), "native time units")

  # the offset column is retained in the data
  expect_true("log_exposure" %in% names(obj))
  expect_equal(obj$log_exposure, log(panel$contributions$exposure))

  # intercept-only models are supported
  obj_one <- specify_event_rate(panel, events ~ 1)
  expect_s3_class(obj_one, "infer")
  expect_false(has_explanatory(obj_one))
})

test_that("specify_event_rate validates its inputs", {
  expect_error(
    specify_event_rate(panel$contributions, events ~ 1),
    "build_event_panel"
  )
  expect_error(
    specify_event_rate(panel, exposure ~ 1),
    "must be `events`"
  )
  expect_error(
    specify_event_rate(panel, "events ~ 1"),
    "formula"
  )

  # covariates must cover every exposed device, exactly once
  expect_error(
    specify_event_rate(panel, events ~ region, covariates = covariates[1, ]),
    "no row in `covariates`"
  )
  expect_error(
    specify_event_rate(
      panel,
      events ~ region,
      covariates = covariates[c(1, 1, 2), ]
    ),
    "exactly one row per device"
  )

  na_covariates <- covariates
  na_covariates$region[1] <- NA
  expect_error(
    specify_event_rate(panel, events ~ region, covariates = na_covariates),
    "missing"
  )

  # a panel with no effective exposure cannot be modeled
  empty_panel <- build_event_panel(online, events, period = c(100, 200))
  expect_error(
    specify_event_rate(empty_panel, events ~ 1),
    "not estimable"
  )
})

test_that("fit estimates a Poisson rate model with a log-exposure offset", {
  obj <- specify_event_rate(panel, events ~ region, covariates = covariates)
  model_fit <- fit(obj)

  expect_true(is_fitted(model_fit))
  expect_equal(names(model_fit), c("term", "estimate"))

  # coefficients match a direct glm() call on the panel
  direct <- stats::glm(
    events ~ region + offset(log(exposure)),
    data = dplyr::left_join(panel$contributions, covariates, by = "device"),
    family = stats::poisson
  )
  expect_equal(
    model_fit$estimate,
    unname(coef(direct)),
    tolerance = 1e-8
  )

  # intercept-only: the estimated rate is total events / total exposure
  fit_one <- fit(specify_event_rate(panel, events ~ 1))
  expect_equal(fit_one$term, "intercept")
  expect_equal(fit_one$estimate, log(3 / 17), tolerance = 1e-8)

  # the model formula carries the offset term
  expect_equal(
    deparse(get_formula(obj)),
    "events ~ region + offset(log_exposure)"
  )
})

test_that("inestimable models fail explicitly", {
  # no events at all
  zero_panel <- build_event_panel(online, events[0, ], period = c(0, 12))
  expect_error(
    fit(specify_event_rate(zero_panel, events ~ 1)),
    "not estimable"
  )

  # rank-deficient design
  aliased <- data.frame(device = c("d1", "d2"), x1 = c(1, 2), x2 = c(2, 4))
  expect_error(
    fit(specify_event_rate(panel, events ~ x1 + x2, covariates = aliased)),
    "could not be estimated"
  )
})

test_that("fit_event_rate returns contributions, attribution, ids, and coefficients", {
  res <- fit_event_rate(
    online,
    events,
    exclude = exclude,
    period = c(0, 12),
    covariates = covariates,
    formula = events ~ region
  )

  expect_s3_class(res, "event_rate_fit")
  expect_named(
    res,
    c("contributions", "attribution", "ids", "fit", "object")
  )

  # interval contributions
  expect_equal(res$contributions, panel$contributions)

  # event attribution
  expect_equal(res$attribution, panel$attribution)

  # the IDs actually entering the model
  expect_setequal(res$ids$devices, c("d1", "d2"))
  expect_setequal(res$ids$events, c("e1", "e3", "e6"))

  # native coefficients from fit()
  expect_s3_class(res$fit, "tbl_df")
  expect_true(is_fitted(res$fit))
  expect_equal(res$fit$term, c("intercept", "regionsouth"))

  # the infer object used for fitting is returned as well
  expect_s3_class(res$object, "infer")
  expect_equal(attr(res$object, "offset"), as.name("log_exposure"))
})

test_that("event rate print methods work", {
  expect_output(print(panel), "Event rate panel")
  expect_output(print(panel), "Dropped \\(no effective exposure\\): d3, d4")

  res <- fit_event_rate(
    online,
    events,
    exclude = exclude,
    period = c(0, 12),
    covariates = covariates,
    formula = events ~ region
  )
  expect_output(print(res), "Event rate model fit")
  expect_output(print(res), "Devices in model: 2")

  obj <- specify_event_rate(panel, events ~ region, covariates = covariates)
  expect_output(print(obj), "Offset: log_exposure")
})

# specify() offset support ---------------------------------------------------

test_that("specify() supports an offset argument", {
  df <- data.frame(
    events = c(1, 2, 0, 3),
    group = c("a", "a", "b", "b"),
    log_exposure = log(c(1, 2, 3, 4))
  )

  obj <- specify(df, events ~ group, offset = log_exposure)
  expect_equal(attr(obj, "offset"), as.name("log_exposure"))
  expect_true("log_exposure" %in% names(obj))

  # offset objects default to poisson, and the offset enters the formula
  model_fit <- fit(obj)
  direct <- stats::glm(
    events ~ group + offset(log_exposure),
    data = df,
    family = stats::poisson
  )
  expect_equal(model_fit$estimate, unname(coef(direct)), tolerance = 1e-8)

  # an explicit family still takes precedence
  expect_silent(fit(obj, family = "gaussian"))
})

test_that("specify() detects offset() terms in formulas", {
  df <- data.frame(
    events = c(1, 2, 0, 3),
    group = c("a", "a", "b", "b"),
    log_exposure = log(c(1, 2, 3, 4))
  )

  obj <- specify(df, events ~ group + offset(log_exposure))
  expect_true(has_attr(obj, "offset"))
  expect_true("log_exposure" %in% names(obj))

  # the offset is not duplicated in the model formula
  expect_equal(
    deparse(get_formula(obj)),
    "events ~ group + offset(log_exposure)"
  )

  # supplying the offset twice is an error
  expect_error(
    specify(df, events ~ group + offset(log_exposure), offset = log_exposure),
    "only once"
  )

  # offsets must reference existing, numeric variables
  expect_error(
    specify(df, events ~ group, offset = blah),
    "cannot be found"
  )
  expect_error(
    specify(df, events ~ group, offset = group),
    "must be numeric"
  )
  expect_error(
    specify(df, events ~ group, offset = "log_exposure"),
    "bare variable name"
  )
})

test_that("objects without an offset are unaffected", {
  obj <- specify(gss, age ~ sex)
  expect_false(has_attr(obj, "offset"))
  expect_equal(offset_name(obj), character(0))
  expect_false(grepl("offset", deparse(get_formula(obj))))
})

test_that("intercept-only formulas are supported", {
  obj <- specify(mtcars, mpg ~ 1)
  expect_s3_class(obj, "infer")
  expect_false(has_explanatory(obj))

  model_fit <- fit(obj)
  expect_equal(model_fit$term, "intercept")
  expect_equal(model_fit$estimate, mean(mtcars$mpg), tolerance = 1e-8)
})
