# Fit an event rate model in one step

`fit_event_rate()` chains
[`build_event_panel()`](https://infer.tidymodels.org/dev/reference/build_event_panel.md),
[`specify_event_rate()`](https://infer.tidymodels.org/dev/reference/specify_event_rate.md),
and [`fit()`](https://generics.r-lib.org/reference/fit.html): it
computes effective exposures, attributes events to segments, and fits a
Poisson rate model to the count/exposure panel with `log(exposure)` (in
`exposure_unit`s) as the offset. The model is fitted with infer's native
machinery rather than by aggregating rates externally; if the model is
not estimable, the function errors.

## Usage

``` r
fit_event_rate(
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
)

# S3 method for class 'event_rate_fit'
print(x, ...)
```

## Arguments

- online:

  A data frame of online coverage intervals with columns giving the
  device, the interval start, and the interval end. Intervals may
  overlap.

- events:

  A data frame of events with columns giving the device, a stable event
  ID, and the event time. Rows sharing an event ID are treated as
  retries of the same event and counted once; conflicting device or time
  values for the same ID raise an error.

- exclude:

  An optional data frame of exclusion intervals (e.g. maintenance
  periods), structured like `online`, to subtract from the merged online
  coverage.

- period:

  A vector of length 2, `c(start, end)`, giving the reporting period.
  Only coverage inside this window counts as exposure, and only events
  inside it are counted. Boundaries are half-open.

- covariates:

  An optional data frame of fixed device covariates, with exactly one
  row per device and the same device column as the inputs to
  [`build_event_panel()`](https://infer.tidymodels.org/dev/reference/build_event_panel.md).
  Every device with effective exposure must be present.

- formula:

  A model formula with the panel's `events` column on the left-hand side
  and device covariates on the right-hand side, e.g. `events ~ region`.
  Use `events ~ 1` for an intercept-only (overall rate) model.

- exposure_unit:

  The unit in which exposure is expressed. For `Date` or `POSIXct` time
  variables, one of `"seconds"`, `"minutes"`, `"hours"`, `"days"` (the
  default), or `"weeks"`. For numeric time variables, a single positive
  number giving the number of native time units per exposure unit
  (default `1`).

- device, id, time, start, end:

  Strings naming the device column (shared by all inputs), the event ID
  column, the event time column, and the interval start and end columns.

- ...:

  Additional arguments passed to
  [`fit()`](https://generics.r-lib.org/reference/fit.html), e.g.
  `family`.

- x:

  For the print method, an `event_rate_fit` object.

## Value

An object of class `event_rate_fit`, a list with components:

- `contributions`: The interval contributions—per device and segment,
  the effective exposure and event count entering the model.

- `attribution`: The event attribution—per unique event, the segment it
  was counted towards, if any.

- `ids`: The IDs actually entering the model: `devices` (devices with
  effective exposure) and `events` (unique event IDs counted).

- `fit`: The native model coefficients, as returned by
  [`fit()`](https://generics.r-lib.org/reference/fit.html).

- `object`: The infer object produced by
  [`specify_event_rate()`](https://infer.tidymodels.org/dev/reference/specify_event_rate.md).

## See also

Other event rate functions:
[`build_event_panel()`](https://infer.tidymodels.org/dev/reference/build_event_panel.md),
[`specify_event_rate()`](https://infer.tidymodels.org/dev/reference/specify_event_rate.md)

## Examples

``` r
online <- data.frame(
  device = c("a", "a", "b"),
  start = c(0, 5, 0),
  end = c(10, 15, 8)
)
events <- data.frame(
  device = c("a", "a", "b"),
  event_id = c("e1", "e1", "e2"),
  time = c(3, 3, 7)
)
covariates <- data.frame(
  device = c("a", "b"),
  region = c("north", "south")
)

fit_event_rate(
  online,
  events,
  period = c(0, 20),
  covariates = covariates,
  formula = events ~ region
)
#> # Event rate model fit
#> Exposure unit: native time units
#> Devices in model: 2
#> Events in model: 2
#> 
#> # A tibble: 2 × 2
#>   term        estimate
#>   <chr>          <dbl>
#> 1 intercept     -2.71 
#> 2 regionsouth    0.629
```
