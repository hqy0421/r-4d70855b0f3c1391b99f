# Specify an event rate model

`specify_event_rate()` brings an event rate panel (from
[`build_event_panel()`](https://infer.tidymodels.org/dev/reference/build_event_panel.md))
and optional fixed device covariates into infer's native
[`specify()`](https://infer.tidymodels.org/dev/reference/specify.md),
declaring the model formula for the event counts and setting
`log(exposure)`—with exposure expressed in the panel's
`exposure_unit`—as the model offset. Passing the result to
[`fit()`](https://generics.r-lib.org/reference/fit.html) fits a Poisson
rate model via [`stats::glm()`](https://rdrr.io/r/stats/glm.html);
devices with positive exposure and zero events remain in the model.

## Usage

``` r
specify_event_rate(panel, formula, covariates = NULL)
```

## Arguments

- panel:

  An `event_panel` object, the output of
  [`build_event_panel()`](https://infer.tidymodels.org/dev/reference/build_event_panel.md).

- formula:

  A model formula with the panel's `events` column on the left-hand side
  and device covariates on the right-hand side, e.g. `events ~ region`.
  Use `events ~ 1` for an intercept-only (overall rate) model.

- covariates:

  An optional data frame of fixed device covariates, with exactly one
  row per device and the same device column as the inputs to
  [`build_event_panel()`](https://infer.tidymodels.org/dev/reference/build_event_panel.md).
  Every device with effective exposure must be present.

## Value

An infer object (see
[`specify()`](https://infer.tidymodels.org/dev/reference/specify.md))
carrying an `offset` attribute; pass it to
[`fit()`](https://generics.r-lib.org/reference/fit.html) to fit the
Poisson rate model.

## See also

Other event rate functions:
[`build_event_panel()`](https://infer.tidymodels.org/dev/reference/build_event_panel.md),
[`fit_event_rate()`](https://infer.tidymodels.org/dev/reference/fit_event_rate.md)

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

panel <- build_event_panel(online, events, period = c(0, 20))
specify_event_rate(panel, events ~ region, covariates = covariates)
#> Response: events (numeric)
#> Explanatory: region (factor)
#> Offset: log_...
#> # A tibble: 2 × 3
#>   events region log_exposure
#>    <dbl> <fct>         <dbl>
#> 1      1 north          2.71
#> 2      1 south          2.08
```
