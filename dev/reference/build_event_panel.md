# Build an event rate panel from interval and event data

Device failure counts cannot be compared directly: online records may
overlap, and maintenance (or other excluded) periods should not count as
exposure. `build_event_panel()` computes effective exposure and event
counts for event rate analysis:

1.  The online coverage intervals of each device are merged (unioned),
    so overlapping records are not double-counted.

2.  The exclusion intervals (e.g. maintenance periods) are subtracted
    from the merged coverage.

3.  The remaining coverage is clipped to the reporting `period`.

4.  For each device and remaining segment, the effective exposure and
    the number of events are computed. All boundaries are treated as
    half-open, `[start, end)`: an event exactly at a segment start is
    counted, while an event exactly at a segment end is not.

Events are deduplicated by their stable ID: retries of the same event
are counted only once. If the same event ID is recorded with conflicting
device or time values, `build_event_panel()` errors.

Devices with positive effective exposure but no events are retained with
an event count of zero. Devices with no effective exposure at all are
dropped—no zero rate is fabricated for them—and are listed in the
`dropped` component.

## Usage

``` r
build_event_panel(
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
)

# S3 method for class 'event_panel'
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

- exposure_unit:

  The unit in which exposure is expressed. For `Date` or `POSIXct` time
  variables, one of `"seconds"`, `"minutes"`, `"hours"`, `"days"` (the
  default), or `"weeks"`. For numeric time variables, a single positive
  number giving the number of native time units per exposure unit
  (default `1`).

- device, id, time, start, end:

  Strings naming the device column (shared by all inputs), the event ID
  column, the event time column, and the interval start and end columns.

- x:

  For the print method, an `event_panel` object.

- ...:

  For the print method, additional arguments (currently ignored).

## Value

An object of class `event_panel`, a list with components:

- `contributions`: A tibble with one row per device and effective
  segment: the device, a `segment` index, the segment `start` and `end`,
  the effective `exposure` (in `exposure_unit`s), and the number of
  `events` attributed to the segment.

- `attribution`: A tibble with one row per unique event: the event ID,
  device, and time, the `segment` it was attributed to (`NA` if it fell
  outside every effective segment), and whether it was `counted`.

- `duplicates`: Rows of `events` removed as retries of an already
  counted event ID.

- `dropped`: Devices with no effective exposure in the reporting period.

## See also

[`specify_event_rate()`](https://infer.tidymodels.org/dev/reference/specify_event_rate.md)
to declare a model for the panel, and
[`fit_event_rate()`](https://infer.tidymodels.org/dev/reference/fit_event_rate.md)
for the one-shot workflow.

Other event rate functions:
[`fit_event_rate()`](https://infer.tidymodels.org/dev/reference/fit_event_rate.md),
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

# `a` is covered on [0, 15) after merging, `b` on [0, 8);
# the retry of `e1` is counted once
build_event_panel(online, events, period = c(0, 20))
#> # Event rate panel
#> Devices with exposure: 2 (2 segments)
#> Exposure unit: native time units
#> Events: 2 counted / 2 unique (1 duplicate retries removed)
#> 
#> # A tibble: 2 × 6
#>   device segment start   end exposure events
#>   <chr>    <int> <dbl> <dbl>    <dbl>  <int>
#> 1 a            1     0    15       15      1
#> 2 b            1     0     8        8      1
```
