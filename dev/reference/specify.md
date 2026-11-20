# Specify response and explanatory variables

`specify()` is used to specify which columns in the supplied data frame
are the relevant response (and, if applicable, explanatory) variables.
Note that character variables are converted to `factor`s.

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/dev/articles/infer.md).

## Usage

``` r
specify(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  success = NULL,
  offset = NULL
)
```

## Arguments

- x:

  A data frame that can be coerced into a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

- formula:

  A formula with the response variable on the left and the explanatory
  on the right. Alternatively, a `response` and `explanatory` argument
  can be supplied.

- response:

  The variable name in `x` that will serve as the response. This is an
  alternative to using the `formula` argument.

- explanatory:

  The variable name in `x` that will serve as the explanatory variable.
  This is an alternative to using the formula argument.

- success:

  The level of `response` that will be considered a success, as a
  string. Needed for inference on one proportion, a difference in
  proportions, and corresponding z stats.

- offset:

  A variable name in `x` (or an expression involving variables in `x`)
  to be used as an offset in model fitting with
  [`fit()`](https://generics.r-lib.org/reference/fit.html). When
  supplied, [`fit()`](https://generics.r-lib.org/reference/fit.html)
  defaults to `family = "poisson"` and includes `offset(<offset>)` on
  the right-hand side of the model formula. This is primarily used for
  event rate models; see
  [`specify_event_rate()`](https://infer.tidymodels.org/dev/reference/specify_event_rate.md).

## Value

A tibble containing the response (and explanatory, if specified)
variable data.

## See also

Other core functions:
[`calculate()`](https://infer.tidymodels.org/dev/reference/calculate.md),
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md),
[`hypothesize()`](https://infer.tidymodels.org/dev/reference/hypothesize.md)

## Examples

``` r
# specifying for a point estimate on one variable
gss |>
   specify(response = age)
#> Response: age (numeric)
#> # A tibble: 500 × 1
#>      age
#>    <dbl>
#>  1    36
#>  2    34
#>  3    24
#>  4    42
#>  5    31
#>  6    32
#>  7    48
#>  8    36
#>  9    30
#> 10    33
#> # ℹ 490 more rows

# specify a relationship between variables as a formula...
gss |>
  specify(age ~ partyid)
#> Dropping unused factor levels DK from the supplied explanatory
#> variable 'partyid'.
#> Response: age (numeric)
#> Explanatory: partyid (factor)
#> # A tibble: 500 × 2
#>      age partyid
#>    <dbl> <fct>  
#>  1    36 ind    
#>  2    34 rep    
#>  3    24 ind    
#>  4    42 ind    
#>  5    31 rep    
#>  6    32 rep    
#>  7    48 dem    
#>  8    36 ind    
#>  9    30 rep    
#> 10    33 dem    
#> # ℹ 490 more rows

# ...or with named arguments!
gss |>
  specify(response = age, explanatory = partyid)
#> Dropping unused factor levels DK from the supplied explanatory
#> variable 'partyid'.
#> Response: age (numeric)
#> Explanatory: partyid (factor)
#> # A tibble: 500 × 2
#>      age partyid
#>    <dbl> <fct>  
#>  1    36 ind    
#>  2    34 rep    
#>  3    24 ind    
#>  4    42 ind    
#>  5    31 rep    
#>  6    32 rep    
#>  7    48 dem    
#>  8    36 ind    
#>  9    30 rep    
#> 10    33 dem    
#> # ℹ 490 more rows

# more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
