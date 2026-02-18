# Determine disinfection credit from ozone.

This function takes a water defined by
[`define_water()`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and the first order decay curve parameters from an ozone dose and
outputs a dataframe of actual CT, and log removal for giardia, virus,
and crypto. For a single water, use `solvect_o3`; to apply the model to
a dataframe, use `solvect_o3_df`. For most arguments, the `_df` helper
"use_col" default looks for a column of the same name in the dataframe.
The argument can be specified directly in the function instead or an
unquoted column name can be provided.

## Usage

``` r
solvect_o3(water, time, dose, kd, baffle)

solvect_o3_df(
  df,
  input_water = "defined",
  time = "use_col",
  dose = "use_col",
  kd = "use_col",
  baffle = "use_col",
  water_prefix = TRUE
)
```

## Source

USEPA (2020) Equation 4-4 through 4-7
https://www.epa.gov/system/files/documents/2022-02/disprof_bench_3rules_final_508.pdf

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [`define_water()`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).
  Water must include ph and temp

- time:

  Retention time of disinfection segment in minutes.

- dose:

  Ozone dose in mg/L. This value can also be the y intercept of the
  decay curve (often slightly lower than ozone dose.)

- kd:

  First order decay constant. This parameter is optional. If not
  specified, the default ozone decay equations will be used.

- baffle:

  Baffle factor - unitless value between 0 and 1.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [`define_water_df()`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).

- input_water:

  name of the column of Water class data to be used as the input for
  this function. Default is "defined_water".

- water_prefix:

  name of the input water used for the calculation will be appended to
  the start of output columns. Default is TRUE.

## Value

`solvect_o3` returns a data frame containing actual CT (mg/L\*min),
giardia log removal, virus log removal, and crypto log removal.

`solvect_o3_df` returns a data frame containing the original data frame
and columns for required CT, actual CT, and giardia log removal.

## Details

First order decay curve for ozone has the form:
`residual = dose * exp(kd*time)`. kd should be a negative number. Actual
CT is an integration of the first order curve. The first 30 seconds are
removed from the integral to account for instantaneous demand.

When `kd` is not specified, a default decay curve is used from the Water
Treatment Plant Model (2002). This model does not perform well for ozone
decay, so specifying the decay curve is recommended.

## Examples

``` r
# Use kd from experimental data (recommended):
define_water(ph = 7.5, temp = 25) %>%
  solvect_o3(time = 10, dose = 2, kd = -0.5, baffle = 0.9)
#> Warning: Missing value for alkalinity. Carbonate balance will not be calculated.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
#>   ct_actual glog_removal vlog_removal clog_removal
#> 1  2.779426     17.22941     34.85294     1.131231
# Use modeled decay curve:
define_water(ph = 7.5, alk = 100, doc = 2, uv254 = .02, br = 50) %>%
  solvect_o3(time = 10, dose = 2, baffle = 0.5)
#> Warning: Missing value for TOC. DOC assumed to be 95% of TOC.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
#>   ct_actual glog_removal vlog_removal clog_removal
#> 1  4.624554     28.66719     57.99013     1.882201

# \donttest{
ct_calc <- water_df %>%
  dplyr::mutate(br = 50) %>%
  define_water_df() %>%
  dplyr::mutate(
    dose = 2,
    O3time = 10,
  ) %>%
  solvect_o3_df(time = O3time, baffle = .7)
# }
```
