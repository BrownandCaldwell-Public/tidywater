# Determine disinfection credit from chlorine.

This function takes a water defined by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and other disinfection parameters and outputs a data frame of the
required CT (`ct_required`), actual CT (`ct_actual`), and giardia log
removal (`glog_removal`). For a single water, use `solvect_chlorine`; to
apply the model to a dataframe, use `solvect_chlorine_df`. For most
arguments, the `_df` helpers "use_col" default looks for a column of the
same name in the dataframe. The argument can be specified directly in
the function instead or an unquoted column name can be provided.

## Usage

``` r
solvect_chlorine(water, time, residual, baffle, free_cl_slot = "residual_only")

solvect_chlorine_df(
  df,
  input_water = "defined",
  time = "use_col",
  residual = "use_col",
  baffle = "use_col",
  free_cl_slot = "residual_only",
  water_prefix = TRUE
)
```

## Source

Smith et al. (1995)

USEPA (2020)

USEPA (1991)

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).
  Water must include ph and temp

- time:

  Retention time of disinfection segment in minutes.

- residual:

  Minimum chlorine residual in disinfection segment in mg/L as Cl2.

- baffle:

  Baffle factor - unitless value between 0 and 1.

- free_cl_slot:

  Defaults to "residual_only", which uses the residual argument. If
  "slot_only", the model will use the free_chlorine slot in the input
  water. "sum_with_residual", will use the sum of the residual argument
  and the free_chlorine slot.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md)

- input_water:

  name of the column of Water class data to be used as the input for
  this function. Default is "defined_water".

- water_prefix:

  name of the input water used for the calculation will be appended to
  the start of output columns. Default is TRUE.

## Value

`solvect_chlorine` returns a data frame containing required CT
(mg/L*min), actual CT (mg/L*min), giardia log removal, and virus log
removal.

`solvect_chlorine_df` returns a data frame containing the original data
frame and columns for required CT, actual CT, and giardia log removal.

## Details

CT actual is a function of time, chlorine residual, and baffle factor,
whereas CT required is a function of pH, temperature, chlorine residual,
and the standard 0.5 log removal of giardia requirement. CT required is
an empirical regression equation developed by Smith et al. (1995) to
provide conservative estimates for CT tables in USEPA Disinfection
Profiling Guidance. Log removal is a rearrangement of the CT equations.

## Examples

``` r
example_ct <- define_water(ph = 7.5, temp = 25) %>%
  solvect_chlorine(time = 30, residual = 1, baffle = 0.7)
#> Warning: Missing value for alkalinity. Carbonate balance will not be calculated.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
ct_calc <- water_df %>%
  define_water_df() %>%
  solvect_chlorine_df(residual = 2, time = 10, baffle = .5)
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7

chlor_resid <- water_df %>%
  dplyr::mutate(br = 50) %>%
  define_water_df() %>%
  dplyr::mutate(
    residual = seq(1, 12, 1),
    time = seq(2, 24, 2),
    baffle = 0.7
  ) %>%
  solvect_chlorine_df()
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7
#> Warning: Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7
```
