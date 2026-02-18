# Determine ozone decay

This function applies the ozone decay model to a `water` from U.S. EPA
(2001) equation 5-128. For a single water, use `solveresid_o3`; to apply
the model to a dataframe, use `solveresid_o3_df`. For most arguments,
the `_df` helper "use_col" default looks for a column of the same name
in the dataframe. The argument can be specified directly in the function
instead or an unquoted column name can be provided.

## Usage

``` r
solveresid_o3(water, dose, time)

solveresid_o3_df(
  df,
  input_water = "defined",
  output_column = "o3resid",
  dose = "use_col",
  time = "use_col"
)
```

## Source

U.S. EPA (2001)

See reference list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class `water` created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- dose:

  Applied ozone dose in mg/L

- time:

  Ozone contact time in minutes

- df:

  a data frame containing a water class column, which has already been
  computed using
  [`define_water_df`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md)

- input_water:

  name of the column of Water class data to be used as the input for
  this function. Default is "defined".

- output_column:

  name of the output column storing doses in mg/L. Default is
  "dose_required".

## Value

`solveresid_o3` returns a numeric value for the residual ozone.

`solveresid_o3_df` returns a data frame containing the original data
frame and columns for ozone dosed, time, and ozone residual.

## Examples

``` r
ozone_resid <- define_water(7, 20, 100, doc = 2, toc = 2.2, uv254 = .02, br = 50) %>%
  solveresid_o3(dose = 2, time = 10)
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

ozone_resid <- water_df %>%
  dplyr::mutate(br = 50) %>%
  define_water_df() %>%
  solveresid_o3_df(dose = 2, time = 10)

ozone_resid <- water_df %>%
  dplyr::mutate(br = 50) %>%
  define_water_df() %>%
  dplyr::mutate(
    dose = seq(1, 12, 1),
    time = seq(2, 24, 2)
  ) %>%
  solveresid_o3_df()
```
