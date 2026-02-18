# Calculate Dissolved Copper Concentration

Calculate Dissolved Copper Concentration

## Usage

``` r
dissolve_cu_df(df, input_water = "defined", water_prefix = TRUE)
```

## Arguments

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md)

- input_water:

  name of the column of Water class data to be used as the input for
  this function. Default is "defined_water".

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`dissolve_cu_df` returns a data frame containing the original data frame
and a column for dissolved copper in mg/L.

## Examples

``` r
cu_calc <- water_df %>%
  define_water_df() %>%
  dissolve_cu_df()
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
#> Warning: This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.
```
