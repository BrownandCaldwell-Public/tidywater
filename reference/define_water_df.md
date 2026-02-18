# Apply `define_water` within a dataframe and output a column of `water` class to be chained to other tidywater functions

This function allows
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
to be added to a piped data frame. Its output is a `water` class, and
can therefore be chained with "downstream" tidywater functions.

## Usage

``` r
define_water_df(
  df,
  output_water = "defined",
  pluck_cols = FALSE,
  water_prefix = TRUE
)
```

## Arguments

- df:

  a data frame containing columns with all the desired parameters with
  column names matching argument names in define_water

- output_water:

  name of the output column storing updated parameters with the class,
  water. Default is "defined".

- pluck_cols:

  Extract primary water slots (ph, alk, doc, uv254) into new numeric
  columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

A data frame containing a water class column.

## See also

[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

## Examples

``` r
# \donttest{
example_df <- water_df %>%
  define_water_df() %>%
  balance_ions_df()

example_df <- water_df %>%
  define_water_df(output_water = "This is a column of water") %>%
  balance_ions_df(input_water = "This is a column of water")
# }
```
