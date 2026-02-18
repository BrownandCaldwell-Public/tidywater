# Pluck out a single parameter from a `water` class object

This function plucks one or more selected parameters from selected
columns of `water` class objects. The names of the output columns will
follow the form `water_parameter`

## Usage

``` r
pluck_water(df, input_waters = c("defined"), parameter)
```

## Arguments

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- input_waters:

  vector of names of the columns of water class data to be used as the
  input for this function.

- parameter:

  vector of water class parameters to view outside the water column. Can
  also specify "all" to get all non-NA water slots.

## Value

A data frame containing columns of selected parameters from a list of
water class objects.

## See also

[convert_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/convert_water.md)

## Examples

``` r
pluck_example <- water_df %>%
  define_water_df("raw") %>%
  pluck_water(input_waters = c("raw"), parameter = c("hco3", "doc"))
```
