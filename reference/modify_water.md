# Modify slots in a `water` class object

This function modifies selected slots of a `water` class object without
impacting the other parameters. For example, you can manually update
"tthm" and the new speciation will not be calculated. This function is
designed to make sure all parameters are stored in the correct units
when manually updating a water. Some slots cannot be modified with this
function because they are interconnected with too many others (usually
pH dependent, eg, hco3). For those parameters, update
[define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).

## Usage

``` r
modify_water(water, slot, value, units)

modify_water_df(
  df,
  input_water = "defined",
  output_water = "modified",
  slot = "use_col",
  value = "use_col",
  units = "use_col"
)
```

## Arguments

- water:

  A water class object

- slot:

  A vector of slots in the water to modify, eg, "tthm"

- value:

  A vector of new values for the modified slots

- units:

  A vector of units for each value being entered, typically one of
  c("mg/L", "ug/L", "M", "cm-1"). For ions any units supported by
  [convert_units](https://brownandcaldwell-public.github.io/tidywater/reference/convert_units.md)
  are allowed. For organic carbon, one of "mg/L", "ug/L". For uv254 one
  of "cm-1", "m-1". For DBPs, one of "ug/L" or "mg/L".

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated parameters with the class,
  water. Default is "modified".

## Value

A data frame containing columns of selected parameters from a list of
water class objects.

`modify_water_df` returns a data frame containing a water class column
with updated slot

## Details

In `modify_water_df`, `slot`, `value`, and `units` can each be a
constant, an unquoted column name, or left as "use_col" to read a column
of the same name ("slot", "value", "units"). Constants are applied to
every row as given, so `slot = c("br", "na")`, `value = c(50, 60)`,
`units = c("ug/L", "mg/L")` sets both slots on each water. Unlike other
`_df` functions, vector constants are not expanded into one row per
combination. Columns are read row by row.

## Examples

``` r
water1 <- define_water(ph = 7, alk = 100, tds = 100, toc = 5) %>%
  modify_water(slot = "toc", value = 4, units = "mg/L")
#> Warning: Missing value for `doc`; defaulting to 95% of TOC.

water2 <- define_water(ph = 7, alk = 100, tds = 100, toc = 5, ca = 10) %>%
  modify_water(slot = c("ca", "toc"), value = c(20, 10), units = c("mg/L", "mg/L"))
#> Warning: Missing values for `mg` and `tot_hard`; estimated from calcium using a 65%/35%
#> Ca/Mg split.
#> Warning: Missing value for `doc`; defaulting to 95% of TOC.


example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(bromide = 50) %>%
  modify_water_df(slot = "br", value = bromide, units = "ug/L")

example_df <- water_df %>%
  define_water_df() %>%
  modify_water_df(
    slot = c("br", "na"),
    value = c(50, 60),
    units = c("ug/L", "mg/L")
  )
```
