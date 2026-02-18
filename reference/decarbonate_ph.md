# Apply decarbonation to a water

Calculates the new water quality (pH, alkalinity, etc) after a specified
amount of CO2 is removed (removed as bicarbonate). The function takes an
object of class "water" and a fraction of CO2 removed, then returns a
water class object with updated water slots. For a single water, use
`decarbonate_ph`; to apply the model to a dataframe, use
`decarbonate_ph_df`. For a single water use `chemdose_toc`; for a
dataframe use `chemdose_toc_df`. Use `pluck_cols = TRUE` to get values
from the output water as new dataframe columns. For most arguments in
the `_df` helper "use_col" default looks for a column of the same name
in the dataframe. The argument can be specified directly in the function
instead or an unquoted column name can be provided.

## Usage

``` r
decarbonate_ph(water, co2_removed)

decarbonate_ph_df(
  df,
  input_water = "defined",
  output_water = "decarbonated",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  co2_removed = "use_col"
)
```

## Arguments

- water:

  Source water of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- co2_removed:

  Fraction of CO2 removed

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include a column with names for each of the chemicals being
  dosed.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "decarbonated".

- pluck_cols:

  Extract water slots modified by the function (ph, alk) into new
  numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

A water with updated pH/alk/etc.

`decarbonate_ph_df` returns a data frame containing a water class column
with updated ph and alk (and pH dependent ions). Optionally, it also
adds columns for each of those slots individually.

## Details

`decarbonate_ph` uses `water@h2co3` to determine the existing CO2 in
water, then applies
[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md)
to match the CO2 removal.

## See also

[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md)

## Examples

``` r
water <- define_water(ph = 4, temp = 25, alk = 5) %>%
  decarbonate_ph(co2_removed = .95)
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.


example_df <- water_df %>%
  define_water_df() %>%
  decarbonate_ph_df(
    input_water = "defined", output_water = "decarb",
    co2_removed = .95, pluck_cols = TRUE
  )
```
