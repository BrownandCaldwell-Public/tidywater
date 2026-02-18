# Calculate pH for water in an open system

Calculates the new water quality (pH, alkalinity, pH dependent ions) for
a water in an open system where CO2(aq) is at equilibrium with
atmospheric CO2. The function takes an object of class "water" and the
partial pressure of CO2, then returns a water class object with updated
water slots. For a single water, use `opensys_ph`; to apply the model to
a dataframe, use `opensys_ph_df`. For most arguments, the \`\_df helper
"use_col" default looks for a column of the same name in the dataframe.
The argument can be specified directly in the function instead or an
unquoted column name can be provided.

## Usage

``` r
opensys_ph(water, partialpressure = 10^-3.42)

opensys_ph_df(
  df,
  input_water = "defined",
  output_water = "opensys",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  partialpressure = "use_col"
)
```

## Arguments

- water:

  Source water of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- partialpressure:

  Partial pressure of CO2 in the air in atm. Default is 10^-3.5 atm,
  which is approximately Pco2 at sea level.

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
  is "opensys".

- pluck_cols:

  Extract water slots modified by the function (ph, alk) into new
  numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

A water with updated pH/alk/etc.

`opensys_ph_df` returns a data frame containing a water class column
with updated ph and alk (and pH dependent ions). Optionally, it also
adds columns for each of those slots individually.

## Details

`opensys_ph` uses the equilibrium concentration of CO2(aq) to determine
the concentrations of carbonate species in the water and the pH by
solving for the CO2 dose that results in a H2CO3 concentration equal to
CO2(aq).

## See also

[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md)

## Examples

``` r
water <- define_water(ph = 7, temp = 25, alk = 5) %>%
  opensys_ph()
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

# \donttest{
example_df <- water_df %>%
  define_water_df() %>%
  opensys_ph_df(
    input_water = "defined", output_water = "opensys",
    partialpressure = 10^-4, pluck_cols = TRUE
  )
# }
```
