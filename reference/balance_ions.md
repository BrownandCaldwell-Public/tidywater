# Add an ion to balance overall charge in a water

This function takes a water defined by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and balances charge. For a single water use `balance_ions`; for a
dataframe use `balance_ions_df`. Use `pluck_cols = TRUE` to get values
from the output water as new dataframe columns.

## Usage

``` r
balance_ions(water, anion = "cl", cation = "na")

balance_ions_df(
  df,
  input_water = "defined",
  output_water = "balanced",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  anion = "cl",
  cation = "na"
)
```

## Arguments

- water:

  Water created with
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md),
  which may have some ions set to 0 when unknown

- anion:

  Selected anion to use to for ion balance when more cations are
  present. Defaults to "cl". Choose one of c("cl", "so4").

- cation:

  Selected cation to use to for ion balance when more anions are
  present. Defaults to "na". Choose one of c("na", "k", "ca", or "mg").

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md)

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined_water".

- output_water:

  name of the output column storing updated water classes. Default is
  "balanced_water".

- pluck_cols:

  Extract water slots modified by the function (selected cation and
  anion) into new numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`balance_ions` returns a single water class object with updated ions to
balance water charge.

`balance_ions_df` returns a dataframe with a new column with the ion
balanced water

## Details

If more cations are needed, sodium will be added. User may specify which
cation ("na", "k", "ca", or "mg") to use for balancing. If calcium and
magnesium are not specified when defining a water with
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md),
they will default to 0 and not be changed by this function unless
specified in the cation argument. Anions are added by default with
chloride. User may specify which anion ("cl", "so4") to use for
balancing. This function is purely mathematical. User should always
check the outputs to make sure values are reasonable for the input
source water.

## Examples

``` r
water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10) %>%
  balance_ions()
#> Warning: User entered total hardness is >10% different than calculated hardness.

water_defined <- define_water(7, 20, 50, tot_hard = 150) %>%
  balance_ions(anion = "so4")
#> Warning: Missing values for calcium and magnesium but total hardness supplied. Default ratio of 65% Ca2+ and 35% Mg2+ will be used.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

example_df <- water_df %>%
  define_water_df() %>%
  balance_ions_df(anion = "so4", cation = "ca")
```
