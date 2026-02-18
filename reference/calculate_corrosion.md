# Calculate six corrosion and scaling indices (AI, RI, LSI, LI, CSMR, CCPP)

This function takes an object created by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and calculates corrosion and scaling indices. For a single water, use
`calculate_corrosion`; to apply the calculations to a dataframe, use
`calculate_corrosion_df`.

## Usage

``` r
calculate_corrosion(
  water,
  index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
  form = "calcite"
)

calculate_corrosion_df(
  df,
  input_water = "defined",
  water_prefix = TRUE,
  index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
  form = "calcite"
)
```

## Source

AWWA (1977)

Crittenden et al. (2012)

Langelier (1936)

Larson and Skold (1958)

Merrill and Sanks (1977a)

Merrill and Sanks (1977b)

Merrill and Sanks (1978)

Nguyen et al. (2011)

Plummer and Busenberg (1982)

Ryznar (1944)

Schock (1984)

Trussell (1998)

U.S. EPA (1980)

See reference list at
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- index:

  The indices to be calculated. Default calculates all six indices:
  "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"
  CCPP may not be able to be calculated sometimes, so it may be
  advantageous to leave this out of the function to avoid errors

- form:

  Form of calcium carbonate mineral to use for modelling solubility:
  "calcite" (default), "aragonite", or "vaterite"

- df:

  a data frame containing a water class column, created using
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- input_water:

  name of the column of water class data to be used as the input.
  Default is "defined".

- water_prefix:

  append water name to beginning of output columns. Defaults to TRUE

## Value

`calculate_corrosion` returns a data frame with corrosion and scaling
indices as individual columns.

`calculate_corrosion_df` returns the a data frame containing specified
corrosion and scaling indices as columns.

## Details

Aggressiveness Index (AI), unitless - the corrosive tendency of water
and its effect on asbestos cement pipe.

Ryznar Index (RI), unitless - a measure of scaling potential.

Langelier Saturation Index (LSI), unitless - describes the potential for
calcium carbonate scale formation. Equations use empirical calcium
carbonate solubilities from Plummer and Busenberg (1982) and Crittenden
et al. (2012) rather than calculated from the concentrations of calcium
and carbonate in the water.

Larson-skold Index (LI), unitless - describes the corrosivity towards
mild steel.

Chloride-to-sulfate mass ratio (CSMR), mg Cl/mg SO4 - indicator of
galvanic corrosion for lead solder pipe joints.

Calcium carbonate precipitation potential (CCPP), mg/L as CaCO3 - a
prediction of the mass of calcium carbonate that will precipitate at
equilibrium. A positive CCPP value indicates the amount of CaCO3 (mg/L
as CaCO3) that will precipitate. A negative CCPP indicates how much
CaCO3 can be dissolved in the water.

## Examples

``` r
water <- define_water(
  ph = 8, temp = 25, alk = 200, tot_hard = 200,
  tds = 576, cl = 150, so4 = 200
)
#> Warning: Missing values for calcium and magnesium but total hardness supplied. Default ratio of 65% Ca2+ and 35% Mg2+ will be used.
corrosion_indices <- calculate_corrosion(water)
#> Warning: Calcium estimated by previous tidywater function, aggressiveness index calculation approximate.

water <- define_water(ph = 8, temp = 25, alk = 100, tot_hard = 50, tds = 200)
#> Warning: Missing values for calcium and magnesium but total hardness supplied. Default ratio of 65% Ca2+ and 35% Mg2+ will be used.
corrosion_indices <- calculate_corrosion(water, index = c("aggressive", "ccpp"))
#> Warning: Calcium estimated by previous tidywater function, aggressiveness index calculation approximate.


example_df <- water_df %>%
  define_water_df() %>%
  calculate_corrosion_df(index = c("aggressive", "ccpp"))
```
