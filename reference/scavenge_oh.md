# OH radical scavenging function

The background hydroxyl radical scavenging term is a measure of the
degradation rate of non-target constituents in a water. The term is
defined as the sum of the reaction rate of the constituent multiplied by
the concentration of the constituent: `sum(k_i * [C]i)` where `k_i` is
the reaction rate of a contaminant and `[C]i` its concentration.
Supporting Information in Wang et al. 2019 provides the definition and
additional methods for calculating background scavenging. For a data
frame of waters, use `scavenge_oh_df`, which adds the scavenging term as
a new column.

## Usage

``` r
scavenge_oh(water, dose = 0, kdoc_adjust = 25000)

scavenge_oh_df(
  df,
  input_water = "defined",
  output_column = "oh_scavenging",
  dose = "use_col",
  kdoc_adjust = "use_col"
)
```

## Source

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

Brezonik and Fulkerson-Brekken (1998)

Buxton et al. (1988)

Coddington et al. (1999)

Katsoyiannis et al. (2011)

Larson and Zepp (1988)

Vinge et al. (2020)

Vinge et al. (2021)

Wang et al. (2019)

Wols and Hofman-Caris (2012)

Wols et al. (2014)

## Arguments

- water:

  Source water of tidywater 'water' object type

- dose:

  Hydrogen peroxide (h2o2) dose applied in mg/L, defaults to 0

- kdoc_adjust:

  Optional input for a different k_DOC rate constant for fitting data,
  defaults to 2.5e4 L/mg C/s. See Details for other reported values.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).
  The df may include columns named for the H2O2 dose in mg/L and the DOC
  rate constant.

- input_water:

  name of the column of water class data to be used as the input.
  Default is "defined".

- output_column:

  name of the output column storing the OH radical scavenging term in
  s^-1. Default is "oh_scavenging".

## Value

`scavenge_oh` returns the total background hydroxyl radical scavenging
term in units of s^-1.

`scavenge_oh_df` returns a data frame containing the original data frame
and a column with the background hydroxyl radical scavenging term in
s^-1.

## Details

Rate constants used in the code:

- DOC: 2.5e4 L/mg C/s (Larson and Zepp 1988; 3.0e8 M_C^-1 s^-1 divided
  by 12,011 mg C/mol)

- HCO3-: 8.5e6 M^-1 s^-1 (Buxton et al. 1988)

- CO3 2-: 3.9e8 M^-1 s^-1 (Buxton et al. 1988)

- NO2-: 1.0e10 M^-1 s^-1 (Coddington et al. 1999; Vinge et al. 2020)

- NO3-: 1.0e5 M^-1 s^-1 (Buxton et al. 1988)

- H2O2: 2.7e7 M^-1 s^-1 (Buxton et al. 1988)

Other reported DOC rate constants, for use as `kdoc_adjust`. Sources
report these per mole of carbon (M_C^-1 s^-1); they are converted to
L/mg C/s by dividing by 12,011 mg C/mol. Brezonik and Katsoyiannis
values are as compiled in Wols and Hofman-Caris (2012), Table 2.

- 2.41e4 (2.9e8 M_C^-1 s^-1): lake and river water, Brezonik and
  Fulkerson-Brekken (1998)

- 2.25e4 (2.7e8 M_C^-1 s^-1): Lake Zurich, Katsoyiannis et al. (2011)

- 1.67e4 (2.0e8 M_C^-1 s^-1): Lake Jonsvatnet, Katsoyiannis et al.
  (2011)

- 1.75e4 (2.1e8 M_C^-1 s^-1): Lake Greifensee, Katsoyiannis et al.
  (2011)

- 2.91e4 (3.5e8 M_C^-1 s^-1): effluent organic matter, Katsoyiannis et
  al. (2011)

- 1.67e4 (2.0e8 M_C^-1 s^-1): model value, Wols et al. (2014), Table 3

- 5.29e4 (6.35e8 M_C^-1 s^-1): midpoint of the 3.5e8 to 9.2e8 range
  cited by Vinge et al. (2021)

## Examples

``` r
water1 <- define_water(ph=7, doc=3, alk = 70, no3 = 4)
#> Warning: Missing value for `toc`; DOC assumed to be 95% of TOC.
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
h2o2_dose<-20

background_scavenging <- scavenge_oh(water1, h2o2_dose)

background_SC_adjusted <- scavenge_oh(water1, h2o2_dose, 1.67e4) # Katsoyiannis et al. (2011)


example_df <- water_df %>%
  define_water_df() %>%
  scavenge_oh_df(dose = 10)

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(h2o2 = seq(2, 24, 2)) %>%
  scavenge_oh_df(dose = h2o2, kdoc_adjust = 1.67e4, output_column = "sc")
```
