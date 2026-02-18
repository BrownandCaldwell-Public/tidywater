# Calculate Dissolved Copper Concentration

This function takes a water defined by defined_water and output a column
of dissolved copper. It is an empirical model developed based on
bench-scale copper solubility testing that can be used to predict copper
levels as a function of pH, DIC, and orthophosphate. For a single water,
use `dissolve_cu`; to apply the model to a dataframe use
`dissolve_cu_df`.

## Usage

``` r
dissolve_cu(water)
```

## Source

Lytle et al (2018)

## Arguments

- water:

  Source water object of class "water" created by
  [`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).
  Water must include ph and dic

## Value

`dissolve_cu` returns a column containing dissolved copper concentration
in mg/L.

## Details

Dissolved copper is a function of pH, DIC, and PO4. Output units are in
mg/L.

## Examples

``` r
example_cu <- define_water(ph = 7.5, alk = 125, tot_po4 = 2) %>%
  dissolve_cu()
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
```
