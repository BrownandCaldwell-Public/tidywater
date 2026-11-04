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
  [`define_water`](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).
  Water must include ph and dic

## Value

`dissolve_cu` returns a column containing dissolved copper concentration
in mg/L.

## Details

Dissolved copper is a function of pH, DIC (mg/L as C), and
orthophosphate (mg/L as PO4). Output units are in mg/L. The model was
fit on waters with pH 6.48 to 8.52, orthophosphate 0.2 to 3.1 mg/L as
PO4, and DIC of 10, 50, and 100 mg/L as C (Lytle et al. 2018, equation
1); a warning is given for inputs outside these ranges.

DIC is not an input:
[`define_water`](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)
calculates it from pH and alkalinity. Lytle et al. (2018) give no
alkalinity range, and the alkalinity that corresponds to a given DIC
depends on pH. At 25 C, the 10 to 100 mg/L as C DIC range is roughly 24
to 244 mg/L as CaCO3 alkalinity at pH 6.5, 34 to 340 at pH 7, 39 to 390
at pH 7.5, and 41 to 410 at pH 8.

## Examples

``` r

example_cu <- define_water(ph = 7.5, alk = 125, tot_po4 = 2) %>%
  dissolve_cu()
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
```
