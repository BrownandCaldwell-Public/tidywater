# Create summary table from water class

This function takes a water data frame defined by
[`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and outputs a formatted summary table of specified water quality
parameters.

`summarise_wq()` and `summarize_wq()` are synonyms.

## Usage

``` r
summarize_wq(water, params = c("general"))

summarise_wq(water, params = c("general"))
```

## Arguments

- water:

  Source water vector created by
  [`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).

- params:

  List of water quality parameters to be summarized. Options include
  "general", "ions", and "dbps". Defaults to "general" only.

## Value

A knitr_kable table of specified water quality parameters.

## Details

Use
[`chemdose_dbp`](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_dbp.md)
for modeled DBP concentrations.

## Examples

``` r
# Summarize general parameters
water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#> Warning: User entered total hardness is >10% different than calculated hardness.
summarize_wq(water_defined)
#> 
#> 
#> General water quality parameters      Result  Units         
#> ---------------------------------  ---------  --------------
#> pH                                    7.0000  -             
#> Temp                                 20.0000  deg C         
#> Alkalinity                           50.0000  mg/L as CaCO3 
#> Total_Hardness                      240.9638  mg/L as CaCO3 
#> TDS                                 232.4338  mg/L          
#> Conductivity                        363.1779  uS/cm         
#> TOC                                       NA  mg/L          

# Summarize major cations and anions
summarize_wq(water_defined, params = list("ions"))
#> 
#> 
#> Major ions    Concentration (mg/L)
#> -----------  ---------------------
#> Na                           10.00
#> Ca                           80.00
#> Mg                           10.00
#> K                            10.00
#> Cl                           10.00
#> SO4                             NA
#> HCO3                         60.62
#> CO3                           0.03
```
