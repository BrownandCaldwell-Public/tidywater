# check a water object for common issues and applicable models

This function takes a previously defined water class input and checks
common mismatch issues with water quality. It returns an unnamed list
with the check, the value, and a flag if there is an issue. It will also
check to see what other tidywater models are applicable to the input
water unless the user specifies to skip that step.

## Usage

``` r
check_water(water, skip_models = FALSE)
```

## Arguments

- water:

  a water class object

- skip_models:

  a boolean to specify whether to skip the check for applicable models,
  defaults to FALSE

## Value

an unnamed list with the check, value, and flag for any issues found. If
skip_models = FALSE, also includes a check for applicable models with
tidywater functions.

## Details

This function serves as a heuristic check for users to check common
issues with water data.

## Examples

``` r

water1<-define_water(ph=7, alk = 70)
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
check_water(water1)
#> [[1]]
#> [1] "Total hardness check: tot_hard, Ca, or Mg missing."
#> 
#> [[2]]
#> [1] "Ion Balance Reasonable Percent Difference: 200%. Unreasonable PD > 10%."
#> 
#> [[3]]
#> [1] "TDS check: TDS or ions missing, cannot compare to sum of ions."
#> 
#> [[4]]
#> [1] "Ionic strength: IS missing."
#> 
#> [[5]]
#> [1] "Applicable models: chemdose_ph, decarbonate_ph, dissolve_cu, opensys_ph, chemdose_chloramine."
#> 

water2<-define_water( ph=7, alk = 70, ca = 20, na = 20, cl = 20, so4 = 20)
#> Warning: Missing values for `mg` and `tot_hard`; estimated from calcium using a 65%/35%
#> Ca/Mg split.
check_water(water2, skip_models = TRUE)
#> [[1]]
#> [1] "TDS check: 43.6%. Unreasonable PD > 10%."
#> 

```
