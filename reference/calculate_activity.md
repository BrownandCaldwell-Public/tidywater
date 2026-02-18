# Calculate activity coefficients

This function calculates activity coefficients at a given temperature
based on equation 5-43 from Davies (1967), Crittenden et al. (2012)

## Usage

``` r
calculate_activity(z, is, temp)
```

## Arguments

- z:

  Charge of ions in the solution

- is:

  Ionic strength of the solution

- temp:

  Temperature of the solution in Celsius

## Value

A numeric value for the activity coefficient.

## Examples

``` r
calculate_activity(2, 0.1, 25)
#> [1] 0.3727232
```
