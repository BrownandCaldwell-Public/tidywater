# Correct acid dissociation constants

This function calculates the corrected equilibrium constant for
temperature and ionic strength

## Usage

``` r
correct_k(water)
```

## Arguments

- water:

  Defined water with values for temperature and ion concentrations

## Value

A dataframe with equilibrium constants for co3, po4, so4, ocl, and nh4.

## Examples

``` r
water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
correct_k(water_defined)
#>          k1co3       k2co3       k1po4        k2po4        k3po4         kocl
#> 1 4.974812e-07 5.82167e-11 0.008584056 8.453217e-08 6.537799e-13 3.151725e-08
#>           knh4      kso4         kbo4       k1sio4       k2sio4      kch3coo
#> 1 4.674863e-10 0.0164065 6.200956e-10 1.423268e-10 6.745386e-14 2.057709e-05
#>       k1citric     k2citric     k3citric
#> 1 0.0008539198 2.360233e-05 6.657483e-07
```
