# Create summary plot of ions from water class

This function takes a water data frame defined by
[`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and outputs an ion balance plot.

## Usage

``` r
plot_ions(water)
```

## Arguments

- water:

  Source water vector created by link function here

## Value

A ggplot object displaying the water's ion balance.

## Examples

``` r
# \donttest{
water <- define_water(7, 20, 50, 100, 20, 10, 10, 10, 10, tot_po4 = 1)
plot_ions(water)

# }
```
