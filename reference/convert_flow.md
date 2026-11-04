# Convert flow rate

Given units for flow rate, convert to different units of flow rate.

## Usage

``` r
convert_flow(value, startunit, endunit, sg = 1)
```

## Arguments

- value:

  Value of flow rate to be converted

- startunit:

  Current units of flow rate

- endunit:

  Desired units of flow rate

- sg:

  Optional argument for specific gravity, default to 1. Required for
  converting between mass and volumetric flows.

## Value

A numeric value for the converted flow

## Examples

``` r
flow <- 40 #in MGD

convert_flow(flow, "MGD", "gpm")
#> [1] 27777.76
```
