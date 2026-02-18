# Calculate hardness from calcium and magnesium

This function takes Ca and Mg in mg/L and returns hardness in mg/L as
CaCO3

## Usage

``` r
calculate_hardness(ca, mg, type = "total", startunit = "mg/L")
```

## Arguments

- ca:

  Calcium concentration in mg/L as Ca

- mg:

  Magnesium concentration in mg/L as Mg

- type:

  "total" returns total hardness, "ca" returns calcium hardness.
  Defaults to "total"

- startunit:

  Units of Ca and Mg. Defaults to mg/L

## Value

A numeric value for the total hardness in mg/L as CaCO3.

## Examples

``` r
calculate_hardness(50, 10)
#> [1] 166.0447

water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#> Warning: User entered total hardness is >10% different than calculated hardness.
calculate_hardness(water_defined@ca, water_defined@mg, "total", "M")
#> [1] 240.9638
```
