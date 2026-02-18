# Calculate unit conversions for common compounds

This function takes a value and converts units based on compound name.

## Usage

``` r
convert_units(value, formula, startunit = "mg/L", endunit = "M")
```

## Arguments

- value:

  Value to be converted

- formula:

  Chemical formula of compound. Accepts compounds in mweights for
  conversions between g and mol or eq

- startunit:

  Units of current value, currently accepts g/L; g/L CaCO3; g/L N; M;
  eq/L; and the same units with "m", "u", "n" prefixes

- endunit:

  Desired units, currently accepts same as start units

## Value

A numeric value for the converted parameter.

## Examples

``` r
convert_units(50, "ca") # converts from mg/L to M by default
#> [1] 0.001247567
convert_units(50, "ca", "mg/L", "mg/L CaCO3")
#> [1] 124.8651
convert_units(50, "ca", startunit = "mg/L", endunit = "eq/L")
#> [1] 0.002495134
```
