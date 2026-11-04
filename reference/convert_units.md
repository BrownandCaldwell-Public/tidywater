# Calculate unit conversions for common compounds

This function takes a value and converts units based on compound name.

## Usage

``` r
convert_units(value, formula = NULL, startunit = "mg/L", endunit = "M")
```

## Arguments

- value:

  Value to be converted

- formula:

  Chemical formula of compound; accepts compounds in mweights. Optional
  when converting within the same unit type and basis (e.g., mg/L to
  g/L, mM to M, meq/L to eq/L, or mg/L CaCO3 to g/L CaCO3).

- startunit:

  Units of current value, currently accepts g/L; g/L CaCO3; g/L N; g/L
  P; g/L Cl2; M; eq/L; and the same units with "m", "u", "n" prefixes

- endunit:

  Desired units, currently accepts same as start units

## Value

A numeric value for the converted parameter.

## Details

Mass concentrations may be expressed as CaCO3, N, P, or molecular
chlorine (Cl2). Mass as CaCO3 is an equivalents basis: 1 eq of the
compound = 50 g as CaCO3, so 61 mg/L HCO3 is 50 mg/L as CaCO3. Mass as N
or P counts the atoms in the compound: 1 mol of (NH4)2SO4 is 28 g as N.
Mass as Cl2 counts oxidizing capacity: 1 mol of NHCl2 is 2 mol as Cl2.
"mg/L Cl" is treated as "mg/L Cl2".

## Examples

``` r
convert_units(50, "ca") # converts from mg/L to M by default
#> [1] 0.001247567
convert_units(50, "ca", "mg/L", "mg/L CaCO3")
#> [1] 124.8651
convert_units(50, "ca", startunit = "mg/L", endunit = "eq/L")
#> [1] 0.002495134
```
