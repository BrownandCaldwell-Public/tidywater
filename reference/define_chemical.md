# Create a chemical class object given a chemical's parameters

Creates an S4 chemical class object that stores a chemical's formula,
dose, strength, dose units, hydration, cost, and solids formula.

## Usage

``` r
define_chemical(
  formula,
  dose = 0,
  strength = 100,
  dose_unit = "mg/L as chemical",
  hydration = NA_real_,
  cost = NA_real_,
  solids_formula = NA_character_
)
```

## Arguments

- formula:

  Associated chemical formula

- dose:

  Dosage of the chemical

- strength:

  from 100% to 1%, measure of the amount of chemical to its solution

- dose_unit:

  Units the chemical is in, defaults to mg/L as chemical

- hydration:

  Waters of hydration

- cost:

  How much the chemical costs

- solids_formula:

  formula for solids

## Value

An S4 chemical class object with user defined parameters.

## Examples

``` r

naoh_50p <- define_chemical(formula="naoh",dose=12, strength = 50, dose_unit = "M")
```
