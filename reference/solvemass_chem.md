# Convert mg/L of chemical to lb/day

This function takes a chemical dose in mg/L, plant flow in MGD, and
chemical strength and calculates lb/day of product

## Usage

``` r
solvemass_chem(dose, flow, strength = 100)
```

## Arguments

- dose:

  Chemical dose in mg/L as chemical

- flow:

  Plant flow in MGD

- strength:

  Chemical product strength in percent. Defaults to 100 percent.

## Value

A numeric value for the chemical mass in lb/day.

## Examples

``` r
alum_mass <- solvemass_chem(dose = 20, flow = 10, strength = 49)

library(dplyr)
mass_data <- tibble(
  dose = seq(10, 50, 10),
  flow = 10
) %>%
  mutate(mass = solvemass_chem(dose = dose, flow = flow, strength = 49))
```
