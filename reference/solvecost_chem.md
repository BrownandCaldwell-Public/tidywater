# Determine chemical cost

This function takes a chemical dose in mg/L, plant flow, chemical
strength, and \$/lb and calculates cost.

## Usage

``` r
solvecost_chem(dose, flow, strength = 100, cost, time = "day")
```

## Arguments

- dose:

  Chemical dose in mg/L as chemical

- flow:

  Plant flow in MGD

- strength:

  Chemical product strength in percent. Defaults to 100 percent.

- cost:

  Chemical product cost in \$/lb

- time:

  Desired output units, one of c("day", "month", "year"). Defaults to
  "day".

## Value

A numeric value for chemical cost, \$/time.

## Examples

``` r
alum_cost <- solvecost_chem(dose = 20, flow = 10, strength = 49, cost = .22)

cost_data <- data.frame(
  dose = seq(10, 50, 10),
  flow = 10
) %>%
  dplyr::mutate(costs = solvecost_chem(dose = dose, flow = flow, strength = 49, cost = .22))
```
