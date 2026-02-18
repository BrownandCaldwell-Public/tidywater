# Determine power cost

This function takes kW, % utilization, \$/kWhr and determines power
cost.

## Usage

``` r
solvecost_power(power, utilization = 100, cost, time = "day")
```

## Arguments

- power:

  Power consumed in kW

- utilization:

  Amount of time equipment is running in percent. Defaults to
  continuous.

- cost:

  Power cost in \$/kWhr

- time:

  Desired output units, one of c("day", "month", "year"). Defaults to
  "day".

## Value

A numeric value for power, \$/time.

## Examples

``` r
powercost <- solvecost_power(50, 100, .08)

cost_data <- data.frame(
  power = seq(10, 50, 10),
  utilization = 80
) %>%
  dplyr::mutate(costs = solvecost_power(power = power, utilization = utilization, cost = .08))
```
