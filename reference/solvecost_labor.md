# Determine labor cost

This function takes number of FTE and annual \$/FTE and determines labor
cost

## Usage

``` r
solvecost_labor(fte, cost, time = "day")
```

## Arguments

- fte:

  Number of FTEs. Can be decimal.

- cost:

  \$/year per FTE

- time:

  Desired output units, one of c("day", "month", "year"). Defaults to
  "day".

## Value

A numeric value for labor \$/time.

## Examples

``` r
laborcost <- solvecost_labor(1.5, 50000)

cost_data <- data.frame(
  fte = seq(1, 10, 1)
) %>%
  dplyr::mutate(costs = solvecost_labor(fte = fte, cost = .08))
```
