# Determine solids disposal cost

This function takes coagulant doses in mg/L as chemical, removed
turbidity, and cost (\$/lb) to determine disposal cost.

## Usage

``` r
solvecost_solids(
  alum = 0,
  ferricchloride = 0,
  ferricsulfate = 0,
  flow,
  toc_removed = 0,
  caco3_removed = 0,
  turb,
  b = 1.5,
  cost,
  time = "day"
)
```

## Source

https://water.mecc.edu/courses/ENV295Residuals/lesson3b.htm#:~:text=From%20the%20diagram%2C%20for%20example,million%20gallons%20of%20water%20produced.

## Arguments

- alum:

  Hydrated aluminum sulfate Al2(SO4)3\*14H2O + 6HCO3 -\> 2Al(OH)3(am)
  +3SO4 + 14H2O + 6CO2

- ferricchloride:

  Ferric Chloride FeCl3 + 3HCO3 -\> Fe(OH)3(am) + 3Cl + 3CO2

- ferricsulfate:

  Amount of ferric sulfate added in mg/L: Fe2(SO4)3\*8.8H2O + 6HCO3 -\>
  2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2

- flow:

  Plant flow in MGD

- toc_removed:

  Amount of total organic carbon removed by the treatment process in
  mg/L

- caco3_removed:

  Amount of hardness removed by softening as mg/L CaCO3

- turb:

  Turbidity removed in NTU

- b:

  Correlation factor from turbidity to suspended solids. Defaults to
  1.5.

- cost:

  Disposal cost in \$/lb

- time:

  Desired output units, one of c("day", "month", "year"). Defaults to
  "day".

## Value

A numeric value for disposal costs, \$/time.

## Examples

``` r
alum_solidscost <- solvecost_solids(alum = 50, flow = 10, turb = 2, cost = 0.05)

cost_data <- data.frame(
  alum = seq(10, 50, 10),
  flow = 10
) %>%
  dplyr::mutate(costs = solvecost_solids(alum = alum, flow = flow, turb = 2, cost = 0.05))
```
