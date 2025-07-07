# Cost calculations

#' Determine chemical cost
#'
#' This function takes a chemical dose in mg/L, plant flow, chemical strength, and $/lb and calculates cost.
#'
#' @param dose Chemical dose in mg/L as chemical
#' @param flow Plant flow in MGD
#' @param strength Chemical product strength in percent. Defaults to 100 percent.
#' @param cost Chemical product cost in $/lb
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#'
#' @examples
#' alum_cost <- solvecost_chem(dose = 20, flow = 10, strength = 49, cost = .22)
#'
#' library(dplyr)
#' cost_data <- tibble(
#'   dose = seq(10, 50, 10),
#'   flow = 10
#' ) %>%
#'   mutate(costs = solvecost_chem(dose = dose, flow = flow, strength = 49, cost = .22))
#'
#' @export
#' @returns A numeric value for chemical cost, $/time.
#'
solvecost_chem <- function(dose, flow, strength = 100, cost, time = "day") {
  cost_day <- solvemass_chem(dose, flow, strength) * cost
  if (time == "day") {
    cost_day
  } else if (time == "month") {
    cost_day * 30.4167
  } else if (time == "year") {
    cost_day * 365.25
  } else {
    stop("time must be one of 'day', 'month', 'year'.")
  }
}

#' Determine power cost
#'
#' This function takes kW, % utilization, $/kWhr and determines power cost.
#'
#' @param power Power consumed in kW
#' @param utilization Amount of time equipment is running in percent. Defaults to continuous.
#' @param cost Power cost in $/kWhr
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#'
#' @examples
#' powercost <- solvecost_power(50, 100, .08)
#'
#' library(dplyr)
#' cost_data <- tibble(
#'   power = seq(10, 50, 10),
#'   utilization = 80
#' ) %>%
#'   mutate(costs = solvecost_power(power = power, utilization = utilization, cost = .08))
#'
#' @export
#' @returns A numeric value for power, $/time.
solvecost_power <- function(power, utilization = 100, cost, time = "day") {
  cost_day <- power * cost * 24 * (utilization / 100)
  if (time == "day") {
    cost_day
  } else if (time == "month") {
    cost_day * 30.4167
  } else if (time == "year") {
    cost_day * 365.25
  } else {
    stop("time must be one of 'day', 'month', 'year'.")
  }
}

#' Determine solids disposal cost
#'
#' This function takes coagulant doses in mg/L as chemical, removed turbidity, and cost ($/lb) to determine disposal cost.
#'
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param flow Plant flow in MGD
#' @param toc_removed Amount of total organic carbon removed by the treatment process
#' @param caco3_removed Amount of hardness removed by softening as mg/L CaCO3
#' @param turb Turbidity removed in NTU
#' @param b Correlation factor from turbidity to suspended solids. Defaults to 1.5.
#' @param cost Disposal cost in $/lb
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#' @source https://water.mecc.edu/courses/ENV295Residuals/lesson3b.htm#:~:text=From%20the%20diagram%2C%20for%20example,million%20gallons%20of%20water%20produced.
#'
#' @examples
#' alum_solidscost <- solvecost_solids(alum = 50, flow = 10, turb = 2, cost = 0.05)
#'
#' library(dplyr)
#' cost_data <- tibble(
#'   alum = seq(10, 50, 10),
#'   flow = 10
#' ) %>%
#'   mutate(costs = solvecost_solids(alum = alum, flow = flow, turb = 2, cost = 0.05))
#'
#' @export
#' @returns A numeric value for disposal costs, $/time.
#'
solvecost_solids <- function(alum = 0, ferricchloride = 0, ferricsulfate = 0, flow, toc_removed = 0, caco3_removed = 0, turb, b = 1.5, cost, time = "day") {
  lb_day <- solvemass_solids(alum, ferricchloride, ferricsulfate, flow, toc_removed, caco3_removed, turb, b)
  cost_day <- cost * lb_day # $/lb * lb/day

  if (time == "day") {
    cost_day
  } else if (time == "month") {
    cost_day * 30.4167
  } else if (time == "year") {
    cost_day * 365.25
  } else {
    stop("time must be one of 'day', 'month', 'year'.")
  }
}

#' Determine labor cost
#'
#' This function takes number of FTE and annual $/FTE and determines labor cost
#'
#' @param fte Number of FTEs. Can be decimal.
#' @param cost $/year per FTE
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#'
#' @examples
#' laborcost <- solvecost_labor(1.5, 50000)
#'
#' library(dplyr)
#' cost_data <- tibble(
#'   fte = seq(1, 10, 1)
#' ) %>%
#'   mutate(costs = solvecost_labor(fte = fte, cost = .08))
#'
#' @export
#' @returns A numeric value for labor $/time.
solvecost_labor <- function(fte, cost, time = "day") {
  cost_day <- fte * cost / 365.25
  if (time == "day") {
    cost_day
  } else if (time == "month") {
    cost_day * 30.4167
  } else if (time == "year") {
    cost_day * 365.25
  } else {
    stop("time must be one of 'day', 'month', 'year'.")
  }
}
