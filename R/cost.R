# Cost calculations

#' @title Convert mg/L of chemical to lb/day
#'
#' @description This function takes a chemical dose in mg/L, plant flow, and chemical strength and calculates lb/day of product
#'
#' @param dose Chemical dose in mg/L as chemical
#' @param flow Plant flow in MGD
#' @param strength Chemical product strength in percent. Defaults to 100%.
#'
#' @examples
#' alum_mass <- solvemass_chem(dose = 20, flow = 10, strength = 49)
#'
#' @export
#'
solvemass_chem <- function(dose, flow, strength = 100) {
  dose * flow * 8.34 / (strength / 100)
}

#' @title Determine chemical cost
#'
#' @description This function takes a chemical dose in mg/L, plant flow, chemical strength, and $/lb and calculates cost.
#'
#' @param dose Chemical dose in mg/L as chemical
#' @param flow Plant flow in MGD
#' @param strength Chemical product strength in percent. Defaults to 100%.
#' @param cost Chemical product cost in $/lb
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#'
#' @examples
#' alum_cost <- solvecost_chem(dose = 20, flow = 10, strength = 49, cost = .22)
#'
#' @export
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

#' @title Determine power cost
#'
#' @description This function takes kW, % utilization, $/kWhr and determines power cost.
#'
#' @param power Power consumed in kW
#' @param utilization Amount of time equipment is running in %. Defaults to continuous (100%).
#' @param cost Power cost in $/kWhr
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#'
#' @examples
#' powercost <- solvecost_power(50, 100, .08)
#'
#' @export
#'
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

#' @title Determine solids disposal cost
#'
#' @description This function takes coagulant doses in mg/L as chemical, raw turbidity, % solids, and $/lb to determine disposal cost.
#'
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param cost Disposal cost in $/lb
#' @param flow Plant flow in MGD
#' @param turb Turbidity removed in NTU
#' @param b Correlation factor from turbidity to suspended solids. Defaults to 1.5.
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#' @source https://water.mecc.edu/courses/ENV295Residuals/lesson3b.htm#:~:text=From%20the%20diagram%2C%20for%20example,million%20gallons%20of%20water%20produced.
#'
#' @examples
#' alum_solidscost <- solvecost_solids(alum = 50, flow = 10, turb = 2, cost = 5)
#'
#' @export
#'
solvecost_solids <- function(alum = 0, ferricchloride = 0, ferricsulfate = 0, flow, turb, b = 1.5, cost, time = "day") {
  suspended <- turb * b
  fe <- ferricsulfate * (mweights$fe * 2 / mweights$ferricsulfate) # 2 mol of Fe added per mol of ferric sulfate

  lb_day <- 8.34 * flow * (0.44 * alum + 2.9 * fe + ferricchloride + suspended)
  cost_day <- cost / lb_day

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

#' @title Determine labor cost
#'
#' @description This function takes number of FTE and annual $/FTE and determines labor cost
#'
#' @param fte Number of FTEs. Can be decimal.
#' @param cost $/year per FTE
#' @param time Desired output units, one of c("day", "month", "year"). Defaults to "day".
#'
#' @examples
#' laborcost <- solvecost_labor(1.5, 50000)
#'
#' @export
#'
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
