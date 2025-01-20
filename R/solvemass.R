#' Convert mg/L of chemical to lb/day
#'
#' This function takes a chemical dose in mg/L, plant flow, and chemical strength and calculates lb/day of product
#'
#' @param dose Chemical dose in mg/L as chemical
#' @param flow Plant flow in MGD
#' @param strength Chemical product strength in percent. Defaults to 100 percent.
#'
#' @examples
#' alum_mass <- solvemass_chem(dose = 20, flow = 10, strength = 49)
#'
#' library(dplyr)
#' mass_data <- tibble(
#'   dose = seq(10, 50, 10),
#'   flow = 10
#' ) %>%
#'   mutate(mass = solvemass_chem(dose = dose, flow = flow, strength = 49))
#'
#' @export
#' @returns  A numeric value for the chemical mass in lb/day.
#'
solvemass_chem <- function(dose, flow, strength = 100) {
  dose * flow * 8.34 / (strength / 100) # 8.34 lb/mg/L/MG
}


#' Determine solids lb/day
#'
#' This function takes coagulant doses in mg/L as chemical, removed turbidity, and plant flow to determine solids production.
#'
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param flow Plant flow in MGD
#' @param turb Turbidity removed in NTU
#' @param b Correlation factor from turbidity to suspended solids. Defaults to 1.5.
#' @source https://water.mecc.edu/courses/ENV295Residuals/lesson3b.htm#:~:text=From%20the%20diagram%2C%20for%20example,million%20gallons%20of%20water%20produced.
#'
#' @examples
#' solids_mass <- solvemass_solids(alum = 50, flow = 10, turb = 20)
#'
#' library(dplyr)
#' mass_data <- tibble(
#'   alum = seq(10, 50, 10),
#'   flow = 10
#' ) %>%
#'   mutate(mass = solvemass_solids(alum = alum, flow = flow, turb = 20))
#' #'
#' @export
#' @returns A numeric value for solids mass in lb/day.
#'
solvemass_solids <- function(alum = 0, ferricchloride = 0, ferricsulfate = 0, flow, turb, b = 1.5) {
  suspended <- turb * b
  # 2 mol of Fe added per mol of ferric sulfate
  fe <- ferricsulfate * (tidywater::mweights$fe * 2 / tidywater::mweights$ferricsulfate)

  8.34 * flow * (0.44 * alum + 2.9 * fe + ferricchloride + suspended)
}
