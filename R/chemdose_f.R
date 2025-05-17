# Fluoride removal model
# Removed from exports because it's not a real model, just a placeholder equation.
# Applies equation of the form: raw_f - A*alum^a*ph ^ b * raw_f^c. There is no published model, so it is
# recommended to fit the coefficients with experimental data. When fitting, the following units must be used:
# Alum in mg/L as chemical, Fluoride in mg/L, pH in SU. Default coefficients are fit from Sollo et al (1978). This function
# outputs a water class object with an updated fluoride concentration (which will be in M, per standard water units).

# @param water Source water object of class "water" created by \code{\link{define_water}}
# @param alum Amount of hydrated aluminum sulfate added in mg/L: Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
# @param coeff Model coefficients to use as vector of numbers.

# @examples
# dosed_water <- define_water(ph = 7, temp = 25, alk = 50, f = 4) %>%
#   chemdose_ph(alum = 50) %>%
#   chemdose_f(alum = 50)
#
# convert_units(dosed_water@f, "f", "M", "mg/L")
#
# @returns A water class object with an updated fluoride concentration.
chemdose_f <- function(water, alum, coeff = c(1.11, .628, -2.07, .861)) {
  if (is.na(water@ph) | is.na(water@f)) {
    stop("Water must include pH and fluoride. Specify in define water.")
  }
  if (length(coeff) != 4 | !is.numeric(coeff)) {
    stop("coeff must be specified as a numeric vector of 4 coefficients.")
  }

  ph <- water@ph
  raw_f <- convert_units(water@f, "f", "M", "mg/L")

  f_treated <- raw_f - coeff[1] * alum^coeff[2] * ph^coeff[3] * raw_f^coeff[4]

  water@f <- convert_units(f_treated, "f")

  return(water)
}
