# DBP Modeling functions
# These functions help predict total trihalomethane (TTHM) and haloacetic acid (HAA) formation


#' Create a water class object given water quality parameters
#'
#' This function takes user-defined water quality parameters and creates an S4 "water" class object that forms the input and output of all tidywater models.
#' Carbonate balance is calculated and units are converted to mol/L. Ionic strength is determined from ions, TDS, or conductivity. Missing values are handled by defaulting to 0 or
#' NA. Calcium hardness defaults to 65% of the total hardness because that falls within a typical range. For best results
#' manually specify all ions in the define_water arguments. The following equations are used to determine ionic strength:
#' Ionic strength (if TDS provided): MWH equation 5-38
#' Ionic strength (if electrical conductivity provided): Snoeyink & Jenkins 1980
#' Ionic strength (from ion concentrations): Lewis and Randall (1921), MWH equation 5-37
#' Temperature correction of dielectric constant (relative permittivity): Harned and Owen (1958), MWH equation 5-45.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2 applied chlorine dose (mg/L as Cl2). Dose should be between 1.51 and 33.55 mg/L
#' @param br bromide concentration (ug/L). Concentration should be between 7 and 600 ug/L
#' @param time reaction time (hours). Reaction time should be between 2 and 168 hours
#' @examples
#'
#' @export
#'
chemdose_dbp <- function(water, cl2, br, time) {
  toc = water@toc
  uv254 = water@uv254
  temp = water@temp
  ph = water@ph
  
  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (is.na(toc)) {
    stop("Missing value for toc. TOC is required for calculating DBP formation.")
  }
  if (toc < 1.2 | toc > 10.6) {
    warning("TOC is outside the model bounds of 1.2 <= toc <= 10.6 mg/L as set in the WTP model.")
  }
  
  if (is.na(uv254)) {
    stop("Missing value for uv254. UV254 is required for calculating DBP formation.")
  }
  if (uv254 < 0.01 | uv254 > 0.318) {
    warning("UV254 is outside the model bounds of 0.01 <= uv254 <= 0.318 cm-1 as set in the WTP model.")
  }
  
  if (missing(cl2)) {
    stop("Missing value for cl2. Chlorine must be dosed to calculate DBP formation.")
  }
  if (cl2 < 1.51 | cl2 > 33.55) {
    warning("Chlorine is outside the model bounds of 1.51 <= cl2 <= 33.55 mg/L as set in the WTP model.")
  }
  
  if (missing(br)) {
    stop("Missing value for br. Bromide must be dosed to calculate DBP formation.")
  }
  if (br < 7 | br > 600) {
    warning("Bromide is outside the model bounds of 7 <= cl2 <= 600 ug/L as set in the WTP model.")
  }
  
  if (missing(time)) {
    stop("Missing value for time. Please add a reaction time to form DBPs.")
  }
  if (time < 2 | time > 168) {
    warning("Reaction time is outside the model bounds of 2 <= time <= 168 hours as set in the WTP model.")
  }
  
  tthm = 4.21e-2 * toc^1.098 * cl2^0.152 * br^0.068 * temp^0.609 * ph^1.601 * time^0.263
  return(tthm)
}

test <- define_water(8, 25, 66, toc = 4, uv254 = .2) %>%
  chemdose_dbp(cl2=1, br = 8, time = 5)

