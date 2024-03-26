# DBP Modeling functions
# These functions help predict total trihalomethane (TTHM) and haloacetic acid (HAA) formation


#' Calculate DBP formation using dosed chlorine, ambient bromide, and reaction time.
#'
#' \code{chemdose_dbp} calculates disinfection byproduct (DBP) formation based on
#' chlorine addition, bromide, TOC, UV254, bromide, temperature, pH, and reaction time. This function will 
#' calculate haloacetic acids (HAA) as HAA5 or HAA6, as well as total trihalomethanes (TTHM).
#' The function takes an object of class "water" created by \code{\link{define_water}} and user-specified
#' chlorine addition and returns a dataframe of predicted DBP formation.
#' 
#' TTHMs, untreated: Amy et al. (1998), WTP Model v. 2.0, equation 5-131
#' HAAs, untreated: Amy et al. (1998), WTP Model v. 2.0, equation 5-134 
#' 
#' TTHMs, treated: Amy et al. (1998), WTP Model v. 2.0, equation 5-139
#' HAAs, treated: Amy et al. (1998), WTP Model v. 2.0, equation 5-142 
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2 applied chlorine dose (mg/L as Cl2). Dose should be between 1.51 and 33.55 mg/L
#' @param br bromide concentration (ug/L). Concentration should be between 7 and 600 ug/L
#' @param time reaction time (hours). Reaction time should be between 2 and 168 hours
#' @param water_type type of treatment applied to the water. Default type is "treated", but
#' user may also specify "untreated". Untreated water is generally raw water. "treated water applies to 
#' water that has been coagulated or softened.
#' @param species the dbp species or group of species that should be modeled. Default species is "tthm", but
#' user may also specify haa5, haa6, haa9, or any of the species within these groups. Note: haa9 and its subspecies
#' are only available when water_type = "treated".
#' @examples
#'
#' @export
#'
chemdose_dbp <- function(water, cl2, br, time, water_type = "treated", species = "tthm") {
  toc = water@toc
  uv254 = water@uv254
  temp = water@temp
  ph = water@ph
  
  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (is.na(toc) | is.na(uv254) | is.na(temp) | is.na(ph)) {
    stop("Missing value for toc, uv254, temp, or ph. Please add them to define_water.")
  }
  if (toc < 1.2 | toc > 10.6) {
    warning("TOC is outside the model bounds of 1.2 <= toc <= 10.6 mg/L as set in the WTP model.")
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
  
  if (temp < 15 | temp > 25) {
    warning("Temperature is outside the model bounds of 15 <= temp <= 25 Celsius as set in the WTP model.")
  }
  
  if (ph < 6.5 | ph > 8.5) {
    warning("pH is outside the model bounds of 6.5 <= ph <= 8.5 as set in the WTP model.")
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

