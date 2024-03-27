# DBP Modeling functions
# These functions predict total trihalomethane (TTHM) and haloacetic acid (HAA) formation

#' Calculate DBP formation using dosed chlorine, ambient bromide, and reaction time.
#'
#' \code{chemdose_dbp} calculates disinfection byproduct (DBP) formation based on the WTP model. Parameters required
#' include chlorine addition, bromide, TOC, UV254, temperature, pH, and reaction time. This function will 
#' calculate haloacetic acids (HAA) as HAA5, HAA6, or HAA9, as well as total trihalomethanes (TTHM).
#' The function takes an object of class "water" created by \code{\link{define_water}} and user-specified
#' chlorine addition, bromide, and reaction time and returns a dataframe of predicted DBP formation.
#' 
#' TTHMs, untreated: Amy et al. (1998), WTP Model v. 2.0, equation 5-131
#' HAAs, untreated: Amy et al. (1998), WTP Model v. 2.0, equation 5-134 
#' 
#' TTHMs, treated: Amy et al. (1998), WTP Model v. 2.0, equation 5-139
#' HAAs, treated: Amy et al. (1998), WTP Model v. 2.0, equation 5-142 
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2 applied chlorine dose (mg/L as Cl2). Dose should be between 1.51 and 33.55 mg/L
#' @param br bromide (br-) concentration (ug/L). Concentration should be between 7 and 600 ug/L
#' @param time reaction time (hours). Reaction time should be between 2 and 168 hours
#' @param water_type type of treatment applied to the water. Default type is "treated", but
#' user may also specify "untreated". Untreated water is generally raw water. "treated water applies to 
#' water that has been coagulated or softened.
#' @param species the dbp species or group of species that should be modeled. Default species is "tthm", but
#' user may also specify haa5, haa6, haa9, or any of the species within these groups. Note: haa9 and its subspecies
#' are only available when water_type = "treated". For a list of all species names, see dbpcoeffs dataframe. Multiple
#' species may be selected using c().
#' @examples
#' example_dbp <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2)) %>%
#' chemdose_dbp(cl2 = 2, br = 30, time = 8)
#'example_dbp <- suppressWarnings(define_water(8, 25, 66, toc = 4, uv254 = .2)) %>%
#'chemdose_dbp(cl2 = 2, br = 30, time = 8, water_type = "untreated", species = c("tthm", "chcl3", "chcl2br", "chbr2cl", "chbr3"))
#'
#' @export
#'
chemdose_dbp <- function(water, cl2, br, time, water_type = "treated", species = "tthm") {
  toc = water@toc
  doc = water@doc
  uv254 = water@uv254
  temp = water@temp
  ph = water@ph
  
  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (is.na(toc) | is.na(uv254) | is.na(temp) | is.na(ph)) {
    stop("Missing value for toc, uv254, temp, or ph. Please add them to define_water.")
  }
  if (is.na(doc) & water_type == "treated") {
    stop("Missing value for doc. Please add doc to define_water.")
  }
  if (missing(cl2) | missing(br) | missing(time)) {
    stop("Missing value for cl2, br, or time. Please check the function inputs required to calculate DBP formation.")
  }
  
  if (water_type == "untreated" & (toc < 1.2 | toc > 10.6)) {
    warning("TOC is outside the model bounds of 1.2 <= toc <= 10.6 mg/L as set in the WTP model.")
  }
  if (water_type == "treated" & (doc < 1.00 | doc > 7.77)) {
    warning("DOC is outside the treated water model bounds of 1.00 <= doc <= 7.77 mg/L as set in the WTP model.")
  }
  
  if (water_type == "untreated" & (uv254 < 0.01 | uv254 > 0.318)) {
    warning("UV254 is outside the untreated water model bounds of 0.01 <= uv254 <= 0.318 cm-1 as set in the WTP model.")
  }
  if (water_type == "treated" & (uv254 < 0.016 | uv254 > 0.215)) {
    warning("UV254 is outside the treated water model bounds of 0.016 <= uv254 <= 0.215 cm-1 as set in the WTP model.")
  }

  if (water_type == "untreated" & (cl2 < 1.51 | cl2 > 33.55)) {
    warning("Chlorine is outside the untreated water model bounds of 1.51 <= cl2 <= 33.55 mg/L as set in the WTP model.")
  }
  if (water_type == "treated" & (cl2 < 1.11 | cl2 > 24.75)) {
    warning("Chlorine is outside the treated water model bounds of 1.11 <= cl2 <= 24.75 mg/L as set in the WTP model.")
  }

  if (water_type == "untreated" & (br < 7 | br > 600)) {
    warning("Bromide is outside the untreated water model bounds of 7 <= cl2 <= 600 ug/L as set in the WTP model.")
  }
  if (water_type == "treated" & (br < 23 | br > 308)) {
    warning("Bromide is outside the treated water model bounds of 23 <= cl2 <= 308 ug/L as set in the WTP model.")
  }
  
  if (water_type == "untreated" & (temp < 15 | temp > 25)) {
    warning("Temperature is outside the untreated water model bounds of 15 <= temp <= 25 Celsius as set in the WTP model.")
  }
  if (water_type == "treated" & temp != 20 ) {
    warning("Temperature is not set to 20 Celsius as set in the WTP model for treated water modeling.")
  }
  
  if (water_type == "untreated" & (ph < 6.5 | ph > 8.5)) {
    warning("pH is outside the untreated water model bounds of 6.5 <= ph <= 8.5 as set in the WTP model.")
  }
  if (water_type == "treated" & ph != 7.5) {
    warning("pH is not set to 7.5 as set in the WTP model for treated water modeling.")
  }

  if (time < 2 | time > 168) {
    warning("Reaction time is outside the model bounds of 2 <= time <= 168 hours as set in the WTP model.")
  }
  
  if (water_type == "untreated") {

    predicted_dbp <- dbpcoeffs %>%
      filter(water_type == "untreated") %>%
      filter(ID %in% species) %>%
      mutate(modeled_dbp_ug.L = A * toc^a * cl2^b * br^c * temp^d * ph^e * time^f)%>%
      select(-c(A:f))
  } 
  
  if (water_type == "treated") {
    
    predicted_dbp <- dbpcoeffs %>%
      filter(water_type == "treated") %>%
      filter(ID %in% species) %>%
      mutate(modeled_dbp_ug.L = A * (doc*uv254)^a * cl2^b * br^c * d^(temp-20) * e^(ph-7.5) * time^f) %>%
      select(-c(A:f))
  }
  

  return(predicted_dbp)
}
