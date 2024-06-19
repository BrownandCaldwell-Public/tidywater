# DBP Modeling functions
# These functions predict total trihalomethane (TTHM) and haloacetic acid (HAA) formation

#' Calculate DBP formation
#'
#' @description \code{chemdose_dbp} calculates disinfection byproduct (DBP) formation based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001). Required arguments include an object of class "water"
#' created by \code{\link{define_water}} chlorine dose, type, reaction time, and treatment applied (if any).
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including bromide, TOC, UV254, temperature, and pH.
#' The function will calculate haloacetic acids (HAA) as HAA5, and total trihalomethanes (TTHM).
#' The function returns a new object of class "water" with predicted DBP concentrations.
#' Use \code{summarise_dbp} to quickly tabulate the results.
#'
#' @source TTHMs, raw: U.S. EPA (2001) equation 5-131
#' @source HAAs, raw: U.S. EPA (2001) equation 5-134
#' @source TTHMs, treated: U.S. EPA (2001) equation 5-139
#' @source HAAs, treated: U.S. EPA (2001) equation 5-142
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2 Applied chlorine dose (mg/L as Cl2). Model results are valid for doses between 1.51 and 33.55 mg/L.
#' @param time Reaction time (hours). Model results are valid for reaction times between 2 and 168 hours.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default), "coag" for
#' water that has been coagulated or softened, and "gac" for water that has been treated by granular activated carbon (GAC).
#' GAC treatment has also been used for estimating formation after membrane treatment with good results.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @param location Location for DBP formation, either in the "plant" (default), or in the distributions system, "ds".
#' @examples
#' example_dbp <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chemdose_dbp(cl2 = 2, time = 8)
#' example_dbp <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chemdose_dbp(cl2 = 3, time = 168, treatment = "coag", location = "ds")
#'
#' @export
#'
chemdose_dbp <- function(water, cl2, time, treatment = "raw", cl_type = "chorine", location = "plant") {
  toc = water@toc
  doc = water@doc
  uv254 = water@uv254
  temp = water@temp
  ph = water@ph
  br = water@br

  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (treatment == "raw" & (is.na(toc) | is.na(temp) | is.na(ph) | is.na(br))) {
    stop("Missing value for toc, temp, ph, or br. Please add missing parameters to define_water.")
  }

  if ((treatment == "coag" | treatment == "gac") & (is.na(doc) | is.na(uv254) | is.na(temp) | is.na(ph) | is.na(br))) {
    stop("Missing value for doc, uv254, temp, ph, or br. Please add missing parameters to define_water.")
  }

  if (missing(cl2) | missing(time)) {
    stop("Missing value for cl2 or time. Please check the function inputs required to calculate DBP formation.")
  }

  # toc/doc warnings
  if (treatment == "raw" & (toc < 1.2 | toc > 10.6)) {
    warning("TOC is outside the model bounds of 1.2 <= TOC <= 10.6 mg/L.")
  }

  if (treatment == "coag" & (doc < 1.00 | doc > 7.77)) {
    warning("DOC is outside the model bounds of 1.00 <= doc <= 7.77 mg/L for coagulated water.")
  }

  if (treatment == "gac" & (doc < 0.14 | doc > 2.0)) {
    warning("DOC is outside the model bounds of 0.14 <= DOC <= 2.0 mg/L for GAC treated water.")
  }

  # uv254 warnings
  if (treatment == "coag" & (uv254 < 0.016 | uv254 > 0.215)) {
    warning("UV254 is outside the model bounds of 0.016 <= UV254 <= 0.215 cm-1 for coagulated water.")
  }

  if (treatment == "gac" & (uv254 < 0.001 | uv254 > 0.048)) {
    warning("UV254 is outside the model bounds of 0.001 <= UV254 <= 0.048 cm-1 for GAC treated water.")
  }

  # cl2 warnings
  if (treatment == "raw" & (cl2 < 1.51 | cl2 > 33.55)) {
    warning("Chlorine is outside the model bounds of 1.51 <= Cl2 <= 33.55 mg/L for raw water.")
  }

  if (treatment == "coag" & (cl2 < 1.11 | cl2 > 24.75)) {
    warning("Chlorine is outside the model bounds of 1.11 <= Cl2 <= 24.75 mg/L for coagulated water.")
  }

  if (treatment == "gac" & (cl2 < 0.5 | cl2 > 3.0)) {
    warning("Chlorine is outside the model bounds of 0.5 <= Cl2 <= 3.0 mg/L for GAC treated water.")
  }

  # br warnings
  if (treatment == "raw" & (br < 7 | br > 600)) {
    warning("Bromide is outside the model bounds of 7 <= Br <= 600 ug/L for raw water.")
  }

  if (treatment == "coag" & (br < 23 | br > 308)) {
    warning("Bromide is outside the model bounds of 23 <= Br <= 308 ug/L for coagulated water.")
  }

  if (treatment == "gac" & (br < 10 | br > 570)) {
    warning("Bromide is outside the model bounds of 10 <= Br <= 570 ug/L for GAC treated water.")
  }

  # temp warnings
  if (treatment == "raw" & (temp < 15 | temp > 25)) {
    warning("Temperature is outside the model bounds of 15 <= temp <= 25 Celsius for raw water.")
  }

  if (treatment == "coag" & temp != 20) {
    warning("Temperature is outside the model bounds of temp=20 Celsius for coagulated water.")
  }

  if (treatment == "gac" & (temp < 3 | temp > 33)) {
    warning("Temperature is outside the model bounds of 3 <= temp <= 33 Celsius for GAC treated water.")
  }

  # ph warnings
  if (treatment == "raw" & (ph < 6.5 | ph > 8.5)) {
    warning("pH is outside the model bounds of 6.5 <= pH <= 8.5 for raw water.")
  }

  if (treatment == "coag" & ph != 7.5) {
    warning("pH is outside the model bounds of pH = 7.5 for coagulated water")
  }

  if (treatment == "gac" & (ph < 6.7 | ph > 10)) {
    warning("pH is outside the model bounds of 6.7 <= pH <= 10 for GAC treated water.")
  }

  # time warning
  if (time < 2 | time > 168) {
    warning("Reaction time is outside the model bounds of 2 <= time <= 168 hours.")
  }

  # estimate formation based on level of treatment - results in ug/L
  if (treatment == "raw") {
    predicted_dbp <- dbpcoeffs %>%
      filter(treatment == "raw") %>%
      mutate(modeled_dbp = A * toc^a * cl2^b * br^c * temp^d * ph^e * time^f)
  } else if (treatment == "coag") {
    predicted_dbp <- dbpcoeffs %>%
      filter(treatment == "coag") %>%
      mutate(modeled_dbp = A * (doc * uv254)^a * cl2^b * br^c * d^(ph - ph_const) * e^(temp - 20) * time^f)
  } else if (treatment == "gac") {
    predicted_dbp <- dbpcoeffs %>%
      filter(treatment == "gac") %>%
      mutate(modeled_dbp = A * (doc * uv254)^a * cl2^b * br^c * d^(ph - ph_const) * e^(temp - 20) * time^f)
  }

  # apply dbp correction factors based on selected location for "raw" and "coag" treatment (corrections do not apply to "gac" treatment), U.S. EPA (2001) Table 5-7
  if (location == "plant" & treatment != "gac") {
    corrected_dbp_1 <- predicted_dbp %>%
      left_join(dbp_correction, by = "ID") %>%
      mutate(modeled_dbp = modeled_dbp / plant) %>%
      select(ID, group, modeled_dbp)
  } else if (location == "ds" & treatment != "gac") {
    corrected_dbp_1 <- predicted_dbp %>%
      left_join(dbp_correction, by = "ID") %>%
      mutate(modeled_dbp = modeled_dbp / ds) %>%
      select(ID, group, modeled_dbp)
  } else {
    corrected_dbp_1 <- predicted_dbp %>%
      select(ID, group, modeled_dbp)
  }

  # proportional corrections following U.S. EPA (2001), section 5.7.3
  bulk_dbp <- corrected_dbp_1 %>%
    filter(ID %in% c("tthm", "haa5")) # only model tthm and haa5, problems with haa6 and haa9 model outputs being <haa5 with low Br or Cl2

  individual_dbp <- corrected_dbp_1 %>%
    filter(!(ID %in% c("tthm", "haa5")),
      !(group %in% c("haa6", "haa9"))) %>%
    group_by(group) %>%
    mutate(sum_group = sum(modeled_dbp),
      proportion_group = modeled_dbp / sum_group) %>%
    left_join(bulk_dbp, by = "group", suffix = c("_ind", "_bulk")) %>%
    mutate(modeled_dbp = proportion_group * modeled_dbp_bulk)

  corrected_dbp_2 <- individual_dbp %>%
    select(ID_ind, group, modeled_dbp) %>%
    rename(ID = ID_ind) %>%
    rbind(bulk_dbp)

  # estimate reduced formation if using chloramines, U.S. EPA (2001) Table 5-10
  if (cl_type == "chloramine") {
    corrected_dbp_2 <- corrected_dbp_2 %>%
      left_join(chloramine_conv, by = "ID") %>%
      mutate(modeled_dbp = modeled_dbp * percent)
  }

  water@tthm = corrected_dbp_2 %>% filter(ID == "tthm") %>% {.$modeled_dbp}
  water@chcl3 = corrected_dbp_2 %>% filter(ID == "chcl3") %>% {.$modeled_dbp}
  water@chcl2br = corrected_dbp_2 %>% filter(ID == "chcl2br") %>% {.$modeled_dbp}
  water@chbr2cl = corrected_dbp_2 %>% filter(ID == "chbr2cl") %>% {.$modeled_dbp}
  water@chbr3 = corrected_dbp_2 %>% filter(ID == "chbr3") %>% {.$modeled_dbp}
  water@haa5 = corrected_dbp_2 %>% filter(ID == "haa5") %>% {.$modeled_dbp}
  water@mcaa = corrected_dbp_2 %>% filter(ID == "mcaa") %>% {.$modeled_dbp}
  water@dcaa = corrected_dbp_2 %>% filter(ID == "dcaa") %>% {.$modeled_dbp}
  water@tcaa = corrected_dbp_2 %>% filter(ID == "tcaa") %>% {.$modeled_dbp}
  water@mbaa = corrected_dbp_2 %>% filter(ID == "mbaa") %>% {.$modeled_dbp}
  water@dbaa = corrected_dbp_2 %>% filter(ID == "dbaa") %>% {.$modeled_dbp}

  return(water)
}
