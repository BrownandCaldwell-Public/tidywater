# DBP Modeling functions
# These functions predict total trihalomethane (TTHM) and haloacetic acid (HAA) formation

#' Calculate DBP formation
#'
#' \code{chemdose_dbp} calculates disinfection byproduct (DBP) formation based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001). Required arguments include an object of class "water"
#' created by \code{\link{define_water}} chlorine dose, type, reaction time, and treatment applied (if any).
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including bromide, TOC, UV254, temperature, and pH.
#' The function will calculate haloacetic acids (HAA) as HAA5, HAA6, or HAA9, as well as
#' total trihalomethanes (TTHM).
#' The function returns a new object of class "water" with predicted DBP concentrations.
#' Use \code{summarise_dbp} to quickly tabulate the results.
#'
#' TTHMs, untreated: Amy et al. (1998), WTP Model v. 2.0, equation 5-131
#' HAAs, untreated: Amy et al. (1998), WTP Model v. 2.0, equation 5-134
#'
#' TTHMs, treated: Amy et al. (1998), WTP Model v. 2.0, equation 5-139
#' HAAs, treated: Amy et al. (1998), WTP Model v. 2.0, equation 5-142
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2 Applied chlorine dose (mg/L as Cl2). Model results are valid for doses between 1.51 and 33.55 mg/L.
#' @param time Reaction time (hours). Model results are valid for reaction times between 2 and 168 hours.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default), "coag" for
#' water that has been coagulated or softened, and "gac" for water that has been treated by granular activated carbon.
#' GAC treatment has also been used for estimating formation after membrane treatment with good results.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @examples
#' example_dbp <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#' chemdose_dbp(cl2 = 2, time = 8)
#' example_dbp <- suppressWarnings(define_water(8, 25, 66, toc = 4, uv254 = .2, br = 50)) %>%
#' chemdose_dbp(cl2 = 2, time = 8, water_type = "untreated")
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
  if (is.na(toc) | is.na(uv254) | is.na(temp) | is.na(ph) | is.na(br)) {
    stop("Missing value for toc, uv254, temp, ph, or br. Please add them to define_water.")
  }
  if (is.na(doc) & (treatment == "coag" | treatment == "gac")){
    stop("Missing value for doc. Please add doc to define_water.")
  }
  if (missing(cl2) | missing(time)) {
    stop("Missing value for cl2 or time. Please check the function inputs required to calculate DBP formation.")
  }

  if (treatment == "raw" & (toc < 1.2 | toc > 10.6)) {
    warning("TOC is outside the model bounds of 1.2 <= toc <= 10.6 mg/L as set in the WTP model.")
  }

  if (treatment == "raw" & (doc < 1.00 | doc > 7.77)) {
    warning("DOC is outside the treated water model bounds of 1.00 <= doc <= 7.77 mg/L as set in the WTP model.")
  }

  if (treatment == "raw" & (uv254 < 0.01 | uv254 > 0.318)) {
    warning("UV254 is outside the untreated water model bounds of 0.01 <= uv254 <= 0.318 cm-1 as set in the WTP model.")
  }

  if (treatment == "coag" & (uv254 < 0.016 | uv254 > 0.215)) {
    warning("UV254 is outside the treated water model bounds of 0.016 <= uv254 <= 0.215 cm-1 as set in the WTP model.")
  }

  if (treatment == "raw" & (cl2 < 1.51 | cl2 > 33.55)) {
    warning("Chlorine is outside the untreated water model bounds of 1.51 <= cl2 <= 33.55 mg/L as set in the WTP model.")
  }
  if (treatment == "coag" & (cl2 < 1.11 | cl2 > 24.75)) {
    warning("Chlorine is outside the treated water model bounds of 1.11 <= cl2 <= 24.75 mg/L as set in the WTP model.")
  }

  if (treatment == "raw" & (br < 7 | br > 600)) {
    warning("Bromide is outside the untreated water model bounds of 7 <= br <= 600 ug/L as set in the WTP model.")
  }
  if (treatment == "coag" & (br < 23 | br > 308)) {
    warning("Bromide is outside the treated water model bounds of 23 <= br <= 308 ug/L as set in the WTP model.")
  }

  if (treatment == "raw" & (temp < 15 | temp > 25)) {
    warning("Temperature is outside the untreated water model bounds of 15 <= temp <= 25 Celsius as set in the WTP model.")
  }

  if (treatment == "coag" & temp != 20 ) {
    warning("Temperature is not set to 20 Celsius as set in the WTP model for treated water modeling.")
  }

  if (treatment == "raw" & (ph < 6.5 | ph > 8.5)) {
    warning("pH is outside the untreated water model bounds of 6.5 <= ph <= 8.5 as set in the WTP model.")
  }

  if (treatment == "coag" & ph != 7.5) {
    warning("pH is not set to 7.5 as set in the WTP model for treated water modeling.")
  }

  if (time < 2 | time > 168) {
    warning("Reaction time is outside the model bounds of 2 <= time <= 168 hours as set in the WTP model.")
  }

  # estimate formation based on level of treatment
  if (treatment == "raw") {
    predicted_dbp <- dbpcoeffs %>%
      filter(treatment == "raw") %>%
      mutate(modeled_dbp_ug.L = A * toc^a * cl2^b * br^c * temp^d * ph^e * time^f)
  } else if (treatment == "coag") {
    predicted_dbp <- dbpcoeffs %>%
      filter(treatment == "coag") %>%
      mutate(modeled_dbp_ug.L = A * (doc*uv254)^a * cl2^b * br^c * d^(ph-ph_const) * e^(temp-20) * time^f)
  } else if (treatment == "gac") {
    predicted_dbp <- dbpcoeffs %>%
      filter(treatment == "gac") %>%
      mutate(modeled_dbp_ug.L = A * (doc*uv254)^a * cl2^b * br^c * d^(ph-ph_const) * e^(temp-20) * time^f)
  }

# apply dbp correction factors for "raw" and "coag" treatment (corrections do not apply to "gac" treatment)
  if (location == "plant" & location!="gac") {
    corrected_dbp <- predicted_dbp%>%
      left_join(dbp_correction, by="ID")%>%
      mutate(modeled_dbp_ug.L = modeled_dbp_ug.L/plant)
  } else if(location == "ds" & location!="gac") {
    corrected_dbp <- predicted_dbp%>%
      left_join(dbp_correction, by="ID")%>%
      mutate(modeled_dbp_ug.L = modeled_dbp_ug.L/ds)
  }

# estimate reduced formation if using chloramines
  if (cl_type == "chloramine") {
    corrected_dbp <- corrected_dbp%>%
      left_join(conv_chloramine, by="ID")%>%
      mutate(modeled_dbp_ug.L = modeled_dbp_ug.L * percent)
  }

# proportional corrections
bulk_dbp <- predicted_dbp%>%
  filter(ID %in% c("tthm", "haa5", "haa6", "haa9"))%>%
  select(ID, group, modeled_dbp_ug.L)%>%
  rename("bulk_c" = modeled_dbp_ug.L)
# problem with haas, haa6 and haa9 are <haa5

individual_dbp <- predicted_dbp%>%
  filter(!(ID %in% c("tthm", "haa5", "haa6", "haa9")))%>%
  group_by(group)%>%
  mutate(sum_group = sum(modeled_dbp_ug.L),
         proportion_group = modeled_dbp_ug.L/sum_group)%>%
  # ungroup()%>%
  # mutate(proportion = case_when(group=="tthm" | group=="haa5" ~ modeled_dbp_ug.L/sum_group,
  #                               group=="haa6" ~ modeled_dbp_ug.L/(sum_group + lag(sum_group, 1)),
  #                               ID=="cdbaa" ~ modeled_dbp_ug.L/(sum_group + lag(sum_group,1) + lag(sum_group, 2)),
  #                               ID=="dcbaa" ~ modeled_dbp_ug.L/(sum_group + lag(sum_group,2) + lag(sum_group, 3)),
  #                               ID=="tbaa" ~ modeled_dbp_ug.L/(sum_group + lag(sum_group,3) + lag(sum_group, 4)),
  #                               TRUE ~ NA))%>%
  left_join(bulk_dbp, by="group")%>%
  mutate(corrected_c = proportion_group*bulk_c)

  #left_join(bulk_dbp, by="type")%>%
  select(ID.x, ID.y, type, modeled_dbp_ug.L, bulk_c)%>%
  mutate(proportion = modeled_dbp_ug.L/bulk_c)


  water@tthm = corrected_dbp%>%filter(ID=="tthm")%>%{.$modeled_dbp_ug.L}
  water@chcl3 = corrected_dbp%>%filter(ID=="chcl3")%>%{.$modeled_dbp_ug.L}
  water@chcl2br = corrected_dbp%>%filter(ID=="chcl2br")%>%{.$modeled_dbp_ug.L}
  water@chbr2cl = corrected_dbp%>%filter(ID=="chbr2cl")%>%{.$modeled_dbp_ug.L}
  water@chbr3 = corrected_dbp%>%filter(ID=="chbr3")%>%{.$modeled_dbp_ug.L}
  water@haa5 = corrected_dbp%>%filter(ID=="haa5")%>%{.$modeled_dbp_ug.L}
  water@haa6 = corrected_dbp%>%filter(ID=="haa6")%>%{.$modeled_dbp_ug.L}
  water@haa9 = corrected_dbp%>%filter(ID=="haa9")%>%{.$modeled_dbp_ug.L}
  water@mcaa = corrected_dbp%>%filter(ID=="mcaa")%>%{.$modeled_dbp_ug.L}
  water@dcaa = corrected_dbp%>%filter(ID=="dcaa")%>%{.$modeled_dbp_ug.L}
  water@tcaa = corrected_dbp%>%filter(ID=="tcaa")%>%{.$modeled_dbp_ug.L}
  water@mbaa = corrected_dbp%>%filter(ID=="mbaa")%>%{.$modeled_dbp_ug.L}
  water@dbaa = corrected_dbp%>%filter(ID=="dbaa")%>%{.$modeled_dbp_ug.L}
  water@bcaa = corrected_dbp%>%filter(ID=="bcaa")%>%{.$modeled_dbp_ug.L}
  water@cdbaa = corrected_dbp%>%filter(ID=="cdbaa")%>%{.$modeled_dbp_ug.L}
  water@dcbaa = corrected_dbp%>%filter(ID=="dcbaa")%>%{.$modeled_dbp_ug.L}
  water@tbaa = corrected_dbp%>%filter(ID=="tbaa")%>%{.$modeled_dbp_ug.L}

  return(water)
}
