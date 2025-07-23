# The original source for much of this came from USEPA's Theoretical Equilibrium Lead Solubility Simulator (TELSS)
# code in https://github.com/USEPA/TELSS/app.R
#
# Below is the original license statement for the TELSS app.
#
# The MIT License (MIT)
# =====================
#
#   Copyright © 2021 USEPA.
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the “Software”), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following
# conditions:
#
#   The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

# Substantial changes were made to adapt the app into a function, but the underlying calculations are generally from the
# simulate_solubility function.

#' Simulate contributions of various lead solids to total soluble lead
#'
#' @description This function takes a water data frame defined by [define_water]
#' and outputs a dataframe of the controlling lead solid and total lead solubility.
#' Lead solid solubility is calculated based on controlling solid.
#' Total dissolved lead species (tot_dissolved_pb, M) are calculated based on lead complex calculations.
#' For a single water, use `dissolve_pb`; to apply the model to a dataframe, use `dissolve_pb_df`.
#' For most arguments, the `_df`
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details The solid with lowest solubility will form the lead scale (controlling lead solid).
#' Some lead solids have two k-constant options. The function will default to the EPA's default constants.
#' The user may change the constants to hydroxypyromorphite = "Zhu" or pyromorphite = "Xie" or laurionite = "Lothenbach"
#'
#' Make sure that total dissolved solids, conductivity, or
#' ca, na, cl, so4 are used in `define_water` so that an ionic strength is calculated.
#'
#' @source Code is from EPA's TELSS lead solubility dashboard \url{https://github.com/USEPA/TELSS}
#' which is licensed under MIT License:
#' Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
#' associated documentation files (the "Software"), to deal in the Software without restriction,
#' including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
#' following conditions: The above copyright notice and this permission notice shall be included in all copies or
#' substantial portions of the Software.
#' @source Wahman et al. (2021)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#'
#' @param water Source water object of class "water" created by [define_water]. Water must include alk and is.
#' If po4, cl, and so4 are known, those should also be included.
#' @param hydroxypyromorphite defaults to "Schock", the constant, K, developed by Schock et al (1996). Can also use "Zhu".
#' @param pyromorphite defaults to "Topolska", the constant, K, developed by Topolska et al (2016). Can also use "Xie".
#' @param laurionite defaults to "Nasanen", the constant, K, developed by Nasanen & Lindell (1976). Can also use "Lothenbach".
#'
#' @examples
#'
#' example_pb <- define_water(
#'   ph = 7.5, temp = 25, alk = 93, cl = 240,
#'   tot_po4 = 0, so4 = 150, tds = 200
#' ) %>%
#'   dissolve_pb()
#' example_pb <- define_water(
#'   ph = 7.5, temp = 25, alk = 93, cl = 240,
#'   tot_po4 = 0, so4 = 150, tds = 200
#' ) %>%
#'   dissolve_pb(pyromorphite = "Xie")
#'
#' @export
#'
#' @returns `dissolve_pb` returns a one row data frame containing only the controlling lead solid and modeled dissolved lead concentration.

dissolve_pb <- function(water, hydroxypyromorphite = "Schock", pyromorphite = "Topolska", laurionite = "Nasanen") {
  constant_name <- log_value <- species_name <- K_num <- NULL # Quiet RCMD check global variable note
  validate_water(water, c("ph", "alk", "is"))

  water@po4 <- ifelse(is.na(water@po4), 0, water@po4)
  water@cl <- ifelse(is.na(water@cl), 0, water@cl)
  water@so4 <- ifelse(is.na(water@so4), 0, water@so4)

  if (!(hydroxypyromorphite == "Schock" | hydroxypyromorphite == "Zhu")) {
    stop("Hydroxypyromorphite equilibrium constant must be 'Schock' or 'Zhu'.")
  }

  if (!(pyromorphite == "Topolska" | pyromorphite == "Xie")) {
    stop("Pyromorphite equilibrium constant must be 'Topolska' or 'Xie'.")
  }

  if (!(laurionite == "Nasanen" | laurionite == "Lothenbach")) {
    stop("Laurionite equilibrium constant must be 'Nasanen' or 'Lothenbach'.")
  }

  leadsol_K <- tidywater::leadsol_constants
  leadsol_K$K_num <- 10^leadsol_K$log_value

  h <- 10^-water@ph

  # Activity calculations
  gamma_1 <- calculate_activity(1, water@is, water@temp)
  gamma_2 <- calculate_activity(2, water@is, water@temp)
  gamma_3 <- calculate_activity(3, water@is, water@temp)
  gamma_4 <- calculate_activity(4, water@is, water@temp)

  # Correction of carbonate, phosphate, and sulfate equilibrium constants
  k <- correct_k(water)

  # * Calculate lead solid solubility based on controlling solid ----
  solids <- subset(leadsol_K, grepl("solid", constant_name))
  solids$Pb_2_plus <- NA
  # Lead Hydroxide: Pb(OH)2(s) + 2H+ --> Pb2+ + 2H2O
  solids["K_solid_lead_hydroxide", "Pb_2_plus"] <- solids["K_solid_lead_hydroxide", "K_num"] * h^2 / gamma_2
  # Cerussite: PbCO3(s) --> Pb2+ + CO32-
  solids["K_solid_cerussite", "Pb_2_plus"] <- solids["K_solid_cerussite", "K_num"] / (gamma_2^2 * water@co3)
  # Hydrocerussite: Pb3(CO3)2(OH)2(s) + 2H+ --> 3Pb2+ + 2CO32- + 2H2O
  solids["K_solid_hydrocerussite", "Pb_2_plus"] <- (solids["K_solid_hydrocerussite", "K_num"] * h^2 / (gamma_2^5 * water@co3^2))^(1 / 3)
  # Hydroxypyromorphite: Pb5(PO4)3OH(s) + H+ --> 5Pb2+ + 3PO43- + H2O
  solids["K_solid_hydroxypyromorphite_s", "Pb_2_plus"] <- (solids["K_solid_hydroxypyromorphite_s", "K_num"] * h / (gamma_2^5 * gamma_3^3 * water@po4^3))^(1 / 5)
  solids["K_solid_hydroxypyromorphite_z", "Pb_2_plus"] <- (solids["K_solid_hydroxypyromorphite_z", "K_num"] * h / (gamma_2^5 * gamma_3^3 * water@po4^3))^(1 / 5)
  # Pyromorphite: Pb5(PO4)3Cl(s) --> 5Pb2+ + 3PO43- + Cl-
  solids["K_solid_pyromorphite_x", "Pb_2_plus"] <- (solids["K_solid_pyromorphite_x", "K_num"] / (gamma_1 * gamma_2^5 * gamma_3^3 * water@po4^3 * water@cl))^(1 / 5)
  solids["K_solid_pyromorphite_t", "Pb_2_plus"] <- (solids["K_solid_pyromorphite_t", "K_num"] / (gamma_1 * gamma_2^5 * gamma_3^3 * water@po4^3 * water@cl))^(1 / 5)
  # Primary Lead Orthophosphate: Pb(H2PO4)2(s) --> Pb2+ + 2PO43- + 4H+
  solids["K_solid_primary_lead_ortho", "Pb_2_plus"] <- solids["K_solid_primary_lead_ortho", "K_num"] / (gamma_2 * gamma_3^2 * water@po4^2 * h^4)
  # Secondary Lead Orthophosphate: PbHPO4(s) --> Pb2+ + PO43- + H+
  solids["K_solid_secondary_lead_ortho", "Pb_2_plus"] <- solids["K_solid_secondary_lead_ortho", "K_num"] / (gamma_2 * gamma_3 * water@po4 * h)
  # Tertiary Lead Orthophosphate: Pb3(PO4)2(s) --> 3Pb2+ + 2PO43- + H+
  solids["K_solid_tertiary_lead_ortho", "Pb_2_plus"] <- (solids["K_solid_tertiary_lead_ortho", "K_num"] / (gamma_2^3 * gamma_3^2 * water@po4^2))^(1 / 3)
  # Anglesite: PbSO4(s) --> Pb2+ + SO42-
  solids["K_solid_anglesite", "Pb_2_plus"] <- solids["K_solid_anglesite", "K_num"] / (gamma_2^2 * water@so4)
  # Laurionite: PbClOH(s) + H+ --> Pb2+ + Cl- + H2O
  solids["K_solid_laurionite_nl", "Pb_2_plus"] <- solids["K_solid_laurionite_nl", "K_num"] * h / (gamma_2 * gamma_1 * water@cl)
  solids["K_solid_laurionite_l", "Pb_2_plus"] <- solids["K_solid_laurionite_l", "K_num"] * h / (gamma_2 * gamma_1 * water@cl)

  # * Calculation of complex concentrations ----
  complexes <- subset(leadsol_K, !grepl("solid", constant_name), select = -c(log_value, species_name, source)) %>%
    tidyr::pivot_wider(names_from = constant_name, values_from = K_num)

  alllead <- solids %>%
    dplyr::cross_join(complexes)
  # Calculate lead-hydroxide complex concentrations
  alllead$PbOH_plus <- (alllead$B_1_OH) * gamma_2 * alllead$Pb_2_plus / (gamma_1 * h)
  alllead$PbOH2 <- (alllead$B_2_OH) * gamma_2 * alllead$Pb_2_plus / h^2
  alllead$PbOH3_minus <- (alllead$B_3_OH) * gamma_2 * alllead$Pb_2_plus / (gamma_1 * h^3)
  alllead$PbOH4_2_minus <- (alllead$B_4_OH) * alllead$Pb_2_plus / h^4
  alllead$Pb2OH_3_plus <- (alllead$B_2_1_OH) * gamma_2^2 * alllead$Pb_2_plus^2 / (gamma_3 * h)
  alllead$Pb3OH4_2_plus <- (alllead$B_3_4_OH) * gamma_2^2 * alllead$Pb_2_plus^3 / h^4
  alllead$Pb4OH4_4_plus <- (alllead$B_4_4_OH) * gamma_2^4 * alllead$Pb_2_plus^4 / (gamma_4 * h^4)
  alllead$Pb6OH8_4_plus <- (alllead$B_6_8_OH) * gamma_2^6 * alllead$Pb_2_plus^6 / (gamma_4 * h^8)
  # Calculate lead-chloride complex concentrations
  alllead$PbCl_plus <- (alllead$K_1_Cl) * gamma_2 * alllead$Pb_2_plus * water@cl
  alllead$PbCl2 <- (alllead$B_2_Cl) * gamma_2 * alllead$Pb_2_plus * gamma_1^2 * water@cl^2
  alllead$PbCl3_minus <- (alllead$B_3_Cl) * gamma_2 * alllead$Pb_2_plus * gamma_1^2 * water@cl^3
  alllead$PbCl4_2_minus <- (alllead$B_4_Cl) * alllead$Pb_2_plus * gamma_1^4 * water@cl^4
  # Calculate lead-sulfate complex concentrations
  alllead$PbSO4 <- (alllead$K_1_SO4) * gamma_2^2 * alllead$Pb_2_plus * water@so4
  alllead$PbSO42_2_minus <- (alllead$B_2_SO4) * gamma_2^2 * alllead$Pb_2_plus * water@so4^2
  # Calculate lead-carbonate complex concentrations
  alllead$PbHCO3_plus <- ((alllead$K_1_CO3) * h * gamma_2^2 * alllead$Pb_2_plus * water@co3) / gamma_1
  alllead$PbCO3 <- (alllead$K_2_CO3) * gamma_2^2 * alllead$Pb_2_plus * water@co3
  alllead$PbCO32_2_minus <- (alllead$K_3_CO3) * gamma_2^2 * alllead$Pb_2_plus * water@co3^2
  # Calculate lead-phosphate complex concentrations
  alllead$PbHPO4 <- (alllead$K_1_PO4) * h * gamma_2 * gamma_3 * alllead$Pb_2_plus * water@po4
  alllead$PbH2PO4_plus <- (alllead$K_2_PO4) * h^2 * gamma_2 * gamma_3 * alllead$Pb_2_plus * water@po4 / gamma_1

  # Calculate total dissolved lead molar concentration
  alllead$tot_dissolved_pb <- alllead$Pb_2_plus +
    alllead$PbOH_plus + alllead$PbOH2 + alllead$PbOH3_minus + alllead$PbOH4_2_minus +
    2 * alllead$Pb2OH_3_plus + 3 * alllead$Pb3OH4_2_plus + 4 * alllead$Pb4OH4_4_plus + 6 * alllead$Pb6OH8_4_plus +

    alllead$PbCl_plus + alllead$PbCl2 + alllead$PbCl3_minus + alllead$PbCl4_2_minus +
    alllead$PbSO4 + alllead$PbSO42_2_minus +
    alllead$PbHCO3_plus + alllead$PbCO3 + alllead$PbCO32_2_minus +
    alllead$PbHPO4 + alllead$PbH2PO4_plus

  alllead_simple <- subset(
    alllead,
    !(alllead$species_name == "Hydroxypyromorphite" & !grepl(hydroxypyromorphite, alllead$source)) &
      !(alllead$species_name == "Pyromorphite" & !grepl(pyromorphite, alllead$source)) &
      !(alllead$species_name == "Laurionite" & !grepl(laurionite, alllead$source)) &
      !is.na(alllead$tot_dissolved_pb)
  )
  controlling_solid <- alllead_simple$species_name[min(alllead_simple$tot_dissolved_pb) == alllead_simple$tot_dissolved_pb]
  tot_dissolved_pb <- min(alllead_simple$tot_dissolved_pb)

  data.frame(controlling_solid, tot_dissolved_pb)
}

#' @rdname dissolve_pb
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_col_solid name of the output column storing the controlling lead solid. Default is "controlling_solid".
#' @param output_col_result name of the output column storing dissolved lead in M. Default is "pb".
#' @param water_prefix name of the input water used for the calculation, appended to the start of output columns. Default is TRUE.
#' Change to FALSE to remove the water prefix from output column names.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dissolve_pb_df(output_col_result = "dissolved_lead", pyromorphite = "Xie")
#'
#' @export
#'
#' @returns `dissolve_pb_df` returns a data frame containing the controlling lead solid and modeled dissolved lead concentration as new columns.

dissolve_pb_df <- function(df, input_water = "defined", output_col_solid = "controlling_solid",
                             output_col_result = "pb", hydroxypyromorphite = "Schock",
                             pyromorphite = "Topolska", laurionite = "Nasanen", water_prefix = TRUE) {
  validate_water_helpers(df, input_water)

  if (!(hydroxypyromorphite == "Schock" | hydroxypyromorphite == "Zhu")) {
    stop("Hydroxypyromorphite equilibrium constant must be 'Schock' or 'Zhu'.")
  }

  if (!(pyromorphite == "Topolska" | pyromorphite == "Xie")) {
    stop("Pyromorphite equilibrium constant must be 'Topolska' or 'Xie'.")
  }

  if (!(laurionite == "Nasanen" | laurionite == "Lothenbach")) {
    stop("Laurionite equilibrium constant must be 'Nasanen' or 'Lothenbach'.")
  }



  pb_df <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    dissolve_pb(
      water = df[[input_water]][[i]]
    )
  }))

  names(pb_df) <- c(output_col_solid, output_col_result)
  if(water_prefix) {
    names(pb_df) <- paste0(input_water, "_", names(pb_df))
  }

  output <- cbind(df, pb_df)
  return(output)

}
