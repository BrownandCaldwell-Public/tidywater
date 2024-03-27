
#' Simulate contributions of various lead solids to total soluble lead
#'
#' This function takes a water data frame defined by \code{\link{define_water}}
#' and outputs a dataframe. Lead solid solubility is calculated based on controlling solid (Pb_2_plus, M).
#' Total dissolved lead species (tot_dissolved_pbm, M) are calculated based on lead complex calculations.
#'
#' The solid with lowest solubility will form the lead scale (controlling lead solid)
#'
#' Make sure that total dissolved solids, conductivity, or
#' ca, na, cl, so4 are used in define_water so that an ionic strength is calculated.
#'
#' Code is from EPA's TELSS lead solubility dashboard https://github.com/USEPA/TELSS/blob/main/app.R
#' which is licensed under MIT License:
#' Permission is hereby granted, free of charge, to any person obtaining a copy of this software and 
#' associated documentation files (the "Software"), to deal in the Software without restriction, 
#' including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
#' copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the 
#' following conditions: The above copyright notice and this permission notice shall be included in all copies or 
#' substantial portions of the Software.
#' 
#' Wahman, D. G., Pinelli, M. D., Schock, M. R., & Lytle, D. A. (2021). 
#' Theoretical equilibrium lead(II) solubility revisited: Open source code and practical relationships.
#' AWWA Water Science, e1250. https://doi.org/10.1002/aws2.1250
#'
#' @param water a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#'example_pb <- define_water(ph = 7.5, temp = 25, alk = 93, cl = 240, tot_po4 = 0, so4 = 150, tds = 200) %>%
#'  dissolve_pb()
#'
#' @export

dissolve_pb <- function(water) {


  # for unit testing/code review
  # water <- define_water(ph = 7, alk = 100, temp = 25, cl = 100, tot_po4 = 2, so4 = 100, tot_hard = 50)

  leadsol_K <- leadsol_constants %>%
    mutate(K_num = 10^log_value)

  h <- 10^-water@ph

  # Activity calculations
  gamma_1 <- calculate_activity(1, water@is, water@temp)
  gamma_2 <- calculate_activity(2, water@is, water@temp)
  gamma_3 <- calculate_activity(3, water@is, water@temp)
  gamma_4 <- calculate_activity(4, water@is, water@temp)

  # Correction of carbonate, phosphate, and sulfate equilibrium constants
  k <- correct_k(water)

  # Calculations for sulfate acid-base species
  alpha_0_s <- 1 / (1 + k$kso4 / h)
  alpha_1_s <- 1 / (h / k$kso4 + 1)

  # Calculate sulfate species concentrations
  hso4 <- alpha_0_s * water@so4
  so4 <- alpha_1_s * water@so4

  # * Calculate lead solid solubility based on controlling solid ----

  solids <- leadsol_K %>%
    filter(grepl("solid", constant_name)) %>%
    # Lead Hydroxide: Pb(OH)2(s) + 2H+ --> Pb2+ + 2H2O
    mutate(Pb_2_plus = case_when(constant_name == "K_solid_lead_hydroxide" ~ K_num * h^2 / gamma_2,
                                 # Cerussite: PbCO3(s) --> Pb2+ + CO32-
                                 constant_name == "K_solid_cerussite" ~ K_num / (gamma_2^2 * water@co3),
                                 # Hydrocerussite: Pb3(CO3)2(OH)2(s) + 2H+ --> 3Pb2+ + 2CO32- + 2H2O
                                 constant_name == "K_solid_hydrocerussite" ~ (K_num * h^2 / (gamma_2^5 * water@co3^2))^(1/3),
                                 # Hydroxypyromorphite: Pb5(PO4)3OH(s) + H+ --> 5Pb2+ + 3PO43- + H2O
                                 constant_name == "K_solid_hydroxypyromorphite_s" ~ (K_num * h / (gamma_2^5 * gamma_3^3 * water@po4^3))^(1/5),
                                 constant_name == "K_solid_hydroxypyromorphite_z" ~ (K_num * h / (gamma_2^5 * gamma_3^3 * water@po4^3))^(1/5),
                                 # Pyromorphite: Pb5(PO4)3Cl(s) --> 5Pb2+ + 3PO43- + Cl-
                                 constant_name == "K_solid_pyromorphite_x" ~ (K_num / (gamma_1 * gamma_2^5 * gamma_3^3 * water@po4^3 * water@cl))^(1/5),
                                 constant_name == "K_solid_pyromorphite_t" ~ (K_num / (gamma_1 * gamma_2^5 * gamma_3^3 * water@po4^3 * water@cl))^(1/5),
                                 # Primary Lead Orthophosphate: Pb(H2PO4)2(s) --> Pb2+ + 2PO43- + 4H+
                                 constant_name == "K_solid_primary_lead_ortho" ~ K_num / (gamma_2 * gamma_3^2 * water@po4^2 * h^4),
                                 # Secondary Lead Orthophosphate: PbHPO4(s) --> Pb2+ + PO43- + H+
                                 constant_name == "K_solid_secondary_lead_ortho" ~ K_num / (gamma_2 * gamma_3 * water@po4 * h),
                                 # Tertiary Lead Orthophosphate: Pb3(PO4)2(s) --> 3Pb2+ + 2PO43- + H+
                                 constant_name == "K_solid_tertiary_lead_ortho" ~ (K_num / (gamma_2^3 * gamma_3^2 * water@po4^2))^(1/3),
                                 # Anglesite: PbSO4(s) --> Pb2+ + SO42-
                                 constant_name == "K_solid_anglesite" ~ K_num / (gamma_2^2 * so4),
                                 # Laurionite: PbClOH(s) + H+ --> Pb2+ + Cl- + H2O
                                 constant_name == "K_solid_laurionite_nl" ~ K_num * h / (gamma_2 * gamma_1 * water@cl),
                                 constant_name == "K_solid_laurionite_l" ~ K_num * h / (gamma_2 * gamma_1 * water@cl)
    ))

  # * Calculation of complex concentrations ----
  complexes <- leadsol_K %>%
    filter(!grepl("solid", constant_name)) %>%
    select(-log_value, -species_name, -source) %>%
    pivot_wider(names_from = constant_name, values_from = K_num)

  alllead <- solids %>%
    cross_join(complexes) %>%
    mutate(# Calculate lead-hydroxide complex concentrations
      PbOH_plus = (B_1_OH) * gamma_2 * Pb_2_plus / (gamma_1 * h),
      PbOH2 = (B_2_OH) * gamma_2 * Pb_2_plus / h^2,
      PbOH3_minus = (B_3_OH) * gamma_2 * Pb_2_plus / (gamma_1 * h^3),
      PbOH4_2_minus = (B_4_OH) * Pb_2_plus / h^4,
      Pb2OH_3_plus = (B_2_1_OH) * gamma_2^2 * Pb_2_plus^2 / (gamma_3 * h),
      Pb3OH4_2_plus = (B_3_4_OH) * gamma_2^2 * Pb_2_plus^3 / h^4,
      Pb4OH4_4_plus = (B_4_4_OH) * gamma_2^4 * Pb_2_plus^4 / (gamma_4 * h^4),
      Pb6OH8_4_plus = (B_6_8_OH) * gamma_2^6 * Pb_2_plus^6 / (gamma_4 * h^8),
      # Calculate lead-chloride complex concentrations
      PbCl_plus = (K_1_Cl) * gamma_2 * Pb_2_plus * water@cl,
      PbCl2 = (B_2_Cl) * gamma_2 * Pb_2_plus * gamma_1^2 * water@cl^2,
      PbCl3_minus = (B_3_Cl) * gamma_2 * Pb_2_plus * gamma_1^2 * water@cl^3,
      PbCl4_2_minus = (B_4_Cl) * Pb_2_plus * gamma_1^4 * water@cl^4,
      # Calculate lead-sulfate complex concentrations
      PbSO4 = (K_1_SO4) * gamma_2^2 * Pb_2_plus * so4,
      PbSO42_2_minus = (B_2_SO4) * gamma_2^2 * Pb_2_plus * so4^2,
      # Calculate lead-carbonate complex concentrations
      PbHCO3_plus = ((K_1_CO3) * h * gamma_2^2 * Pb_2_plus * water@co3) / gamma_1,
      PbCO3 = (K_2_CO3) * gamma_2^2 * Pb_2_plus * water@co3,
      PbCO32_2_minus = (K_3_CO3) * gamma_2^2 * Pb_2_plus * water@co3^2,
      # Calculate lead-phosphate complex concentrations
      PbHPO4 = (K_1_PO4) * h * gamma_2 * gamma_3 * Pb_2_plus * water@po4,
      PbH2PO4_plus = (K_2_PO4) * h^2 * gamma_2 * gamma_3 * Pb_2_plus * water@po4 / gamma_1) %>%
    mutate(# Calculate total dissolved lead molar concentration
      tot_dissolved_pb = Pb_2_plus +
        PbOH_plus + PbOH2 + PbOH3_minus + PbOH4_2_minus + 2 * Pb2OH_3_plus + 3 * Pb3OH4_2_plus + 4 * Pb4OH4_4_plus + 6 * Pb6OH8_4_plus +
        PbCl_plus + PbCl2 + PbCl3_minus + PbCl4_2_minus +
        PbSO4 + PbSO42_2_minus +
        PbHCO3_plus + PbCO3 + PbCO32_2_minus +
        PbHPO4 + PbH2PO4_plus)

  alllead_simple <- alllead %>%
    select(species_name, Pb_2_plus, tot_dissolved_pb, source) %>%
    mutate(ph = water@ph,
           # dic = dic,
           tot_po4 = water@tot_po4,
           so4 = water@so4,
           is = water@is,
           cl = water@cl
           #output other things like hco3, co3 etc too?
    )

  return(alllead_simple)

}


#' Calculate dissolved inorganic carbon (DIC) from total carbonate
#'
#' This function takes a water class object defined by \code{\link{define_water}}
#' and outputs a DIC (mg/L).
#'
#' @param water a water class object containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#'example_dic <- define_water(8, 15, 200) %>%
#'  calculate_dic()
#'
#' @export
#'

calculate_dic <- function(water) {

  dic <- water@tot_co3 * mweights$dic * 1000
  
  return(dic)
}
