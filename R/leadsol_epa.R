
#' Calc lead sol
#'
#' This function calcs leadsol
#'
#' @param water a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' example_df <- water_df %>% define_water_once()
#'
#' @export
#' 
# 
library(tidywater)
library(tidyverse)
# Define function to simulate lead solubility

  simulate_solubility <- function(water) {
  
    ph = water@ph
    temp = water@temp
    cl = water@cl
    tot_po4 = water@tot_po4
    so4 = water@so4 
    dic = calculate_dic(water=water)
    is = water@is

  # for unit tests
  ph = 7.86
  temp = 25
  is = .005
  cl = 241.8
  dic = 39
  tot_po4 = 0
  so4 = 156.6

  discons_leadsol <- discons %>%
    select(-deltah) %>%
    filter(ID != "kocl") %>%
    rename(species_name = ID,
           K_num = k) %>%
    mutate(constant_name = case_when(species_name == "k1co3" ~ "K_c_1",
                                     species_name == "k2co3" ~ "K_c_2",
                                     species_name == "kso4" ~ "K_s",
                                     species_name == "k1po4" ~ "K_p_1",
                                     species_name == "k2po4" ~ "K_p_2",
                                     species_name == "k3po4" ~ "K_p_3"))
  
  leadsol_K <- leadsol_constants %>%
    mutate(K_num = 10^log_value) %>%
    full_join(discons_leadsol)
  
  
  # There's probably a better way to do this than outputting all constants to global env
  list2env(setNames(as.list(leadsol_K$K_num), leadsol_K$constant_name), .GlobalEnv)
  
  # Convert user selected concentrations to molar concentrations
  #NOTE!! need to add dic, po4, and pb to convert_units
  cl <- convert_units(cl, "cl")
  so4 <- convert_units(so4, "so4")
  dic <-convert_units(dic, "dic")
  tot_po4 <- convert_units(tot_po4, "po4")
  
  # Calculate hydrogen ion activity
  # Convert temperature to Kelvin
  H_plus_a <- 10^-ph
  T_K <- temp + 273.15
  
  # Activity calculations
  epsilon <- 87.74 - 0.4008 * temp + temp^2 * 0.0009398 - temp^3 * 0.00000141
  
  A <- 1824830 * (epsilon * T_K)^-1.5
  
  gamma_1 <- 10^(-A * 1^2 * (is^0.5 / (1 + is^0.5) - 0.3 * is))
  gamma_2 <- 10^(-A * 2^2 * (is^0.5 / (1 + is^0.5) - 0.3 * is))
  gamma_3 <- 10^(-A * 3^2 * (is^0.5 / (1 + is^0.5) - 0.3 * is))
  gamma_4 <- 10^(-A * 4^2 * (is^0.5 / (1 + is^0.5) - 0.3 * is))
  
  # Calculations for carbonate acid-base species

  ###################*
  # Carbonate species ----
  ###################*
  
  # Correct constants for ionic strength
  K_c_1_c <- K_c_1 / gamma_1
  K_c_2_c <- gamma_1 * K_c_2 / gamma_2

  # Calculations for carbonate acid-base species
  alpha_0_c <- 1 / (1 + K_c_1_c / H_plus_a + K_c_1_c * K_c_2_c / H_plus_a^2)
  alpha_1_c <- 1 / (H_plus_a / K_c_1_c + 1 + K_c_2_c / H_plus_a)
  alpha_2_c <- 1 / (H_plus_a^2 / (K_c_1_c * K_c_2_c) + H_plus_a / K_c_2_c + 1)
  
  # Calculate carbonate species concentrations
  
  # should these account for any carbonate already in water? Or maybe already incorporated into dic?
  h2co3 <- alpha_0_c * dic
  hco3 <- alpha_1_c * dic
  co3 <- alpha_2_c * dic
  
  ###################*
  # Phosphate species----
  ###################*
  # Correct constants for ionic strength
  K_p_1_c <- K_p_1 / gamma_1
  K_p_2_c <- gamma_1 * K_p_2 / gamma_2
  K_p_3_c <- gamma_2 * K_p_3 / gamma_3

  # Calculations for phosphate alpha values
  alpha_0_p <- 1 / (1 + K_p_1_c / H_plus_a + K_p_1_c * K_p_2_c / H_plus_a^2 + K_p_1_c * K_p_2_c * K_p_3_c / H_plus_a^3)
  alpha_1_p <- 1 / (H_plus_a / K_p_1_c + 1 + K_p_2_c / H_plus_a + K_p_2_c * K_p_3_c / H_plus_a^2)
  alpha_2_p <- 1 / (H_plus_a^2 / (K_p_1_c * K_p_2_c) + H_plus_a / K_p_2_c + 1 + K_p_3_c / H_plus_a)
  alpha_3_p <- 1 / (H_plus_a^3 / (K_p_1_c * K_p_2_c * K_p_3_c) + H_plus_a^2 / (K_p_2_c * K_p_3_c) + H_plus_a / K_p_3_c + 1)
  
  ###################*
  # Sulfate species ----
  ###################*
  # Correct constants for ionic strength
  K_s_c <- gamma_1 * K_s / gamma_2
  
  # Calculate phosphate species concentrations
  h3po4 <- alpha_0_p * tot_po4
  h2po4 <- alpha_1_p * tot_po4
  hpo4 <- alpha_2_p * tot_po4
  ######should this actually be tot_po4  = alpha* tot_po4??----
  po4 <- alpha_3_p * tot_po4
  
  # Calculations for sulfate acid-base species
  alpha_0_s <- 1 / (1 + K_s_c / H_plus_a)
  alpha_1_s <- 1 / (H_plus_a / K_s_c + 1)
  
  # Calculate sulfate species concentrations
  hso4 <- alpha_0_s * so4
  so4 <- alpha_1_s * so4
  
  # * Calculate lead solid solubility based on controlling solid ----
  
  solids <- leadsol_K %>%
    filter(grepl("solid", constant_name)) %>%
    # Lead Hydroxide: Pb(OH)2(s) + 2H+ --> Pb2+ + 2H2O
    mutate(Pb_2_plus = case_when(constant_name == "K_solid_lead_hydroxide" ~ K_num * H_plus_a^2 / gamma_2,
                                 # Cerussite: PbCO3(s) --> Pb2+ + CO32-
                                 constant_name == "K_solid_cerussite" ~ K_num / (gamma_2^2 * co3),
                                 # Hydrocerussite: Pb3(CO3)2(OH)2(s) + 2H+ --> 3Pb2+ + 2CO32- + 2H2O
                                 constant_name == "K_solid_hydrocerussite" ~ (K_num * H_plus_a^2 / (gamma_2^5 * co3^2))^(1/3),
                                 # Hydroxypyromorphite: Pb5(PO4)3OH(s) + H+ --> 5Pb2+ + 3PO43- + H2O
                                 constant_name == "K_solid_hydroxypyromorphite_s" ~ (K_num * H_plus_a / (gamma_2^5 * gamma_3^3 * po4^3))^(1/5),
                                 constant_name == "K_solid_hydroxypyromorphite_z" ~ (K_num * H_plus_a / (gamma_2^5 * gamma_3^3 * po4^3))^(1/5),
                                 # Pyromorphite: Pb5(PO4)3Cl(s) --> 5Pb2+ + 3PO43- + Cl-
                                 constant_name == "K_solid_pyromorphite_x" ~ (K_num / (gamma_1 * gamma_2^5 * gamma_3^3 * po4^3 * cl))^(1/5),
                                 constant_name == "K_solid_pyromorphite_t" ~ (K_num / (gamma_1 * gamma_2^5 * gamma_3^3 * po4^3 * cl))^(1/5),
                                 # Primary Lead Orthophosphate: Pb(H2PO4)2(s) --> Pb2+ + 2PO43- + 4H+
                                 constant_name == "K_solid_primary_lead_ortho" ~ K_num / (gamma_2 * gamma_3^2 * po4^2 * H_plus_a^4),
                                 # Secondary Lead Orthophosphate: PbHPO4(s) --> Pb2+ + PO43- + H+
                                 constant_name == "K_solid_secondary_lead_ortho" ~ K_num / (gamma_2 * gamma_3 * po4 * H_plus_a),
                                 # Tertiary Lead Orthophosphate: Pb3(PO4)2(s) --> 3Pb2+ + 2PO43- + H+
                                 constant_name == "K_solid_tertiary_lead_ortho" ~ (K_num / (gamma_2^3 * gamma_3^2 * po4^2))^(1/3),
                                 # Anglesite: PbSO4(s) --> Pb2+ + SO42-
                                 constant_name == "K_solid_anglesite" ~ K_num / (gamma_2^2 * so4),
                                 # Laurionite: PbClOH(s) + H+ --> Pb2+ + Cl- + H2O
                                 constant_name == "K_solid_laurionite_nl" ~ K_num * H_plus_a / (gamma_2 * gamma_1 * cl),
                                 constant_name == "K_solid_laurionite_l" ~ K_num * H_plus_a / (gamma_2 * gamma_1 * cl)
                                 ))
  
  # * Calculation of complex concentrations ----
  complexes <- leadsol_K %>%
    filter(!grepl("solid|K_s|K_c|K_p", constant_name)) %>%
    select(-log_value, -species_name, -source) %>%
    pivot_wider(names_from = constant_name, values_from = K_num)
  
  alllead <- solids %>%
    cross_join(complexes) %>%
    mutate(# Calculate lead-hydroxide complex concentrations
      PbOH_plus = (B_1_OH) * gamma_2 * Pb_2_plus / (gamma_1 * H_plus_a),
      PbOH2 = (B_2_OH) * gamma_2 * Pb_2_plus / H_plus_a^2,
      PbOH3_minus = (B_3_OH) * gamma_2 * Pb_2_plus / (gamma_1 * H_plus_a^3),
      PbOH4_2_minus = (B_4_OH) * Pb_2_plus / H_plus_a^4,
      Pb2OH_3_plus = (B_2_1_OH) * gamma_2^2 * Pb_2_plus^2 / (gamma_3 * H_plus_a),
      Pb3OH4_2_plus = (B_3_4_OH) * gamma_2^2 * Pb_2_plus^3 / H_plus_a^4,
      Pb4OH4_4_plus = (B_4_4_OH) * gamma_2^4 * Pb_2_plus^4 / (gamma_4 * H_plus_a^4),
      Pb6OH8_4_plus = (B_6_8_OH) * gamma_2^6 * Pb_2_plus^6 / (gamma_4 * H_plus_a^8),
      # Calculate lead-chloride complex concentrations
      PbCl_plus = (K_1_Cl) * gamma_2 * Pb_2_plus * cl,
      PbCl2 = (B_2_Cl) * gamma_2 * Pb_2_plus * gamma_1^2 * cl^2,
      PbCl3_minus = (B_3_Cl) * gamma_2 * Pb_2_plus * gamma_1^2 * cl^3,
      PbCl4_2_minus = (B_4_Cl) * Pb_2_plus * gamma_1^4 * cl^4,
      # Calculate lead-sulfate complex concentrations
      PbSO4 = (K_1_SO4) * gamma_2^2 * Pb_2_plus * so4,
      PbSO42_2_minus = (B_2_SO4) * gamma_2^2 * Pb_2_plus * so4^2,
      # Calculate lead-carbonate complex concentrations
      PbHCO3_plus = ((K_1_CO3) * H_plus_a * gamma_2^2 * Pb_2_plus * co3) / gamma_1,
      PbCO3 = (K_2_CO3) * gamma_2^2 * Pb_2_plus * co3,
      PbCO32_2_minus = (K_3_CO3) * gamma_2^2 * Pb_2_plus * co3^2,
      # Calculate lead-phosphate complex concentrations
      PbHPO4 = (K_1_PO4) * H_plus_a * gamma_2 * gamma_3 * Pb_2_plus * po4,
      PbH2PO4_plus = (K_2_PO4) * H_plus_a^2 * gamma_2 * gamma_3 * Pb_2_plus * po4 / gamma_1) %>%
    mutate(# Calculate total dissolved lead molar concentration
      TOTSOLPb = Pb_2_plus +
        PbOH_plus + PbOH2 + PbOH3_minus + PbOH4_2_minus + 2 * Pb2OH_3_plus + 3 * Pb3OH4_2_plus + 4 * Pb4OH4_4_plus + 6 * Pb6OH8_4_plus +
        PbCl_plus + PbCl2 + PbCl3_minus + PbCl4_2_minus +
        PbSO4 + PbSO42_2_minus +
        PbHCO3_plus + PbCO3 + PbCO32_2_minus +
        PbHPO4 + PbH2PO4_plus)
  
  alllead_simple <- alllead %>%
    select(species_name, Pb_2_plus, TOTSOLPb) %>%
    mutate(ph = ph,
           dic = dic,
           tot_po4 = tot_po4,
           so4 = so4,
           is = is,
           cl=cl
           #output other things like hco3, co3 etc too?
           ) 
  
  return(alllead_simple)
  
  }
  
  # these don't match epa dash, but epa doesn't have input for temp...
  test <- define_water(8, 25, 94, cl=100, tot_po4 =0, so4 = 100, tds = 200)
  dic <- calculate_dic(test) #dic = 22.9
  test_sol <- test %>%
    simulate_solubility()
  simulate_solubility_epa(pH_single = 8, IS_mM_single = 0.005, Cl_minus_mg_L_single = 100, DIC_mg_L_single = 22.9, TOTP_mg_L_single = 0, TOTSO4_mg_L_single = 100)
  
# Controlling solid
# Solid with lowest solubility will form the lead scale

#' Calc DIC (TOTCO3)
#'
#' This function calcs dic
#'
#' @param water a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' example_df <- water_df %>% define_water_once()
#'
#' @export
#' 
# Copied from Sierra's code.  Copied from Damon's code before that.
  # NOTE: doesn't account for temperature. should we incorporate? Ben Trueman uses temp, but this requires phreeqc bleh
  #https://github.com/bentrueman/pbcusol/blob/main/R/calculate_dic.R
calculate_dic <- function(water) {
  
  Alkalinity = water@alk
  ph = water@ph
  
  # Convert alkalinity from mg/L as CaCO3 to meq/L
  Alkalinity_eq <- Alkalinity * (1/50) / 1000
  
  # pH_EndPoint is the pH used for the endpoint of the titration curve to define alkalinity
  pH_EndPoint <- 4.5
  
  # This calculates the initial and final {H+} and {OH-} based on ph
  H_Concentration_initial <- 10^-ph
  H_Concentration_final <- 10^-pH_EndPoint
  OH_Concentration_initial <- (10^-14)/H_Concentration_initial
  OH_Concentration_final <- (10^-14)/H_Concentration_final
  
  #Ka1 and Ka2 are the acidity constants for carbonic acid (need to be corrected for temperature?)
  Ka1_CO3 <- 10^-6.35
  Ka2_CO3 <- 10^-10.33
  
  # alpha - fraction of TOT with each protonation, alpha1 - HCO3, alpha2 - CO3 (number based on charge)
  # Benjamin 5.39 and 5.40
  alpha1_initial <- 1/((H_Concentration_initial/Ka1_CO3)+1+(Ka2_CO3/H_Concentration_initial))
  alpha2_initial <- 1/(((H_Concentration_initial^2)/(Ka1_CO3*Ka2_CO3))+(H_Concentration_initial/Ka2_CO3)+1)
  alpha1_final <- 1/((H_Concentration_final/Ka1_CO3)+1+(Ka2_CO3/H_Concentration_final))
  alpha2_final <- 1/(((H_Concentration_final^2)/(Ka1_CO3*Ka2_CO3))+(H_Concentration_final/Ka2_CO3)+1)
  
  # Calculate TOTCO3 (rearrangement of Benjamin 8.20 + 8.21b)
  # I think the result is in mol/L ???
  TOTCO3_eq <- ((H_Concentration_final - H_Concentration_initial) - (OH_Concentration_final - OH_Concentration_initial) - Alkalinity_eq) /
    ((alpha1_final- alpha1_initial) + 2 * (alpha2_final - alpha2_initial))
  
  # Simplified Benjamin eqn (8.16) as a check - matches pretty well, commenting this out.
  # TOTCO3_approx <- (Alkalinity_eq) / (alpha1_initial + 2 * alpha2_initial)
  
  atom_weight_C <- 12.0107
  # Do we need to convert to M first?
  dic <- TOTCO3_eq * atom_weight_C * 1000
  return(dic)
}
