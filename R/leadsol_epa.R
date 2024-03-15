leadsol_constants <- data.frame(
  # following constants are from Schock et al. (1996) unless otherwise indicated
  
  # Solids 
  
  "K_solid_lead_hydroxide"= 13.06,
  
  "K_solid_cerussite" = -13.11,
  "K_solid_hydrocerussite" = -18,
  
  "K_solid_hydroxypyromorphite_schock" = -62.83,
  "K_solid_hydroxypyromorphite_zhu" = -66.77, #Zhu et al. (2015)
  
  "K_solid_pyromorphite_xie" = -80.4, # Xie & Giammar (2007)
  "K_solid_pyromorphite_topolska" = -79.6, # Topolska et al. (2016)
  
  "K_solid_primary_lead_ortho" = -48.916, # Powell et al. (2009)
  "K_solid_secondary_lead_ortho" = -23.81,
  "K_solid_tertiary_lead_ortho" = -44.4, # Powell et al. (2009)
  
  
  
  "K_solid_anglesite" = -7.79,
  "K_solid_laurionite_nl" = 0.619, # Nasanen & Lindell (1976)
  "K_solid_laurionite_loth" = 0.29, # Lothenbach et al. (1999)
  
  # Lead-Hydroxide Complexes
  
  "B_1_OH" = -7.22, 
  "B_2_OH" = -16.91, 
  "B_3_OH" = -28.08, 
  "B_4_OH" = -39.72, 
  "B_2_1_OH" = -6.36, 
  "B_3_4_OH" = -23.86, 
  "B_4_4_OH" = -20.8, 
  "B_6_8_OH" = -43.62,
  
  # Lead-Chloride Complexes
  
  "K_1_Cl" = 1.59, 
  "B_2_Cl" = 1.8, 
  "B_3_Cl" = 1.71, 
  "B_4_Cl" = 1.43,
  
  # Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes
  
  "K_s" = -1.99, # Benjamin (2002) 
  "K_1_SO4" = 2.73, 
  "B_2_SO4" = 3.5,
  
  # Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes
  
  "K_c_1" = -6.355, #Powell et al. (2005) 
  "K_c_2" = -10.336, #Powell et al. (2005) 
  "K_1_CO3" = 12.59, 
  "K_2_CO3" = 7.1, 
  "K_3_CO3" = 10.33,
  
  # Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes
  
  "K_p_1" = -2.141, #Powell et al. (2005) 
  "K_p_2" = -7.2, #Powell et al. (2005)  
  "K_p_3" = -12.338, #Powell et al. (2005) 
  "K_1_PO4" = 15.41,
  "K_2_PO4" = 21.05)

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
library(tidywater)
library(tidyverse)
# test <- simulate_solubility(7, 20, .1, 20, 100, 10, 30)

# Define function to simulate lead solubility
# simulate_solubility <- function(pH, 
#                                 Temperature, # C
#                                 IS, # M 
#                                 Chloride, # mg/L
#                                 DICmg, # mg/L  
#                                 TOTPmg, # mg/L
#                                 TOTSO4mg) { # mg/L
  simulate_solubility <- function(water, IS, DICmg) { # mg/L
  
    pH = water@pH
    Temperature = water@temp
    Chloride = water@cl
    TOTPmg = water@tot_po4
    TOTSO4mg = water@so4 
    
    
  # for unit tests
  # pH = 7.86
  # Temperature = 7.1
  # IS = .02
  # Chloride = 241.8
  # DICmg = 39
  # TOTPmg = 0
  # TOTSO4mg = 156.6
  
  bc_drive("Englewood Modeling")
  
  eq_constants <- read_excel("constants.xlsx", sheet = "Constants") %>%
    mutate(K_num = 10^as.numeric(gsub("10\\^","",K)))
  
  # Set molecular weights
  Pb_MW <- 207.2
  chloride_MW <- 35.453
  sulfate_MW <- 32.065 + 4 * 15.999
  DIC_MW <- 12.011
  phosphate_MW <- 94.97
  
  # Convert user selected concentrations to molar concentrations
  # Set pH, DIC, phosphate, sulfate, ionic strength, and chloride per user selections
  Cl_minus <- Chloride / chloride_MW / 1000
  TOTSO4 <- TOTSO4mg / sulfate_MW / 1000
  DIC <- DICmg / DIC_MW / 1000
  TOTP <- TOTPmg / phosphate_MW / 1000
  # Calculate hydrogen ion activity
  # Convert temperature to Kelvin
  H_plus_a <- 10^-pH
  T_K <- Temperature + 273.15
  
  # Activity calculations
  epsilon <- 87.74 - 0.4008 * Temperature + Temperature^2 * 0.0009398 - Temperature^3 * 0.00000141
  
  A <- 1824830 * (epsilon * T_K)^-1.5
  
  gamma_1 <- 10^(-A * 1^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))
  gamma_2 <- 10^(-A * 2^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))
  gamma_3 <- 10^(-A * 3^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))
  gamma_4 <- 10^(-A * 4^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))
  
  
  # Correct constants for ionic strength
  acid_base <- eq_constants %>%
    filter(Group == "Carbonate" | Group == "Sulfate" | Group == "Phosphate") %>%
    mutate(K_corrected = case_when(molsPb == 1 ~ K_num / gamma_1,
                                   molsPb == 2 ~ gamma_1 * K_num / gamma_2,
                                   molsPb == 3 ~ gamma_2 * K_num / gamma_3)) %>%
    select(sim_Names, K_corrected) %>%
    pivot_wider(names_from = sim_Names, values_from = K_corrected)
  
  # Calculations for carbonate acid-base species
  alpha_0_c <- 1 / (1 + acid_base$K_c_1 / H_plus_a + acid_base$K_c_1 * acid_base$K_c_2 / H_plus_a^2)
  alpha_1_c <- 1 / (H_plus_a / acid_base$K_c_1 + 1 + acid_base$K_c_2 / H_plus_a)
  alpha_2_c <- 1 / (H_plus_a^2 / (acid_base$K_c_1 * acid_base$K_c_2) + H_plus_a / acid_base$K_c_2 + 1)
  
  # Calculate carbonate species concentrations
  H2CO3 <- alpha_0_c * DIC
  HCO3_minus <- alpha_1_c * DIC
  CO3_2_minus <- alpha_2_c * DIC
  
  # Calculations for phosphate acid-base species
  alpha_0_p <- 1 / (1 + acid_base$K_p_1 / H_plus_a +
                      acid_base$K_p_1 * acid_base$K_p_2 / H_plus_a^2 + 
                      acid_base$K_p_1 * acid_base$K_p_2 * acid_base$K_p_3 / H_plus_a^3)
  alpha_1_p <- 1 / (H_plus_a / acid_base$K_p_1 + 1 + 
                      acid_base$K_p_2 / H_plus_a + 
                      acid_base$K_p_2 * acid_base$K_p_3 / H_plus_a^2)
  alpha_2_p <- 1 / (H_plus_a^2 / (acid_base$K_p_1 * acid_base$K_p_2) + 
                      H_plus_a / acid_base$K_p_2 + 
                      1 + acid_base$K_p_3 / H_plus_a)
  alpha_3_p <- 1 / (H_plus_a^3 / (acid_base$K_p_1 * acid_base$K_p_2 * acid_base$K_p_3) + 
                      H_plus_a^2 / (acid_base$K_p_2 * acid_base$K_p_3) + 
                      H_plus_a / acid_base$K_p_3 + 1)
  
  # Calculate phosphate species concentrations
  H3PO4 <- alpha_0_p * TOTP
  H2PO4_minus <- alpha_1_p * TOTP
  HPO4_2_minus <- alpha_2_p * TOTP
  PO4_3_minus <- alpha_3_p * TOTP
  
  # Calculations for sulfate acid-base species
  alpha_0_s <- 1 / (1 + acid_base$K_s / H_plus_a)
  alpha_1_s <- 1 / (H_plus_a / acid_base$K_s + 1)
  
  # Calculate sulfate species concentrations
  HSO4_minus <- alpha_0_s * TOTSO4
  SO4_2_minus <- alpha_1_s * TOTSO4
  
  # * Calculate lead solid solubility based on controlling solid ----
  
  #Set constants for each solid
  solids <- eq_constants %>%
    filter(Group == "Solids") %>%
    # Lead Hydroxide: Pb(OH)2(s) + 2H+ --> Pb2+ + 2H2O
    mutate(Pb_2_plus = case_when(sim_Names == "K_solid_lead_hydroxide" ~ K_num * H_plus_a^2 / gamma_2,
                                 # Cerussite: PbCO3(s) --> Pb2+ + CO32-
                                 sim_Names == "K_solid_cerussite" ~ K_num / (gamma_2^2 * CO3_2_minus),
                                 # Hydrocerussite: Pb3(CO3)2(OH)2(s) + 2H+ --> 3Pb2+ + 2CO32- + 2H2O
                                 sim_Names == "K_solid_hydrocerussite" ~ (K_num * H_plus_a^2 / (gamma_2^5 * CO3_2_minus^2))^(1/3),
                                 # Hydroxypyromorphite: Pb5(PO4)3OH(s) + H+ --> 5Pb2+ + 3PO43- + H2O
                                 sim_Names == "K_solid_hydroxypyromorphite" ~ (K_num * H_plus_a / (gamma_2^5 * gamma_3^3 * PO4_3_minus^3))^(1/5),
                                 # Pyromorphite: Pb5(PO4)3Cl(s) --> 5Pb2+ + 3PO43- + Cl-
                                 sim_Names == "K_solid_pyromorphite" ~ (K_num / (gamma_1 * gamma_2^5 * gamma_3^3 * PO4_3_minus^3 * Cl_minus))^(1/5),
                                 # Primary Lead Orthophosphate: Pb(H2PO4)2(s) --> Pb2+ + 2PO43- + 4H+
                                 sim_Names == "K_solid_primary_lead_ortho" ~ K_num / (gamma_2 * gamma_3^2 * PO4_3_minus^2 * H_plus_a^4),
                                 # Secondary Lead Orthophosphate: PbHPO4(s) --> Pb2+ + PO43- + H+
                                 sim_Names == "K_solid_secondary_lead_ortho" ~ K_num / (gamma_2 * gamma_3 * PO4_3_minus * H_plus_a),
                                 # Tertiary Lead Orthophosphate: Pb3(PO4)2(s) --> 3Pb2+ + 2PO43- + H+
                                 sim_Names == "K_solid_tertiary_lead_ortho" ~ (K_num / (gamma_2^3 * gamma_3^2 * PO4_3_minus^2))^(1/3),
                                 # Anglesite: PbSO4(s) --> Pb2+ + SO42-
                                 sim_Names == "K_solid_anglesite" ~ K_num / (gamma_2^2 * SO4_2_minus),
                                 # Laurionite: PbClOH(s) + H+ --> Pb2+ + Cl- + H2O
                                 sim_Names == "K_solid_laurionite" ~ K_num * H_plus_a / (gamma_2 * gamma_1 * Cl_minus)))
  
  # * Calculation of complex concentrations ----
  complexes <- eq_constants %>%
    filter(grepl("Complexes", Group)) %>%
    select(sim_Names, K_num) %>%
    pivot_wider(names_from = sim_Names, values_from = K_num)
  
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
      PbCl_plus = (K_1_Cl) * gamma_2 * Pb_2_plus * Cl_minus,
      PbCl2 = (B_2_Cl) * gamma_2 * Pb_2_plus * gamma_1^2 * Cl_minus^2,
      PbCl3_minus = (B_3_Cl) * gamma_2 * Pb_2_plus * gamma_1^2 * Cl_minus^3,
      PbCl4_2_minus = (B_4_Cl) * Pb_2_plus * gamma_1^4 * Cl_minus^4,
      # Calculate lead-sulfate complex concentrations
      PbSO4 = (K_1_SO4) * gamma_2^2 * Pb_2_plus * SO4_2_minus,
      PbSO42_2_minus = (B_2_SO4) * gamma_2^2 * Pb_2_plus * SO4_2_minus^2,
      # Calculate lead-carbonate complex concentrations
      PbHCO3_plus = ((K_1_CO3) * H_plus_a * gamma_2^2 * Pb_2_plus * CO3_2_minus) / gamma_1,
      PbCO3 = (K_2_CO3) * gamma_2^2 * Pb_2_plus * CO3_2_minus,
      PbCO32_2_minus = (K_3_CO3) * gamma_2^2 * Pb_2_plus * CO3_2_minus^2,
      # Calculate lead-phosphate complex concentrations
      PbHPO4 = (K_1_PO4) * H_plus_a * gamma_2 * gamma_3 * Pb_2_plus * PO4_3_minus,
      PbH2PO4_plus = (K_2_PO4) * H_plus_a^2 * gamma_2 * gamma_3 * Pb_2_plus * PO4_3_minus / gamma_1) %>%
    mutate(# Calculate total dissolved lead molar concentration
      TOTSOLPb = Pb_2_plus +
        PbOH_plus + PbOH2 + PbOH3_minus + PbOH4_2_minus + 2 * Pb2OH_3_plus + 3 * Pb3OH4_2_plus + 4 * Pb4OH4_4_plus + 6 * Pb6OH8_4_plus +
        PbCl_plus + PbCl2 + PbCl3_minus + PbCl4_2_minus +
        PbSO4 + PbSO42_2_minus +
        PbHCO3_plus + PbCO3 + PbCO32_2_minus +
        PbHPO4 + PbH2PO4_plus)
  
  alllead_simple <- alllead %>%
    select(Names, Pb_2_plus, TOTSOLPb)
  
  return(alllead_simple)
  
}

# Controlling solid
# Solid with lowest solubility will form the lead scale



# Copied from my other code.  Copied from Damon's code before that.
TOTCO3_calc <- function(Alkalinity, pH) {
  
  # Convert alkalinity from mg/L as CaCO3 to meq/L
  Alkalinity_eq <- Alkalinity * (1/50) / 1000
  
  # pH_EndPoint is the pH used for the endpoint of the titration curve to define alkalinity
  pH_EndPoint <- 4.5
  
  # This calculates the initial and final {H+} and {OH-} based on pH
  H_Concentration_initial <- 10^-pH
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
  DIC <- TOTCO3_eq * atom_weight_C * 1000
  return(DIC)
}
