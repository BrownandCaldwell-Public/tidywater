leadsol_constants <- data.frame(
  species_name = c("Lead Hydroxide",
                   "Cerussite",
                   "Hydrocerussite",
                   "Hydroxypyromorphite_s",
                   "Hydroxypyromorphite_z",
                   "Pyromorphite_x",
                   "Pyromorphite_t",
                   "Primary Lead Orthophosphate",
                   "Secondary Lead Orthophosphate",
                   "Tertiary Lead Orthophosphate",
                   "Anglesite",
                   "Laurionite_nl",
                   "Laurionite_l",
                   "PbOH+",
                   "Pb(OH)2",
                   "Pb(OH)3-",
                   "Pb(OH)4-2",
                   "Pb2OH+3",
                   "Pb3(OH)4+2",
                   "Pb4(OH)4+4",
                   "Pb6(OH)8+4",
                   "PbCl+1",
                   "PbCl2",
                   "PbCl3-",
                   "PbCl4-2",
                   "SO4-2",
                   "PbSO4",
                   "Pb(SO4)2-2",
                   "Bicarbonate",
                   "Carbonate",
                   "PbHCO3+",
                   "PbCO3",
                   "Pb(CO3)2-2",
                   "H2PO4-",
                   "HPO4—2",
                   "PO4-3",
                   "PbHPO4",
                   "PbH2PO4+"
  ),
  
  # following constants are from Schock et al. (1996) unless otherwise indicated
  
  # Solids 
  constant_name = c(
    "K_solid_lead_hydroxide",
    
    "K_solid_cerussite",
    "K_solid_hydrocerussite" ,
    
    "K_solid_hydroxypyromorphite_schock",
    "K_solid_hydroxypyromorphite_zhu" , #Zhu et al. (2015)
    
    "K_solid_pyromorphite_xie", # Xie & Giammar (2007)
    "K_solid_pyromorphite_topolska", # Topolska et al. (2016)
    
    "K_solid_primary_lead_ortho", # Powell et al. (2009)
    "K_solid_secondary_lead_ortho" ,
    "K_solid_tertiary_lead_ortho" , # Powell et al. (2009)
    
    "K_solid_anglesite",
    "K_solid_laurionite_nl", # Nasanen & Lindell (1976)
    "K_solid_laurionite_loth", # Lothenbach et al. (1999)
    
    # Lead-Hydroxide Complexes
    
    "B_1_OH", 
    "B_2_OH" , 
    "B_3_OH", 
    "B_4_OH", 
    "B_2_1_OH", 
    "B_3_4_OH" , 
    "B_4_4_OH" , 
    "B_6_8_OH",
    
    # Lead-Chloride Complexes
    
    "K_1_Cl" , 
    "B_2_Cl", 
    "B_3_Cl" , 
    "B_4_Cl" ,
    
    # Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes
    
    "K_s" , # Benjamin (2002) 
    "K_1_SO4" , 
    "B_2_SO4" ,
    
    # Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes
    
    "K_c_1" , #Powell et al. (2005) 
    "K_c_2" , #Powell et al. (2005) 
    "K_1_CO3" , 
    "K_2_CO3" , 
    "K_3_CO3" ,
    
    # Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes
    
    "K_p_1" , #Powell et al. (2005) 
    "K_p_2" , #Powell et al. (2005)  
    "K_p_3" , #Powell et al. (2005) 
    "K_1_PO4" ,
    "K_2_PO4" ),
  
  log_value = c(
    13.06,
    -13.11,
    -18,
    -62.83,
    -66.77, #Zhu et al. (2015)
    -80.4, # Xie & Giammar (2007)
    -79.6, # Topolska et al. (2016)
    -48.916, # Powell et al. (2009)
    -23.81,
    -44.4, # Powell et al. (2009)
    -7.79,
    0.619, # Nasanen & Lindell (1976)
    0.29, # Lothenbach et al. (1999)
    
    # Lead-Hydroxide Complexes
    
    -7.22, 
    -16.91, 
    -28.08, 
    -39.72, 
    -6.36, 
    -23.86, 
    -20.8, 
    -43.62,
    
    # Lead-Chloride Complexes
    
    1.59, 
    1.8, 
    1.71, 
    1.43,
    
    # Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes
    
    -1.99, # Benjamin (2002) 
    2.73, 
    3.5,
    
    # Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes
    
    -6.355, #Powell et al. (2005) 
    -10.336, #Powell et al. (2005) 
    12.59, 
    7.1, 
    10.33,
    
    # Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes
    
    -2.141, #Powell et al. (2005) 
    -7.2, #Powell et al. (2005)  
    -12.338, #Powell et al. (2005) 
    15.41,
    21.05)
  
)
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

  simulate_solubility <- function(water, is) {
  
    ph = water@ph
    temp = water@temp
    cl = water@cl
    tot_po4 = water@tot_po4
    so4 = water@so4 
    dic = calculate_dic(water=water)
    #use this once IS updates added to define water
    # is = water@is

  # for unit tests
  # ph = 7.86
  # temp = 7.1
  # is = .02
  # cl = 241.8
  # DICmg = 39
  # tot_po4 = 0
  # so4 = 156.6

  leadsol_K <- leadsol_constants %>%
    mutate(K_num = 10^log_value)
  
  # There's probably a better way to do this than outputting all constants to global env
  list2env(setNames(as.list(leadsol_K$K_num), leadsol_K$constant_name), .GlobalEnv)
  
  # Set molecular weights. NOTE!! Delete this once weights has pb, dic merged.

  Pb_MW <- 207.2
  DIC_MW <- 12.011
  
  
  # Convert user selected concentrations to molar concentrations
  #NOTE!! need to add dic, po4, and pb to convert_units
  cl <- convert_units(cl, "cl")
  so4 <- convert_units(so4, "so4")
  dic <- dic / 12.011 / 1000
  # dic <- dic/mweights$dic/1000
  tot_po4 <- tot_po4 / mweights$po4/1000
  # tot_po4 <- convert_units(tot_po4, "po4")
  
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
                                 constant_name == "K_solid_hydroxypyromorphite_schock" ~ (K_num * H_plus_a / (gamma_2^5 * gamma_3^3 * po4^3))^(1/5),
                                 constant_name == "K_solid_hydroxypyromorphite_zhu" ~ (K_num * H_plus_a / (gamma_2^5 * gamma_3^3 * po4^3))^(1/5),
                                 # Pyromorphite: Pb5(PO4)3Cl(s) --> 5Pb2+ + 3PO43- + Cl-
                                 constant_name == "K_solid_pyromorphite_xie" ~ (K_num / (gamma_1 * gamma_2^5 * gamma_3^3 * po4^3 * cl))^(1/5),
                                 constant_name == "K_solid_pyromorphite_topolska" ~ (K_num / (gamma_1 * gamma_2^5 * gamma_3^3 * po4^3 * cl))^(1/5),
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
                                 constant_name == "K_solid_laurionite_loth" ~ K_num * H_plus_a / (gamma_2 * gamma_1 * cl)
                                 ))
  
  # * Calculation of complex concentrations ----
  complexes <- leadsol_K %>%
    filter(!grepl("solid|K_s|K_c|K_p", constant_name)) %>%
    select(-log_value, -species_name) %>%
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
  test <- define_water(8, 25, 100, cl=100, tot_po4 =0, so4 = 100)
  dic <- calculate_dic(test)
  test_sol <- test %>%
    simulate_solubility(is = 5)
  
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
