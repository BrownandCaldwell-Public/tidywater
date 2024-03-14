# Generate data frames of data used across tidywater functions

# List of molecular weights for different chemical additions
mweights <- data.frame(na = 22.98977,
  k = 39.0983,
  cl = 35.453,
  so4 = 96.0626,
  caco3 = 100.0869,
  hcl = 36.46094,
  h2so4 = 98.079,
  h3po4 = 97.995181,
  naoh = 39.9971,
  na2co3 = 105.98844,
  nahco3 = 84.00661,
  caoh2 = 74.09268,
  mgoh2 = 58.31968,
  cl2 = 70.906,
  co2 = 44.009,
  al = 26.981539,
  fe = 55.845,
  alum = 26.981539 * 2 + 96.0626 * 3 + 14 * 18.01528,
  fecl3 = 55.845 + 35.453 * 3,
  fe2so43 = 2 * 55.845 + 3 * 96.0626,
  mg = 24.305,
  ca = 40.078,
  po4 = 94.97,
  hco3 = 61.0168,
  co3 = 60.0089)

usethis::use_data(mweights, overwrite = TRUE)

# List of acid dissociation constants
discons <- data.frame( # Carbonic acid
  k1co3 = 10^-6.35, # H2CO3<-->HCO3- + H+
  k2co3 = 10^-10.33, # HCO3<-->CO32- + H+

  # Sulfate
  kso4 = 10^-1.99, # H2SO4<-->2H+ + SO42-

  # Phosphate
  k1po4 = 10^-2.16, # H3PO4<-->H+ + H2PO4-
  k2po4 = 10^-7.20, # H2PO4-<-->H+ + HPO42-
  k3po4 = 10^-12.35, # HPO42--<-->H+ + PO43-

  # Hypochlorite
  kocl = 10^-7.6) # HOCl<-->H+ + OCl-

usethis::use_data(discons, overwrite = TRUE)

# Dummy data frame for function examples

water_df <- data.frame(
  ph = rep(c(7.9, 8.5, 8.1, 7.8), 3),
  temp =  rep(c(20, 25, 19), 4),
  alk = rep(c(50, 80, 100, 200), 3),
  tot_hard = rep(c(50, 75, 100, 30, 400, 110), 2),
  ca_hard = rep(c(50, 70, 65, 20, 350, 100), 2),
  na= rep(c(20, 90), 6),
  k= rep(c(20, 90), 6),
  cl = rep(c(30, 92), 6),
  so4 = rep(c(20, 40, 60, 80), 3),
  tot_ocl = rep(c(0, 1), 6),
  tot_po4 = rep(c(0, 0, 1), 4))

usethis::use_data(water_df, overwrite = TRUE)

# Data frame of Edwards model coefficients

edwardscoeff <- data.frame(
  ID = "Alum",
  x3 = 4.91, x2 = -74.2, x1 = 284,
  k1 = -0.075, k2 = 0.56,
  b = 0.147) %>%
  add_row(ID = "Ferric",
          x3 = 4.96, x2 = -73.9, x1 = 280,
          k1 = -0.028, k2 = 0.23,
          b = 0.068) %>%
  add_row(ID = "Low DOC",
          x3 = 6.44, x2 = -99.2, x1 = 387,
          k1 = -0.053, k2 = 0.54,
          b = 0.107) %>%
  add_row(ID = "General Alum",
          x3 = 6.42, x2 = -98.6, x1 = 383,
          k1 = -0.054, k2 = 0.54,
          b = 0.145) %>%
  add_row(ID = "General Ferric",
          x3 = 6.42, x2 = -98.6, x1 = 383,
          k1 = -0.054, k2 = 0.54,
          b = 0.092)

usethis::use_data(edwardscoeff, overwrite = TRUE)

# Data frame of equilibrium constants for lead and copper solubility

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

usethis::use_data(leadsol_constants, overwrite = TRUE)
