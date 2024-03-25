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
  co3 = 60.0089,
  pb = 207.2,
  dic = 12.011)

usethis::use_data(mweights, overwrite = TRUE)

# Acid dissociation constants and corresponding enthalpy
# Carbonic acid
discons <- data.frame(ID = c("k1co3", "k2co3"), # H2CO3<-->HCO3- + H+; HCO3<-->CO32- + H+
                      k = c(10^-6.35, 10^-10.33),
                      deltah = c(7700, 14900)) %>% #J/mol
  # Sulfate
  add_row(ID = "kso4", k = 10^-1.99, deltah = -21900) %>%
  # Phosphate
  # H3PO4<-->H+ + H2PO4-; H2PO4-<-->H+ + HPO42-; HPO42--<-->H+ + PO43-
  add_row(ID = c("k1po4", "k2po4", "k3po4"), k = c(10^-2.16, 10^-7.20, 10^-12.35), deltah = c(-8000, 4200, 14700)) %>%
  # Hypochlorite
  add_row(ID = "kocl", k = 10^-7.6, deltah = 13800)# HOCl<-->H+ + OCl-

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
  tot_po4 = rep(c(0, 0, 1), 4),
  tds = rep(c(200, 100, NA), 4),
  cond = rep(c(100, 150, NA), 4),
  toc = rep(c(2, 3, 4), 4),
  doc = rep(c(1.8, 2.8, 3.5), 4),
  uv254 = rep(c(.05, .08, .12), 4))


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
  species_name = c("Lead Hydroxide", "Cerussite",  "Hydrocerussite"),
  constant_name = c("K_solid_lead_hydroxide", "K_solid_cerussite", "K_solid_hydrocerussite"),
  log_value = c(13.06, -13.11,  -18),
  source = rep("Schock et al. (1996)", 3)) %>%
  
  # Solids
  add_row(species_name = c("Hydroxypyromorphite", "Hydroxypyromorphite", "Pyromorphite", "Pyromorphite"),  
          constant_name = c("K_solid_hydroxypyromorphite_s", "K_solid_hydroxypyromorphite_z" ,  "K_solid_pyromorphite_x",  "K_solid_pyromorphite_t"), 
          log_value = c(62.83,-66.77, -80.4, -79.6),
          source = c("Schock et al. (1996)", "Zhu et al. (2015)", "Xie & Giammar (2007)", "Topolska et al. (2016)")) %>%
  
  add_row(species_name = c( "Primary Lead Orthophosphate", "Secondary Lead Orthophosphate", "Tertiary Lead Orthophosphate"),  
          constant_name = c( "K_solid_primary_lead_ortho", "K_solid_secondary_lead_ortho" , "K_solid_tertiary_lead_ortho"), 
          log_value = c(-48.916, -23.81, -44.4),
          source = c("Powell et al. (2009)", "Schock et al. (1996)", "Powell et al. (2009)")) %>%
  
  add_row(species_name = c( "Anglesite", "Laurionite","Laurionite"),  
          constant_name = c( "K_solid_anglesite", "K_solid_laurionite_nl", "K_solid_laurionite_l"),
          log_value = c(-7.79, 0.619, 0.29),
          source = c("Schock et al. (1996)", "Nasanen & Lindell (1976)", "Lothenbach et al. (1999)")) %>%
  
  # Lead-Hydroxide Complexes
  add_row(species_name = c("PbOH+", "Pb(OH)2", "Pb(OH)3-", "Pb(OH)4-2"),  
          constant_name = c(  "B_1_OH", "B_2_OH" , "B_3_OH", "B_4_OH"),
          log_value = c(-7.22, -16.91, -28.08, -39.72),
          source = rep("Schock et al. (1996)", 4)) %>%
  add_row(species_name = c("Pb2OH+3", "Pb3(OH)4+2", "Pb4(OH)4+4", "Pb6(OH)8+4"),  
          constant_name = c("B_2_1_OH", "B_3_4_OH" , "B_4_4_OH", "B_6_8_OH"),
          log_value = c(-6.36, -23.86, -20.88, -43.62),
          source = rep("Schock et al. (1996)", 4)) %>%
  
  # Lead-Chloride Complexes
  add_row(species_name = c("PbCl+1", "PbCl2", "PbCl3-", "PbCl4-2"),  
          constant_name = c("K_1_Cl", "B_2_Cl", "B_3_Cl", "B_4_Cl"),
          log_value = c(1.59, 1.8, 1.71, 1.43),
          source = rep("Schock et al. (1996)", 4)) %>%
  
  # Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes
  add_row(species_name = c("PbSO4", "Pb(SO4)2-2"),  
          constant_name = c("K_1_SO4", "B_2_SO4"),
          log_value = c(2.73, 3.5),
          source = rep("Schock et al. (1996)", 2)) %>%
  
  # Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes
  add_row(species_name = c("PbHCO3+", "PbCO3", "Pb(CO3)2-2"),  
          constant_name = c("K_1_CO3", "K_2_CO3", "K_3_CO3"),
          log_value = c(12.59, 7.1, 10.33),
          source = rep("Schock et al. (1996)", 3)) %>%
  
  # Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes
  add_row(species_name = c("PbHPO4", "PbH2PO4+"),  
          constant_name = c( "K_1_PO4" , "K_2_PO4"),
          log_value = c(15.41, 21.05),
          source = rep("Schock et al. (1996)", 2))
  
  usethis::use_data(leadsol_constants, overwrite = TRUE)
