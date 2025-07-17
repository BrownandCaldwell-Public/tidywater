# Generate data frames of data used across tidywater functions

# mweights ----
# List of molecular weights for different chemical additions
mweights <- data.frame(
  na = 22.98977,
  k = 39.0983,
  cl = 35.453,
  so4 = 96.0626,
  caco3 = 100.0869,
  caso4 = 136.141,
  hcl = 36.46094,
  h2so4 = 98.079,
  h3po4 = 97.995181,
  naoh = 39.9971,
  na2co3 = 105.98844,
  nahco3 = 84.00661,
  caoh2 = 74.09268,
  mgoh2 = 58.31968,
  cacl2 = 110.98,
  cl2 = 70.906,
  co2 = 44.009,
  al = 26.981539,
  fe = 55.845,
  alum = 26.981539 * 2 + 96.0626 * 3 + 14 * 18.01528, # 14 H2O
  ferricchloride = 55.845 + 35.453 * 3,
  fecl3 = 55.845 + 35.453 * 3,
  ferricsulfate = 2 * 55.845 + 3 * 96.0626 + 8.8 * 18.01528, # 8.8 H2O
  ach = 26.981539 * 2 + 17.008 * 5 + 35.453 + 2 * 18.01528, # 2 H2O
  mg = 24.305,
  ca = 40.078,
  po4 = 94.97,
  na3po4 = 163.939,
  hpo4 = 95.98,
  h2po4 = 96.99,
  h2co3 = 62.024,
  hco3 = 61.0168,
  co3 = 60.0089,
  ocl = 16 + 35.453,
  pb = 207.2,
  br = 79.904,
  bro3 = 79.904 + 3 * 15.999,
  f = 18.9984,
  naf = 41.9882,
  mn = 54.938,
  nh4 = 18.04,
  nh3 = 17.031,
  no3 = 62.005,
  hno3 = 63.0128,
  nh2cl = 51.48,
  nhcl2 = 85.92,
  ncl3 = 120.365,
  n = 14.0067,
  nh4oh = 18.04 + 17.008,
  nh42so4 = 18.04 * 2 + 96.0626,
  oh = 17.008,
  kmno4 = 158.032,
  mno4 = 158.032 - 39.0983,
  dic = 12.011
)

usethis::use_data(mweights, overwrite = TRUE)

# discons ----
# Acid dissociation constants and corresponding enthalpy
# Carbonic acid
discons <- data.frame(
  ID = c("k1co3", "k2co3"), # H2CO3<-->HCO3- + H+; HCO3<-->CO32- + H+
  k = c(10^-6.35, 10^-10.33),
  deltah = c(7700, 14900) # J/mol
) %>%
  # Sulfate
  add_row(ID = "kso4", k = 10^-1.99, deltah = -21900) %>%
  # Phosphate
  # H3PO4<-->H+ + H2PO4-; H2PO4-<-->H+ + HPO42-; HPO42--<-->H+ + PO43-
  add_row(ID = c("k1po4", "k2po4", "k3po4"), k = c(10^-2.16, 10^-7.20, 10^-12.35), deltah = c(-8000, 4200, 14700)) %>%
  # Hypochlorite
  add_row(ID = "kocl", k = 10^-7.53, deltah = 13800) %>% # HOCl<-->H+ + OCl-
  # Ammonia
  add_row(ID = "knh4", k = 10^-9.244, deltah = 52210) # NH4+ <--> NH3 + H+
rownames(discons) <- discons$ID

usethis::use_data(discons, overwrite = TRUE)

# water_df ----
# Dummy data frame for function examples
water_df <- data.frame(
  ph = rep(c(7.9, 8.5, 8.1, 7.8), 3),
  temp = rep(c(20, 25, 19), 4),
  alk = rep(c(50, 80, 100, 200), 3),
  tot_hard = rep(c(50, 75, 100, 30, 400, 110), 2),
  ca = rep(c(13, 20, 26, 8, 104, 28), 2),
  mg = rep(c(4, 6, 8, 3, 34, 9), 2),
  na = rep(c(20, 90), 6),
  k = rep(c(20, 90), 6),
  cl = rep(c(30, 92), 6),
  so4 = rep(c(20, 40, 60, 80), 3),
  free_chlorine = rep(c(0, 1), 6),
  tot_po4 = rep(c(0, 0, 1), 4),
  tds = rep(c(200, 100, NA), 4),
  cond = rep(c(100, 150, NA), 4),
  toc = rep(c(2, 3, 4), 4),
  doc = rep(c(1.8, 2.8, 3.5), 4),
  uv254 = rep(c(.05, .08, .12), 4)
)


usethis::use_data(water_df, overwrite = TRUE)

# edwardscoeff ----
# Data frame of Edwards model coefficients
edwardscoeff <- data.frame(
  ID = "Alum",
  x3 = 4.91, x2 = -74.2, x1 = 284,
  k1 = -0.075, k2 = 0.56,
  b = 0.147
) %>%
  add_row(
    ID = "Ferric",
    x3 = 4.96, x2 = -73.9, x1 = 280,
    k1 = -0.028, k2 = 0.23,
    b = 0.068
  ) %>%
  add_row(
    ID = "Low DOC",
    x3 = 6.44, x2 = -99.2, x1 = 387,
    k1 = -0.053, k2 = 0.54,
    b = 0.107
  ) %>%
  add_row(
    ID = "General Alum",
    x3 = 6.42, x2 = -98.6, x1 = 383,
    k1 = -0.054, k2 = 0.54,
    b = 0.145
  ) %>%
  add_row(
    ID = "General Ferric",
    x3 = 6.42, x2 = -98.6, x1 = 383,
    k1 = -0.054, k2 = 0.54,
    b = 0.092
  )
rownames(edwardscoeff) <- edwardscoeff$ID

usethis::use_data(edwardscoeff, overwrite = TRUE)

# leadsol_constants ----
# Data frame of equilibrium constants for lead and copper solubility
leadsol_constants <- data.frame(
  species_name = c("Lead Hydroxide", "Cerussite", "Hydrocerussite"),
  constant_name = c("K_solid_lead_hydroxide", "K_solid_cerussite", "K_solid_hydrocerussite"),
  log_value = c(13.06, -13.11, -18),
  source = rep("Schock et al. (1996)", 3)
) %>%
  # Solids
  add_row(
    species_name = c("Hydroxypyromorphite", "Hydroxypyromorphite", "Pyromorphite", "Pyromorphite"),
    constant_name = c("K_solid_hydroxypyromorphite_s", "K_solid_hydroxypyromorphite_z", "K_solid_pyromorphite_x", "K_solid_pyromorphite_t"),
    log_value = c(-62.83, -66.77, -80.4, -79.6),
    source = c("Schock et al. (1996)", "Zhu et al. (2015)", "Xie & Giammar (2007)", "Topolska et al. (2016)")
  ) %>%
  add_row(
    species_name = c("Primary Lead Orthophosphate", "Secondary Lead Orthophosphate", "Tertiary Lead Orthophosphate"),
    constant_name = c("K_solid_primary_lead_ortho", "K_solid_secondary_lead_ortho", "K_solid_tertiary_lead_ortho"),
    log_value = c(-48.916, -23.81, -44.4),
    source = c("Powell et al. (2009)", "Schock et al. (1996)", "Powell et al. (2009)")
  ) %>%
  add_row(
    species_name = c("Anglesite", "Laurionite", "Laurionite"),
    constant_name = c("K_solid_anglesite", "K_solid_laurionite_nl", "K_solid_laurionite_l"),
    log_value = c(-7.79, 0.619, 0.29),
    source = c("Schock et al. (1996)", "Nasanen & Lindell (1976)", "Lothenbach et al. (1999)")
  ) %>%
  # Lead-Hydroxide Complexes
  add_row(
    species_name = c("PbOH+", "Pb(OH)2", "Pb(OH)3-", "Pb(OH)4-2"),
    constant_name = c("B_1_OH", "B_2_OH", "B_3_OH", "B_4_OH"),
    log_value = c(-7.22, -16.91, -28.08, -39.72),
    source = rep("Schock et al. (1996)", 4)
  ) %>%
  add_row(
    species_name = c("Pb2OH+3", "Pb3(OH)4+2", "Pb4(OH)4+4", "Pb6(OH)8+4"),
    constant_name = c("B_2_1_OH", "B_3_4_OH", "B_4_4_OH", "B_6_8_OH"),
    log_value = c(-6.36, -23.86, -20.88, -43.62),
    source = rep("Schock et al. (1996)", 4)
  ) %>%
  # Lead-Chloride Complexes
  add_row(
    species_name = c("PbCl+1", "PbCl2", "PbCl3-", "PbCl4-2"),
    constant_name = c("K_1_Cl", "B_2_Cl", "B_3_Cl", "B_4_Cl"),
    log_value = c(1.59, 1.8, 1.71, 1.43),
    source = rep("Schock et al. (1996)", 4)
  ) %>%
  # Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes
  add_row(
    species_name = c("PbSO4", "Pb(SO4)2-2"),
    constant_name = c("K_1_SO4", "B_2_SO4"),
    log_value = c(2.73, 3.5),
    source = rep("Schock et al. (1996)", 2)
  ) %>%
  # Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes
  add_row(
    species_name = c("PbHCO3+", "PbCO3", "Pb(CO3)2-2"),
    constant_name = c("K_1_CO3", "K_2_CO3", "K_3_CO3"),
    log_value = c(12.59, 7.1, 10.33),
    source = rep("Schock et al. (1996)", 3)
  ) %>%
  # Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes
  add_row(
    species_name = c("PbHPO4", "PbH2PO4+"),
    constant_name = c("K_1_PO4", "K_2_PO4"),
    log_value = c(15.41, 21.05),
    source = rep("Schock et al. (1996)", 2)
  )
rownames(leadsol_constants) <- leadsol_constants$constant_name

usethis::use_data(leadsol_constants, overwrite = TRUE)

# dbpcoeffs ----
# Data frame of THM and HAA coefficients
dbpcoeffs <- data.frame(
  # raw/untreated water
  # tthms
  ID = "tthm",
  alias = "total trihalomethanes", group = "tthm",
  treatment = "raw",
  A = 4.121e-2, a = 1.098, b = 0.152, c = 0.068, d = 0.609, e = 1.601, f = 0.263, ph_const = NA
) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform", group = "tthm",
    treatment = "raw",
    A = 6.237e-2, a = 1.617, b = -0.094, c = -0.175, d = 0.607, e = 1.403, f = 0.306, ph_const = NA
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane", group = "tthm",
    treatment = "raw",
    A = 1.445e-3, a = 0.901, b = 0.017, c = 0.733, d = 0.498, e = 1.511, f = 0.199, ph_const = NA
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane", group = "tthm",
    treatment = "raw",
    A = 2.244e-6, a = -0.226, b = 0.108, c = 1.810, d = 0.512, e = 2.212, f = 0.146, ph_const = NA
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform", group = "tthm",
    treatment = "raw",
    A = 1.49e-8, a = -0.983, b = 0.804, c = 1.765, d = 0.754, e = 2.139, f = 0.566, ph_const = NA
  ) %>%
  # haa5 and haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids", group = "haa5",
    treatment = "raw",
    A = 30, a = 0.997, b = 0.278, c = -0.138, d = 0.341, e = -0.799, f = 0.169, ph_const = NA
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids", group = "haa6",
    treatment = "raw",
    A = 9.98, a = 0.935, b = 0.443, c = -0.031, d = 0.387, e = -0.655, f = 0.178, ph_const = NA
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid", group = "haa5",
    treatment = "raw",
    A = 0.45, a = 0.173, b = 0.379, c = 0.029, d = 0.573, e = -0.279, f = 0.009, ph_const = NA
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid", group = "haa5",
    treatment = "raw",
    A = 0.3, a = 1.396, b = 0.379, c = -0.149, d = 0.465, e = 0.200, f = 0.218, ph_const = NA
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid", group = "haa5",
    treatment = "raw",
    A = 92.68, a = 1.152, b = 0.331, c = -0.2299, d = 0.299, e = -1.627, f = 0.180, ph_const = NA
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid", group = "haa5",
    treatment = "raw",
    A = 6.21e-5, a = -0.584, b = 0.754, c = 1.10, d = 0.707, e = 0.604, f = 0.090, ph_const = NA
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid", group = "haa5",
    treatment = "raw",
    A = 3.69e-5, a = -1.087, b = 0.673, c = 2.052, d = 0.380, e = -0.001, f = 0.095, ph_const = NA
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid", group = "haa6",
    treatment = "raw",
    A = 5.51e-3, a = 0.463, b = 0.522, c = 0.667, d = 0.379, e = 0.581, f = 0.220, ph_const = NA
  ) %>%
  # coagulated/softened water
  # tthms
  add_row(
    ID = "tthm",
    alias = "total trihalomethanes", group = "tthm",
    treatment = "coag",
    A = 23.9, a = 0.403, b = 0.225, c = 0.141, d = 1.1560, e = 1.0263, f = 0.264, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform", group = "tthm",
    treatment = "coag",
    A = 266, a = 0.403, b = 0.424, c = -0.679, d = 1.1322, e = 1.0179, f = 0.333, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane", group = "tthm",
    treatment = "coag",
    A = 1.68, a = 0.260, b = 0.114, c = 0.462, d = 1.0260, e = 1.0977, f = 0.196, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane", group = "tthm",
    treatment = "coag",
    A = 8.0e-3, a = -0.056, b = -0.157, c = 1.425, d = 1.0212, e = 1.1271, f = 0.148, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform", group = "tthm",
    treatment = "coag",
    A = 4.4e-5, a = -0.300, b = -0.221, c = 2.134, d = 1.0374, e = 1.3907, f = 0.143, ph_const = 7.5
  ) %>%
  # haa5 & haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids", group = "haa5",
    treatment = "coag",
    A = 30.7, a = 0.302, b = 0.541, c = -0.012, d = 0.932, e = 1.021, f = 0.161, ph_const = 7.5
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids", group = "haa6",
    treatment = "coag",
    A = 41.6, a = 0.328, b = 0.585, c = -0.121, d = 1.022, e = 0.9216, f = 0.150, ph_const = 7.5
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid", group = "haa5",
    treatment = "coag",
    A = 4.58, a = -0.090, b = 0.662, c = -0.224, d = 1.024, e = 1.042, f = 0.043, ph_const = 7.5
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid", group = "haa5",
    treatment = "coag",
    A = 60.4, a = 0.397, b = 0.665, c = -0.558, d = 1.017, e = 1.034, f = 0.222, ph_const = 7.5
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid", group = "haa5",
    treatment = "coag",
    A = 52.6, a = 0.403, b = 0.749, c = -0.416, d = 1.014, e = 0.8739, f = 0.163, ph_const = 7.5
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid", group = "haa5",
    treatment = "coag",
    A = 2.06e-2, a = 0.358, b = -0.101, c = 0.812, d = 1.162, e = 0.6526, f = 0.043, ph_const = 7.5
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid", group = "haa5",
    treatment = "coag",
    A = 9.42e-5, a = 0.0590, b = 0.182, c = 2.109, d = 1.007, e = 1.210, f = 0.070, ph_const = 7.5
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid", group = "haa6",
    treatment = "coag",
    A = 3.23e-1, a = 0.153, b = 0.257, c = 0.586, d = 1.042, e = 1.181, f = 0.201, ph_const = 7.5
  ) %>%
  # haa9
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid", group = "haa9",
    treatment = "coag",
    A = 3.70e-3, a = -0.0162, b = -0.170, c = 0.972, d = 1.054, e = 0.839, f = 0.685, ph_const = 8
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid", group = "haa9",
    treatment = "coag",
    A = 5.89e-1, a = 0.230, b = 0.140, c = 0.301, d = 1.022, e = 0.700, f = 0.422, ph_const = 8
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid", group = "haa9",
    treatment = "coag",
    A = 5.59e-6, a = 0.0657, b = -2.51, c = 2.32, d = 1.059, e = 0.555, f = 1.26, ph_const = 8
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids", group = "haa9",
    treatment = "coag",
    A = 10.78, a = 0.25, b = 0.5, c = 0.054, d = 1.015, e = 0.894, f = 0.348, ph_const = 8
  ) %>%
  # gac treated water
  # tthms
  add_row(
    ID = "tthm",
    alias = "total trihalomethanes", group = "tthm",
    treatment = "gac",
    A = 17.7, a = 0.475, b = 0.173, c = 0.246, d = 1.316, e = 1.036, f = 0.366, ph_const = 8
  ) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform", group = "tthm",
    treatment = "gac",
    A = 101, a = 0.615, b = 0.699, c = -0.468, d = 1.099, e = 1.035, f = 0.336, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane", group = "tthm",
    treatment = "gac",
    A = 7.57, a = 0.443, b = 0.563, c = 0.0739, d = 1.355, e = 1.03, f = 0.281, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane", group = "tthm",
    treatment = "gac",
    A = 3.99, a = 0.535, b = 0.125, c = 0.365, d = 1.436, e = 1.037, f = 0.322, ph_const = 7.5
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform", group = "tthm",
    treatment = "gac",
    A = 1.47e-1, a = 0.408, b = -0.115, c = 0.961, d = 1.438, e = 1.048, f = 0.324, ph_const = 7.5
  ) %>%
  # haa5 & haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids", group = "haa5",
    treatment = "gac",
    A = 41.2, a = 0.498, b = 0.388, c = -0.156, d = 0.867, e = 1.021, f = 0.263, ph_const = 8
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids", group = "haa6",
    treatment = "gac",
    A = 37.8, a = 0.511, b = 0.374, c = -0.079, d = 0.913, e = 1.022, f = 0.280, ph_const = 8
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid", group = "haa5",
    treatment = "gac",
    A = 1.31e-1, a = 0.202, b = 0.275, c = -0.958, d = 0.124, e = 1.036, f = 0.923, ph_const = 8
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid", group = "haa5",
    treatment = "gac",
    A = 38.4, a = 0.503, b = 0.421, c = -0.393, d = 0.867, e = 1.019, f = 0.293, ph_const = 8
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid", group = "haa5",
    treatment = "gac",
    A = 47.8, a = 0.627, b = 0.729, c = -0.425, d = 0.602, e = 1.011, f = 0.174, ph_const = 8
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid", group = "haa5",
    treatment = "gac",
    A = 3.0e-1, a = 0.093, b = 0.964, c = -0.408, d = 0.134, e = 1.054, f = 0.554, ph_const = 8
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid", group = "haa5",
    treatment = "gac",
    A = 3.96e-1, a = 0.509, b = -0.251, c = 0.689, d = 1.302, e = 1.019, f = 0.310, ph_const = 8
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid", group = "haa6",
    treatment = "gac",
    A = 3.89, a = 0.560, b = 0.260, c = 0.117, d = 1.077, e = 1.018, f = 0.334, ph_const = 8
  ) %>%
  # haa9
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid", group = "haa9",
    treatment = "gac",
    A = 5.56e-2, a = 0.831, b = -0.296, c = 0.782, d = 0.477, e = 1.016, f = 0.886, ph_const = 8
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid", group = "haa9",
    treatment = "gac",
    A = 2.19, a = 0.665, b = 0.270, c = 0.221, d = 0.587, e = 0.985, f = 0.379, ph_const = 8
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid", group = "haa9",
    treatment = "gac",
    A = 1.65e-4, a = 1.59, b = -2.19, c = 2.06, d = 0.575, e = 0.983, f = 1.78, ph_const = 8
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids", group = "haa9",
    treatment = "gac",
    A = 20.6, a = 0.509, b = 0.253, c = 0.053, d = 0.823, e = 1.019, f = 0.425, ph_const = 8
  )
rownames(dbpcoeffs) <- dbpcoeffs$ID

usethis::use_data(dbpcoeffs, overwrite = TRUE)

# Data frame of DBP conversion factors for chloramines
chloramine_conv <- data.frame(
  # tthms
  ID = "tthm",
  alias = "total trihalomethanes",
  percent = 0.20
) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform",
    percent = 0.20
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane",
    percent = 0.20
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane",
    percent = 0.20
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform",
    percent = 0.20
  ) %>%
  # haa5 and haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids",
    percent = 0.20
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids",
    percent = 0.20
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid",
    percent = 0.50
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid",
    percent = 0.05
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid",
    percent = 0.30
  ) %>%
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid",
    percent = 0.20
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids",
    percent = 0.20
  )
rownames(chloramine_conv) <- chloramine_conv$ID

usethis::use_data(chloramine_conv, overwrite = TRUE)


# Data frame of DBP correction factors based on location from testing with ICR data
# No correction factors developed for CDBAA, BDCAA, TBAA, and HAA9 since ICR plant data was used to develop these equations

dbp_correction <- data.frame(
  # tthms
  ID = "tthm",
  alias = "total trihalomethanes",
  plant = 1,
  ds = 1
) %>%
  add_row(
    ID = "chcl3",
    alias = "chloroform",
    plant = 1,
    ds = 1.1
  ) %>%
  add_row(
    ID = "chcl2br",
    alias = "dichlorobromomethane",
    plant = 0.92,
    ds = 1
  ) %>%
  add_row(
    ID = "chbr2cl",
    alias = "dibromochloromethane",
    plant = 0.65,
    ds = 0.46
  ) %>%
  add_row(
    ID = "chbr3",
    alias = "bromoform",
    plant = 1,
    ds = 1
  ) %>%
  # haa5 and haa6
  add_row(
    ID = "haa5",
    alias = "Five haloacetic acids",
    plant = 1.1,
    ds = 1.1
  ) %>%
  add_row(
    ID = "haa6",
    alias = "Six haloacetic acids",
    plant = 1.1,
    ds = 1.1
  ) %>%
  add_row(
    ID = "mcaa",
    alias = "monochloroacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "dcaa",
    alias = "dichloroacetic acid",
    plant = 0.72,
    ds = 1.1
  ) %>%
  add_row(
    ID = "tcaa",
    alias = "trichloroacetic acid",
    plant = 1.3,
    ds = 1.3
  ) %>%
  add_row(
    ID = "mbaa",
    alias = "monobromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "dbaa",
    alias = "dibromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "bcaa",
    alias = "bromochloroacetic acid",
    plant = 0.86,
    ds = 2
  ) %>%
  add_row(
    ID = "cdbaa",
    alias = "chlorodibromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "dcbaa",
    alias = "dichlorobromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "tbaa",
    alias = "tribromoacetic acid",
    plant = 1,
    ds = 1
  ) %>%
  add_row(
    ID = "haa9",
    alias = "Nine haloacetic acids",
    plant = 1,
    ds = 1
  )
rownames(dbp_correction) <- dbp_correction$ID

usethis::use_data(dbp_correction, overwrite = TRUE)

# bromatecoeffs ----
# Dataframe of bromate formation coefficients
bromatecoeffs <- data.frame(
  model = rep("Ozekin", 2),
  ammonia = c(F, T),
  A = c(1.55E-6, 1.63E-6),
  a = c(0.73, 0.73),
  b = c(-1.26, -1.3),
  c = c(0, 0), # No UV in this model
  d = c(5.82, 5.79),
  e = c(0, 0), # No alk in this model
  f = c(1.57, 1.59),
  g = c(0.28, 0.27),
  h = c(0, -0.033),
  i = c(0, 0), # no temp in this model
  I = c(1, 1) # no temp in this model
) %>%
  add_row(
    model = rep("Sohn", 2),
    ammonia = c(F, T),
    A = c(1.19E-7, 8.71E-8),
    a = c(0.96, 0.944),
    b = c(0, 0), # No DOC in this model
    c = c(-0.623, -0.593),
    d = c(5.68, 5.81),
    e = c(-0.201, -0.167),
    f = c(1.307, 1.279),
    g = c(0.336, 0.337),
    h = c(0, -0.051),
    i = c(0, 0), # temp in exponent
    I = c(1.035, 1.035)
  ) %>%
  add_row(
    model = "Song",
    ammonia = T, # Only applies when ammonia > .005
    A = 7.76E-7,
    a = 0.88,
    b = -1.88,
    c = 0, # No UV in this model
    d = 5.11,
    e = 0.18,
    f = 1.42,
    g = 0.27,
    h = -0.18,
    i = 0, # no temp in this model
    I = 1 # no temp in this model
  ) %>%
  add_row(
    model = "Galey",
    ammonia = F, # Only applies when ammonia = 0
    A = 5.41E-5,
    a = .04,
    b = -1.08,
    c = 0, # No UV in this model
    d = 4.7,
    e = 0, # No alk in this model
    f = 1.12,
    g = 0.304,
    h = 0,
    i = 0.58,
    I = 1 # temp not in exponent
  ) %>%
  add_row(
    model = "Siddiqui",
    ammonia = F, # Only applies when ammonia = 0
    A = 1.5E-3,
    a = 0.61,
    b = 0.61,
    c = 0, # No UV in this model
    d = 2.26,
    e = 0, # No alk in this model
    f = 0.64,
    g = 0, # No time in this model
    h = 0,
    i = 2.03,
    I = 1 # temp not in exponent
  )
usethis::use_data(bromatecoeffs, overwrite = TRUE)

# cl2coeffs -----
# Data frame of Cl2 decay coefficients
cl2coeffs <- tibble(
  treatment = c("chlorine_raw", "chlorine_coag", "chloramine"),
  a = c(-0.8147, -0.8404, -0.99),
  b = c(-2.2808, -0.404, -0.015),
  c = c(-1.2971, -0.9108, NA)
)

usethis::use_data(cl2coeffs, overwrite = TRUE)

# pactoccoeffs -----
# Data frame of PAC TOC removal coefficients
pactoccoeffs <- tibble(
  pactype = c("bituminous", "lignite", "wood"),
  A = c(.1561, .4078, .3653),
  a = c(.9114, .8516, .8692),
  b = c(.0263, .0225, .0151),
  c = c(.002, .002, .0025)
)

usethis::use_data(pactoccoeffs, overwrite = TRUE)

# toc_compliance_table -----
# Data frame of PAC TOC removal coefficients

toc_compliance_table <- data.frame(
  toc_min = c(2, 2, 2, 4, 4, 4, 8, 8, 8),
  toc_max = c(4, 4, 4, 8, 8, 8, Inf, Inf, Inf),
  alk_min = c(0, 60, 120, 0, 60, 120, 0, 60, 120),
  alk_max = c(60, 120, Inf, 60, 120, Inf, 60, 120, Inf),
  required_compliance = c(35, 25, 15, 45, 35, 25, 50, 40, 30)
)

usethis::use_data(toc_compliance_table, overwrite = TRUE)

# vlog_removalcts -----
vlog_removalcts <- data.frame(
    ph_range = "6-9",
    temp_value = 0.5,
    ct_range = "6-9",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 0.5,
    ct_range = "9-12",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 0.5,
    ct_range = "12",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 0.5,
    ct_range = "45-66",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 0.5,
    ct_range = "66-90",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 0.5,
    ct_range = "90",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 5,
    ct_range = "4-6",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 5,
    ct_range = "6-8",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 5,
    ct_range = "8",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 5,
    ct_range = "30-44",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 5,
    ct_range = "44-60",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 5,
    ct_range = "60",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 10,
    ct_range = "3-4",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 10,
    ct_range = "4-6",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 10,
    ct_range = "6",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 10,
    ct_range = "22-33",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 10,
    ct_range = "33-45",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 10,
    ct_range = "45",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 15,
    ct_range = "2-3",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 15,
    ct_range = "3-4",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 15,
    ct_range = "4",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 15,
    ct_range = "15-22",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 15,
    ct_range = "22-30",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 15,
    ct_range = "30",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 20,
    ct_range = "1-2",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 20,
    ct_range = "2-3",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 20,
    ct_range = "3",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 20,
    ct_range = "11-16",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 20,
    ct_range = "16-22",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 20,
    ct_range = "22",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 25,
    ct_range = "1",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 25,
    ct_range = "1-2",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "6-9",
    temp_value = 25,
    ct_range = "2",
    vlog_removal = 4.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 25,
    ct_range = "7-11",
    vlog_removal = 2.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 25,
    ct_range = "11-15",
    vlog_removal = 3.0
  ) %>%
  add_row(
    ph_range = "10",
    temp_value = 25,
    ct_range = "15",
    vlog_removal = 4.0
  )

usethis::use_data(vlog_removalcts, overwrite = TRUE)
