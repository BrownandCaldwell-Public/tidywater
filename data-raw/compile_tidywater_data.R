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
