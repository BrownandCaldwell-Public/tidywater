# Convert units ----
test_that("Unit conversion between mg/L or mg/L CaCO3 and M works.", {
  hcl_mg <- 10
  expect_equal(convert_units(hcl_mg, "hcl"), hcl_mg / mweights$hcl / 1000)
  alum_mg <- 30
  expect_equal(convert_units(alum_mg, "alum"), alum_mg / mweights$alum / 1000)
  naoh_m <- .1
  expect_equal(convert_units(naoh_m, "naoh", startunit = "M", endunit = "mg/L"), naoh_m * mweights$naoh * 1000)
  ca_mgcaco3 <- 50
  expect_equal(convert_units(ca_mgcaco3, "ca", startunit = "mg/L CaCO3", endunit = "M"), ca_mgcaco3 / mweights$caco3 / 1000)
  ca_mol <- .002
  expect_equal(convert_units(ca_mol, "ca", startunit = "M", endunit = "mg/L CaCO3"), ca_mol * mweights$caco3 * 1000)
})

test_that("Unit conversion between mg/L and mg/L CaCO3 works.", {
  ca_mg <- 20
  expect_equal(convert_units(ca_mg, "ca", startunit = "mg/L", endunit = "mg/L CaCO3"), ca_mg / mweights$ca * mweights$caco3)
  hco3_caco3 <- 80
  expect_equal(convert_units(hco3_caco3, "hco3", startunit = "mg/L CaCO3", endunit = "mg/L"), hco3_caco3 * mweights$hco3 / mweights$caco3)
})

test_that("Unit conversion to same units works.", {
  expect_equal(convert_units(10, "na", startunit = "mg/L", endunit = "mg/L"), 10)
  expect_equal(convert_units(.002, "caco3", startunit = "M", endunit = "M"), .002)
  expect_equal(convert_units(1000, "mgoh2", startunit = "mg/L", endunit = "g/L"), 1)
  expect_equal(convert_units(.002, "h2so4", startunit = "M", endunit = "mM"), 2)
})

test_that("Unit conversion between M and eq/L works.", {
  expect_equal(convert_units(.002, "ca", startunit = "M", endunit = "eq/L"), .004)
  expect_equal(convert_units(.004, "caco3", startunit = "eq/L", endunit = "M"), .002)
})

test_that("Unit conversion between mg/L or mg/L CaCO3 to eq/L works.", {
  hcl_mg <- 10
  expect_equal(convert_units(hcl_mg, "hcl", endunit = "eq/L"), hcl_mg / mweights$hcl / 1000)
  al_mg <- 10
  expect_equal(convert_units(al_mg, "al", startunit = "mg/L", endunit = "eq/L"), al_mg / mweights$al / 1000 * 3)
  na_eq <- .002
  expect_equal(convert_units(na_eq, "na", startunit = "eq/L", endunit = "mg/L"), na_eq * mweights$na * 1000)
  ca_mgcaco3 <- 50
  expect_equal(convert_units(ca_mgcaco3, "ca", startunit = "mg/L CaCO3", endunit = "eq/L"), ca_mgcaco3 / mweights$caco3 / 1000 * 2)
  ca_eq <- .002
  expect_equal(convert_units(ca_eq, "ca", startunit = "eq/L", endunit = "mg/L CaCO3"), ca_eq * mweights$caco3 * 1000 / 2)
})

# Summarize WQ ----

test_that("Summarize WQ returns a kable and prints pH and Alkalinity.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  expect_match(summarise_wq(water1), ".+pH.+7.+Alkalinity.+100.+")
  expect_s3_class(summarise_wq(water1), "knitr_kable")
})

# Plot Ions ----

test_that("Plot ions creates a ggplot object that can be printed.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  expect_s3_class(plot_ions(water1), "ggplot")
  expect_no_error(plot_ions(water1))
})

# Plot Lead ----
test_that("Plot lead returns expected errors and warnings.", {
  df1 <- data.frame(dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
                    temp = 25,
                    tds = 200)
  df2 <- data.frame(ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
                    temp = 25,
                    tds = 200)
  df3 <- data.frame(ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
                    dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
                    tds = 200)
  df4 <- data.frame(ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
                    dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
                    temp = 25,
                    tds = 200,
                    alk = 100)
  df5 <- data.frame(ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
                    dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
                    temp = 25,
                    tds = rep(c(150, 200), 5))
  expect_error(plot_lead(df1))
  expect_error(plot_lead(df2))
  expect_error(plot_lead(df3))
  expect_warning(plot_lead(df4))
  expect_warning(plot_lead(df5))
})

test_that("Plot lead creates a ggplot object that can be printed.", {
  historical <- data.frame(pH = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
                 DIC = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34), 
                 temperature = 25,
                 `total_dissolved_solids` = 200)
  expect_s3_class(plot_lead(historical, ph_range = c(7, 10), dic_range = c(10,100)), "ggplot")
  expect_no_error(plot_lead(historical))
})


# Calculate Hardness ----

test_that("Total hardness calculation works.", {
  expect_equal(calculate_hardness(20, 2), 20 / mweights$ca * mweights$caco3 + 2 / mweights$mg * mweights$caco3)
  expect_equal(calculate_hardness(.002, .001, startunit = "M"), .002 * mweights$caco3 * 1000 + .001 * mweights$caco3 * 1000)
})

test_that("Calcium hardness calculation works.", {
  expect_equal(calculate_hardness(20, 2, type = "ca"), 20 / mweights$ca * mweights$caco3)
  expect_equal(calculate_hardness(.002, 0, startunit = "M", type = "ca"), .002 * mweights$caco3 * 1000)
})

# Calculate alpha carbonate ----

test_that("Carbonate alpha calculations work.", {
  k <- data.frame(
    "k1co3" = discons$k[discons$ID == "k1co3"],
    "k2co3" = discons$k[discons$ID == "k2co3"]
  )
  expect_equal(round(calculate_alpha1_carbonate(10^-7, k), 2), 0.82)
  expect_equal(round(calculate_alpha2_carbonate(10^-7, k), 5), 0.00038)
})

# Calculate alpha phosphate ----
test_that("Phosphate alpha calculations work.", {
  k <- data.frame(
    "k1po4" = discons$k[discons$ID == "k1po4"],
    "k2po4" = discons$k[discons$ID == "k2po4"],
    "k3po4" = discons$k[discons$ID == "k3po4"]
  )
  expect_equal(round(calculate_alpha1_phosphate(10^-7, k), 2), 0.61)
  expect_equal(round(calculate_alpha2_phosphate(10^-7, k), 2), 0.39)
  expect_equal(signif(calculate_alpha3_phosphate(10^-7, k), 2), 1.7E-6)
})

# Calculate temperature correction ----
test_that("K temp correction returns a value close to K.", {
  k1po4 <- discons$k[discons$ID == "k1po4"]
  k1po4_h <- discons$deltah[discons$ID == "k1po4"]
  lowtemp <- K_temp_adjust(k1po4_h, k1po4, 5)
  k2co3 <- discons$k[discons$ID == "k2co3"]
  k2co3_h <- discons$deltah[discons$ID == "k2co3"]
  hitemp <- K_temp_adjust(k2co3_h, k2co3, 30)

  expect_true(lowtemp / k1po4 < 1.3 && lowtemp / k1po4 > 1)
  expect_true(hitemp / k2co3 < 1.2 && hitemp / k2co3 > 1)
})

# Ionic Strength ----

test_that("Ionic strength calc in define water works.", {
  water <- define_water(7, 25, 100, 70, 10, 10, 10, 10, 10, 10, doc = 5, toc = 5, uv254 = .1, br = 50)

  is_calced <- 0.5 * ((water@na + water@cl + water@k + water@hco3 + water@h2po4 + water@h + water@oh + water@ocl) * 1^2 +
    (water@ca + water@mg + water@so4 + water@co3 + water@hpo4) * 2^2 +
    (water@po4) * 3^2)
  expect_equal(signif(water@is, 3), signif(is_calced, 3))
})

test_that("Ionic strength correlation in define water works.", {
  water <- define_water(7, 25, 100, 70, 10, 10, 10, 10, 10, tds = 200, doc = 5, toc = 5, uv254 = .1, br = 50)
  is_calced <- 2.5 * 10^-5 * water@tds
  expect_equal(water@is, is_calced)

  water <- suppressWarnings(define_water(7, 25, 100, cond = 200))
  is_calced <- 1.6 * 10^-5 * water@cond
  expect_equal(water@is, is_calced)
})

# Activity coefficients ----

test_that("Activity coefficient calculation works.", {
  expect_equal(round(calculate_activity(1, .001, 25), 2), .97)
  expect_equal(round(calculate_activity(2, .01, 25), 2), .66)
})
