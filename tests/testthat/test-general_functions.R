# Define water -----
test_that("Define water outputs water class.", {
  # Disregard warnings, they are expected here.
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
    water2 <- define_water(temp = 25, tot_hard = 50)
    water3 <- define_water(ph = 7, temp = 25, alk = 100)
  })
  expect_s4_class(water1, "water")
  expect_s4_class(water2, "water")
  expect_s4_class(water3, "water")
})

test_that("Define water calculates correct carbonate balance.", {
  suppressWarnings( {
    water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
  })

  expect_equal(water1@ph, 7)
  expect_equal(round(water1@tot_co3, 5), 0.00244)
  expect_equal(round(water1@hco3, 3), 0.002)

})

test_that("Define water gives missing value warnings.", {
  expect_warning(define_water(alk = 100, temp = 20, tot_hard = 50, ca_hard = 50, na = 10, k = 10, cl = 10, so4 = 10, tds = 100),
    "Missing.+pH.+")
  expect_warning(define_water(ph = 7, temp = 20, tot_hard = 50, ca_hard = 50, na = 10, k = 10, cl = 10, so4 = 10, tds = 100),
    "Missing.+alkalinity.+")
  expect_warning(define_water(ph = 7, alk = 100, temp = 20, tot_hard = 50, ca_hard = 50, tds = 100),
                 "Missing.+cations.+")
  expect_warning(define_water(ph = 7, alk = 100, temp = 20, tot_hard = 50, ca_hard = 50, na = 0, k = 0, cl = 0, so4 = 0),
                 "Ions missing.+")

  expect_warning(define_water(ph = 7, alk = 100, temp = 20, tot_hard = 50, ca_hard = 50, na = 10, k = 10, cl = 10, so4 = 10, toc = 5, uv254 = .1),
                 "Missing.+DOC+")
  expect_warning(define_water(ph = 7, alk = 100, temp = 20, tot_hard = 50, ca_hard = 50, na = 10, k = 10, cl = 10, so4 = 10),
                 "No organic.+")

})

test_that("Define water doesn't output carbonate when pH or alk aren't provided.", {
  # Disregard warnings, they are expected here.
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25)
    water2 <- define_water(temp = 25, alk = 50)
  })

  expect_equal(water1@tot_co3, NA_real_)
  expect_equal(water2@tot_co3, NA_real_)
  expect_equal(water1@alk, NA_real_)
  expect_equal(water2@ph, NA_real_)

})

test_that("define_water handles organics inputs correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 7, doc = 3.5, toc = 3.4))

  expect_equal(water1@doc, 3.325)
  expect_equal(round(water2@toc, 3), 3.684)
  expect_equal(water3@uv254, NA_real_)
})


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
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  expect_match(summarize_wq(water1), ".+pH.+7.+Alkalinity.+100.+")
  expect_s3_class(summarize_wq(water1), "knitr_kable")
})

# Plot Ions ----

test_that("Plot ions creates a ggplot object that can be printed.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  expect_s3_class(plot_ions(water1), "ggplot")
  expect_no_error(plot_ions(water1))
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


# Balance Ions ----

test_that("Balance ions doesn't alter carbonate system.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- balance_ions(water1)
  expect_equal(water1@ph, water2@ph)
  expect_equal(water1@tot_co3, water2@tot_co3)
  expect_equal(water1@hco3, water2@hco3)
})

test_that("Balance ions doesn't alter Ca, Mg, PO4, or OCl.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- balance_ions(water1)
  expect_equal(water1@ca, water2@ca)
  expect_equal(water1@mg, water2@mg)
  expect_equal(water1@tot_ocl, water2@tot_ocl)
  expect_equal(water1@tot_po4, water2@tot_po4)
})

test_that("Balance ions doesn't alter organics.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- balance_ions(water1)
  expect_equal(water1@toc, water2@toc)
  expect_equal(water1@doc, water2@doc)
  expect_equal(water1@uv254, water2@uv254)
})

test_that("Balance ions results in neutral charge.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- balance_ions(water1)

  expect_equal(water2@na + water2@ca * 2 + water2@mg * 2 + water2@k -
                 (water2@cl + 2 * water2@so4 + water2@hco3 + 2 * water2@co3 + water2@h2po4 + 2 * water2@hpo4 + 3 * water2@po4) +
    water2@h - water2@oh - water2@ocl, 0)

  water3 <- define_water(ph = 7, temp = 25, alk = 100, 10, 10, 10, 10, 10, 10, tot_ocl = 2, tot_po4 = 1, toc = 5, doc = 4.8, uv254 = .1)
  water4 <- balance_ions(water3)


  expect_equal(water4@na + water4@ca * 2 + water4@mg * 2 + water4@k -
                 (water4@cl + 2 * water4@so4 + water4@hco3 + 2 * water4@co3 + water4@h2po4 + 2 * water4@hpo4 + 3 * water4@po4) +
                 water4@h - water4@oh - water4@ocl, 0)

})


# Calculate alpha carbonate ----

test_that("Carbonate alpha calculations work.", {
  k = data.frame("k1co3" = discons$k[discons$ID == "k1co3"],
                "k2co3" = discons$k[discons$ID == "k2co3"])
  expect_equal(round(calculate_alpha1_carbonate(10^-7, k), 2), 0.82)
  expect_equal(round(calculate_alpha2_carbonate(10^-7, k), 5), 0.00038)
})

# Calculate alpha phosphate ----
test_that("Phosphate alpha calculations work.", {
  k = data.frame("k1po4" = discons$k[discons$ID == "k1po4"],
                 "k2po4" = discons$k[discons$ID == "k2po4"],
                 "k3po4" = discons$k[discons$ID == "k3po4"])
  expect_equal(round(calculate_alpha1_phosphate(10^-7, k), 2), 0.61)
  expect_equal(round(calculate_alpha2_phosphate(10^-7, k), 2), 0.39)
  expect_equal(signif(calculate_alpha3_phosphate(10^-7, k), 2), 1.7E-6)
})

# Calculate temperature correction ----
test_that("K temp correction returns a value close to K.", {
  k1po4 = discons$k[discons$ID == "k1po4"]
  k1po4_h = discons$deltah[discons$ID == "k1po4"]
  lowtemp <- pK_temp_adjust(k1po4_h, k1po4, 5)
  k2co3 = discons$k[discons$ID == "k2co3"]
  k2co3_h = discons$deltah[discons$ID == "k2co3"]
  hitemp <- pK_temp_adjust(k2co3_h, k2co3, 30)

  expect_true(lowtemp / k1po4 < 1.3 && lowtemp / k1po4 > 1)
  expect_true(hitemp / k2co3 < 1.2 && hitemp / k2co3 > 1)

})

# Ionic Strength ----

test_that("Ionic strength calc in define water works.", {
  water <- define_water(7, 25, 100, 100, 70, 10, 10, 10, 10)

  is_calced <- 0.5 * ((water@na + water@cl + water@k + water@hco3 + water@h2po4 + water@h + water@oh + water@tot_ocl) * 1^2 +
           (water@ca + water@mg + water@so4 + water@co3 + water@hpo4) * 2^2 +
           (water@po4) * 3^2)
  expect_equal(water@is, is_calced)

})

test_that("Ionic strength correlation in define water works.", {
  water <- define_water(7, 25, 100, 100, 70, 10, 10, 10, 10, tds = 200)
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
