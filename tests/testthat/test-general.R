# Convert units ----
test_that("Unit conversion between mg/L or mg/L CaCO3 and M works.", {
  hcl_mg <- 10
  expect_equal(convert_units(hcl_mg, "hcl"), hcl_mg / mweights$hcl / 1000)
  alum_mg <- 30
  expect_equal(convert_units(alum_mg, "alum"), alum_mg / mweights$alum / 1000)
  naoh_m <- .1
  expect_equal(convert_units(naoh_m, "naoh", startunit = "M", endunit = "mg/L"), naoh_m * mweights$naoh * 1000)
  ca_mgcaco3 <- 50
  expect_equal(
    convert_units(ca_mgcaco3, "ca", startunit = "mg/L CaCO3", endunit = "M"),
    ca_mgcaco3 / mweights$caco3 / 1000
  )
  ca_mol <- .002
  expect_equal(convert_units(ca_mol, "ca", startunit = "M", endunit = "mg/L CaCO3"), ca_mol * mweights$caco3 * 1000)
})

test_that("Unit conversion between mg/L and mg/L CaCO3 works.", {
  ca_mg <- 20
  expect_equal(
    convert_units(ca_mg, "ca", startunit = "mg/L", endunit = "mg/L CaCO3"),
    ca_mg / mweights$ca * mweights$caco3
  )
  hco3_caco3 <- 80
  expect_equal(
    convert_units(hco3_caco3, "hco3", startunit = "mg/L CaCO3", endunit = "mg/L"),
    hco3_caco3 * mweights$hco3 / mweights$caco3 * 2
  )
  soda_ash <- 10
  expect_equal(
    convert_units(soda_ash, "na2co3", startunit = "mg/L CaCO3", endunit = "mg/L"),
    soda_ash * mweights$na2co3 / mweights$caco3
  )
  naoh_mg <- 10
  expect_equal(
    convert_units(naoh_mg, "naoh", startunit = "mg/L", endunit = "mg/L CaCO3"),
    naoh_mg / mweights$naoh * mweights$caco3 / 2
  )

  na3po4 <- 10
  expect_equal(
    convert_units(na3po4, "na3po4", startunit = "mg/L", endunit = "mg/L CaCO3"),
    na3po4 / mweights$na3po4 * mweights$caco3 * 3 / 2
  )

})

test_that("Unit conversion to same units works.", {
  expect_equal(convert_units(10, "na", startunit = "mg/L", endunit = "mg/L"), 10)
  expect_equal(convert_units(.002, "caco3", startunit = "M", endunit = "M"), .002)
  expect_equal(convert_units(1000, "mgoh2", startunit = "mg/L", endunit = "g/L"), 1)
  expect_equal(convert_units(.002, "h2so4", startunit = "M", endunit = "mM"), 2)
  expect_equal(convert_units(5, "xyz", startunit = "mM", endunit = "M"), .005)
  expect_equal(convert_units(5, startunit = "mM", endunit = "M"), .005)
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
  expect_equal(
    convert_units(ca_mgcaco3, "ca", startunit = "mg/L CaCO3", endunit = "eq/L"),
    ca_mgcaco3 / mweights$caco3 / 1000 * 2
  )
  ca_eq <- .002
  expect_equal(
    convert_units(ca_eq, "ca", startunit = "eq/L", endunit = "mg/L CaCO3"),
    ca_eq * mweights$caco3 * 1000 / 2
  )

  expect_equal(
    convert_units(10, "na", startunit = "mg/L", endunit = "mg/L CaCO3"),
    10 / mweights$na * mweights$caco3 / 2
  )
})

test_that("Flowrate converter works", {

  MGD <- 10
  KGD <- 100

  # volumetric to volumetric
  MGH_eq1 <- round(10/24,1)
  MGH1 <- round(convert_flow(MGD, startunit = "MGD", endunit = " MGH"),1) # also soft test aliasing
  expect_equal(MGH_eq1, MGH1)

  # mass to mass
  lbs_day_eq1 <- round(KGD * 2.20462,0)
  lbsday1 <- round(convert_flow(KGD, startunit = "kg/day", endunit = "lb/day"),0)
  expect_equal(lbs_day_eq1, lbsday1)

  # volumetric to mass
  lbs_day_eq2 <- signif(MGD * 8.34 * 10^6,3)
  lbsday2 <- signif(convert_flow(MGD, startunit = "MGD", endunit = "lb/day", sg = 1),3)
  expect_equal(lbs_day_eq2, lbsday2)

  # mass to volumetric
  MGD_eq2 <- signif(KGD / 3.785 / 10^6,3)
  MGD2 <- signif(convert_flow(KGD, startunit = "kg/day", endunit = "MGD", sg = 1),3)
  expect_equal(MGD_eq2, MGD2)
  
})
  
test_that("Aliasing works", {
  # correct conversion
  hcl_mg <- 10
  hcl_caco3 <- convert_units(hcl_mg, "hcl", endunit = "mg/L CaCO3")

  # start unit 
  expect_equal( hcl_caco3, convert_units(hcl_mg, "hcl", startunit = "MG  / l ", endunit = "mg/L CaCO3"))
  expect_equal( hcl_caco3, convert_units(hcl_mg, "hcl", startunit = "MG  / l ", endunit = "mg/L as CaCO3"))
  expect_equal( hcl_caco3, convert_units(hcl_mg, "hcl", startunit = "MG  / l ", endunit = "mg/lcaco3"))
  # end unit
  expect_equal(hcl_caco3, convert_units(hcl_mg, "hcl", startunit = "mg/L", endunit = "mG/L caco3"))
  # typos 
  expect_warning(convert_units(hcl_caco3, "hcl", startunit = "mj/L", endunit = "mg/L caco"))
  expect_error((convert_units(hcl_caco3, "hcl", startunit = "mj/L", endunit = "moles")))

  # formula matching
  expect_equal(hcl_caco3, convert_units(hcl_mg, "H C L ", startunit = "mg/L", endunit = "mg/L CaCO3"))

  # warning on formula typo
  expect_warning(convert_units(hcl_mg, "hxl", startunit = "mg/L", endunit = "mg/L CaCO3"))
})

test_that("Fuzzy matching warns about the specific argument that was corrected", {
  expect_warning(
    convert_units(10, "ca", startunit = "mg/Lx", endunit = "M"),
    "Check startunit"
  )
  expect_warning(
    convert_units(10, "ca", startunit = "mg/L", endunit = "nMM"),
    "Check endunit"
  )
  expect_warning(
    convert_units(10, "caa", startunit = "mg/L", endunit = "M"),
    "Check formula"
  )
})

test_that("Single character units are not fuzzy matched", {
  # There are too many one-letter matches in the unit table, so a single character
  # that isn't an exact alias must error rather than silently matching "M" or "m".
  expect_error(suppressWarnings(convert_units(10, "ca", startunit = "x", endunit = "M")))
  expect_error(suppressWarnings(convert_units(10, "ca", startunit = "mg/L", endunit = "q")))

  # ...while the legitimate one-letter units still resolve exactly, with no warning.
  expect_no_warning(mol <- convert_units(10, "ca", startunit = "mg/L", endunit = "M"))
  expect_equal(convert_units(10, "ca", startunit = "mg/L", endunit = "m"), mol)
})

test_that("Canonical units and formulas convert without warning", {
  expect_no_warning(convert_units(10, "ca", startunit = "mg/L", endunit = "M"))
  expect_no_warning(convert_units(10, "ca", startunit = "mg/L CaCO3", endunit = "eq/L"))
  expect_no_warning(convert_units(10, "naoh", startunit = "M", endunit = "mg/L"))
  expect_no_warning(convert_units(10, "po4", startunit = "mg/L", endunit = "mg/L P"))
  expect_no_warning(convert_units(10, "nh4", startunit = "mg/L", endunit = "mg/L N"))
})


test_that("As CaCO3/N/P/Cl2 works", {
  mglCa <- 20
  mglasCaCO3 <- convert_units(mglCa, "ca", startunit = "mg/L", endunit = "mg/L CaCO3")
  mglasCaCO3_eq <- (mglCa / 40.078) * 100.0869
  expect_equal(mglasCaCO3, mglasCaCO3_eq)
  mglasCaCO3_start <- convert_units(mglasCaCO3_eq, "ca", startunit = "mg/L CaCO3", endunit = "mg/L")
  expect_equal(mglasCaCO3_start, mglCa)

  mglNH4 <- 20
  mglasN <- convert_units(mglNH4, "nh4", startunit = "mg/L", endunit = "mg/L N")
  mglasN_eq <- (mglNH4 / 18.04) * 14.0067
  expect_equal(mglasN, mglasN_eq)
  mglasN_start <- convert_units(mglasN_eq, "nh4", startunit = "mg/L N", endunit = "mg/L")
  expect_equal(mglasN_start, mglNH4)

  mglPO4 <- 20
  mglasP <- convert_units(mglPO4, "po4", startunit = "mg/L", endunit = "mg/L P")
  mglasNP_eq <- (mglPO4 / 94.97) * 30.97
  expect_equal(mglasP, mglasNP_eq)
  mglasP_start <- convert_units(mglasNP_eq, "po4", startunit = "mg/L P", endunit = "mg/L")
  expect_equal(mglasP_start, mglPO4)

  mglNHCl2 <- 20
  mglasCl2 <- convert_units(mglNHCl2, "nhcl2", startunit = "mg/L", endunit = "mg/L Cl2")
  # NHCl2 has two active chlorines, so 1 mol NHCl2 = 2 mol as Cl2
  mglasCl2_eq <- (mglNHCl2 / 85.92) * mweights$cl2 * 2
  expect_equal(mglasCl2, mglasCl2_eq)
  mglasCl2_start <- convert_units(mglasCl2_eq, "nhcl2", startunit = "mg/L Cl2", endunit = "mg/L")
  expect_equal(mglasCl2_start, mglNHCl2)

  mglAmmoniumSulfate <- 20
  mglasNFromAmmoniumSulfate <- convert_units(mglAmmoniumSulfate, "nh42so4", startunit = "mg/L", endunit = "mg/L N")
  mglasNFromAmmoniumSulfate_eq <- (mglAmmoniumSulfate / mweights$nh42so4) * mweights$n * 2
  expect_equal(mglasNFromAmmoniumSulfate, mglasNFromAmmoniumSulfate_eq)
})

test_that("Mass-unit scaling is preserved during basis conversions", {
  expect_equal(
    convert_units(1, "hcl", "g/L", "mg/L CaCO3"),
    convert_units(1000, "hcl", "mg/L", "mg/L CaCO3")
  )
  expect_equal(
    convert_units(1, "nh4", "g/L N", "mg/L"),
    convert_units(1000, "nh4", "mg/L N", "mg/L")
  )
})

test_that("Mass as CaCO3 goes through equivalents", {
  # HCO3 is monovalent: 1 mol = 1 eq = 50 g as CaCO3
  expect_equal(convert_units(1, "hco3", "mg/L", "mg/L CaCO3"), 1 / mweights$hco3 * mweights$caco3 / 2)
  expect_equal(convert_units(1, "hco3", "mg/L CaCO3", "mg/L"), 1 / (mweights$caco3 / 2) * mweights$hco3)
  expect_equal(convert_units(1, "hco3", "mg/L CaCO3", "mM"), 1 / (mweights$caco3 / 2))
  expect_equal(convert_units(1, "hco3", "mg/L CaCO3", "meq/L"), 1 / (mweights$caco3 / 2))
  expect_equal(convert_units(1, "hco3", "mM", "mg/L CaCO3"), mweights$caco3 / 2)

  # Ca is divalent: 1 mol = 2 eq = 100 g as CaCO3
  expect_equal(convert_units(1, "ca", "mM", "mg/L CaCO3"), 2 * mweights$caco3 / 2)
  expect_equal(convert_units(1, "ca", "meq/L", "mg/L CaCO3"), mweights$caco3 / 2)

  # Formulas without a charge can't be expressed as CaCO3
  expect_error(convert_units(1, "cl2", "mg/L", "mg/L CaCO3"), "CaCO3")
  expect_error(convert_units(1, "nh3", "mg/L CaCO3", "M"), "CaCO3")
})

test_that("Mass as N or P accounts for atoms per formula", {
  # NH4 has one N
  expect_equal(convert_units(1, "nh4", "mg/L", "mg/L N"), 1 / mweights$nh4 * mweights$n)
  expect_equal(convert_units(1, "nh4", "mg/L N", "mg/L"), 1 / mweights$n * mweights$nh4)
  expect_equal(convert_units(1, "nh4", "mM", "mg/L N"), mweights$n)

  # (NH4)2SO4 has two N
  expect_equal(convert_units(1, "nh42so4", "mg/L", "mg/L N"), 1 / mweights$nh42so4 * 2 * mweights$n)
  expect_equal(convert_units(1, "nh42so4", "mg/L N", "mg/L"), 1 / (2 * mweights$n) * mweights$nh42so4)
  expect_equal(convert_units(1, "nh42so4", "mM", "mg/L N"), 2 * mweights$n)
  expect_equal(convert_units(1, "nh42so4", "mg/L N", "mM"), 1 / (2 * mweights$n))
  expect_equal(convert_units(1, "nh42so4", "mg/L N", "meq/L"), 1 / (2 * mweights$n) * 2)
  expect_equal(convert_units(1, "(NH4)2SO4", "mg/L", "mg/L N"), 1 / mweights$nh42so4 * 2 * mweights$n)

  # PO4 has one P
  expect_equal(convert_units(1, "po4", "mM", "mg/L P"), mweights$p)
  expect_equal(convert_units(1, "po4", "meq/L", "mg/L P"), 1 / 3 * mweights$p)

  # No N in calcium
  expect_error(convert_units(1, "ca", "mg/L", "mg/L N"))
})

test_that("Mass as Cl2 counts active chlorines", {
  expect_equal(convert_units(1, "ocl", "mM", "mg/L Cl2"), mweights$cl2)
  expect_equal(convert_units(1, "nh2cl", "mM", "mg/L Cl2"), mweights$cl2)
  expect_equal(convert_units(1, "nhcl2", "mM", "mg/L Cl2"), 2 * mweights$cl2)
  expect_equal(convert_units(1, "ncl3", "mM", "mg/L Cl2"), 3 * mweights$cl2)
  expect_equal(convert_units(1, "ncl3", "mg/L Cl2", "mM"), 1 / (3 * mweights$cl2))

  # Hypochlorite salts and HOCl
  expect_equal(convert_units(1, "hocl", "mM", "mg/L Cl2"), mweights$cl2)
  expect_equal(convert_units(1, "naocl", "mM", "mg/L Cl2"), mweights$cl2)
  expect_equal(convert_units(1, "caocl2", "mM", "mg/L Cl2"), 2 * mweights$cl2)
  expect_equal(convert_units(1, "Ca(OCl)2", "mg/L", "mg/L Cl2"), 1 / mweights$caocl2 * 2 * mweights$cl2)
  expect_equal(convert_units(1, "NaOCl", "mg/L", "mg/L Cl2"), 1 / mweights$naocl * mweights$cl2)
})

test_that("Conversions between bases match converting through the formula", {
  # (NH4)2SO4: 1 mol = 2 mol N = 2 eq
  expect_equal(convert_units(1, "nh42so4", "mg/L N", "mg/L CaCO3"), mweights$caco3 / (2 * mweights$n))
  expect_equal(convert_units(1, "nh42so4", "mg/L CaCO3", "mg/L N"), 2 * mweights$n / mweights$caco3)

  # NH4: 1 mol = 1 mol N = 1 eq
  expect_equal(convert_units(1, "nh4", "mg/L CaCO3", "mg/L N"), 1 / (mweights$caco3 / 2) * mweights$n)

  # PO4: 1 mol = 1 mol P = 3 eq
  expect_equal(convert_units(1, "po4", "mg/L CaCO3", "mg/L P"), 1 / (mweights$caco3 / 2) / 3 * mweights$p)

  # HPO4: 1 mol = 1 mol P = 2 eq
  expect_equal(convert_units(1, "hpo4", "mg/L CaCO3", "mg/L P"), 1 / (mweights$caco3 / 2) / 2 * mweights$p)
  expect_equal(convert_units(1, "hpo4", "mg/L P", "mg/L CaCO3"), 1 / mweights$p * 2 * mweights$caco3 / 2)
})

test_that("Conversions within the same unit type don't need a formula", {
  expect_equal(convert_units(1000, startunit = "mg/L", endunit = "g/L"), 1)
  expect_equal(convert_units(1000, startunit = "ug/L CaCO3", endunit = "mg/L CaCO3"), 1)
  expect_equal(convert_units(1, startunit = "mg/L N", endunit = "ug/L N"), 1000)
  expect_equal(convert_units(5, startunit = "mM", endunit = "M"), .005)
  expect_equal(convert_units(5, startunit = "meq/L", endunit = "eq/L"), .005)
  expect_equal(convert_units(5, "xyz", startunit = "mg/L", endunit = "g/L"), .005)

  expect_error(convert_units(10, startunit = "mg/L", endunit = "M"))
  expect_error(convert_units(10, startunit = "mg/L", endunit = "mg/L CaCO3"))
  expect_error(convert_units(10, startunit = "mM", endunit = "meq/L"))
})
# Summarize WQ ----

test_that("Summarize WQ returns a kable and prints pH and Alkalinity.", {
  water1 <- define_water(
    ph = 7,
    temp = 25,
    alk = 100,
    0,
    0,
    0,
    0,
    0,
    0,
    tds = 100,
    toc = 5,
    doc = 4.8,
    uv254 = .1,
    br = 50
  )
  expect_match(summarise_wq(water1), ".+pH.+7.+Alkalinity.+100.+")
  expect_s3_class(summarise_wq(water1), "knitr_kable")
})

# Plot Ions ----

test_that("Plot ions creates a ggplot object that can be printed.", {
  water1 <- define_water(
    ph = 7,
    temp = 25,
    alk = 100,
    0,
    0,
    0,
    0,
    0,
    0,
    tds = 100,
    toc = 5,
    doc = 4.8,
    uv254 = .1,
    br = 50
  )
  expect_s3_class(plot_ions(water1), "ggplot")
  expect_no_error(plot_ions(water1))
})

# Plot Lead ----
test_that("Plot lead returns expected errors and warnings.", {
  testthat::skip_on_cran()
  df1 <- data.frame(
    dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
    temp = 25,
    tds = 200
  )
  df2 <- data.frame(
    ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
    temp = 25,
    tds = 200
  )
  df3 <- data.frame(
    ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
    dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
    tds = 200
  )
  df4 <- data.frame(
    ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
    dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
    temp = 25,
    tds = 200,
    alk = 100
  )
  df5 <- data.frame(
    ph = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
    dic = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
    temp = 25,
    tds = rep(c(150, 200), 5)
  )
  expect_error(plot_lead(df1))
  expect_error(plot_lead(df2))
  expect_error(plot_lead(df3))
  expect_no_error(plot_lead(df3, temp = 25))
  expect_warning(plot_lead(df4))
  expect_warning(plot_lead(df5))
})

test_that("Plot lead creates a ggplot object that can be printed.", {
  historical <- data.frame(
    pH = c(7.7, 7.86, 8.31, 7.58, 7.9, 8.06, 7.95, 8.02, 7.93, 7.61),
    DIC = c(14.86, 16.41, 16.48, 16.63, 16.86, 16.94, 17.05, 17.23, 17.33, 17.34),
    temperature = 25,
    `total dissolved solids` = 200
  )
  expect_s3_class(plot_lead(historical, ph_range = c(7, 10), dic_range = c(10, 100)), "ggplot")
  expect_no_error(plot_lead(historical))
})


# Calculate Hardness ----

test_that("Total hardness calculation works.", {
  expect_equal(calculate_hardness(20, 2), 20 / mweights$ca * mweights$caco3 + 2 / mweights$mg * mweights$caco3)
  expect_equal(
    calculate_hardness(.002, .001, startunit = "M"),
    .002 * mweights$caco3 * 1000 + .001 * mweights$caco3 * 1000
  )
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

  is_calced <- 0.5 *
    ((water@na +
      water@cl +
      water@k +
      water@hco3 +
      water@h2po4 +
      water@h +
      water@oh +
      water@ocl +
      water@br +
      water@nh4) *
      1^2 +
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
