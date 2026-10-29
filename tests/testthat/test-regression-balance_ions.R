# Regression pins: balance_ions ----
# Golden files pin every slot of the balanced water. See tests/testthat/README.md.

test_that("regression: balance_ions cation options on cation-short waters", {
  testthat::skip_on_cran()
  # All major ions given, so tds and cond are estimated and get updated by the balance.
  estimated <- suppressWarnings(define_water(
    ph = 7, temp = 20, alk = 50, ca = 10, mg = 2, na = 5, k = 1, cl = 100, so4 = 80
  ))
  # Same ions with measured tds, so tds/cond/is are left alone.
  measured <- suppressWarnings(define_water(
    ph = 7, temp = 20, alk = 50, ca = 10, mg = 2, na = 5, k = 1, cl = 100, so4 = 80, tds = 400
  ))
  cases <- list()
  for (cat in c("na", "k", "ca", "mg")) {
    cases[[paste0("tds_estimated_", cat)]] <- suppressWarnings(balance_ions(estimated, cation = cat))
    cases[[paste0("tds_measured_", cat)]] <- suppressWarnings(balance_ions(measured, cation = cat))
  }
  expect_golden(cases, "balance_cations")
})

test_that("regression: balance_ions anion options on anion-short waters", {
  testthat::skip_on_cran()
  estimated <- suppressWarnings(define_water(
    ph = 7.5, temp = 15, alk = 30, ca = 60, mg = 20, na = 40, k = 5, cl = 5, so4 = 5
  ))
  measured <- suppressWarnings(define_water(
    ph = 7.5, temp = 15, alk = 30, ca = 60, mg = 20, na = 40, k = 5, cl = 5, so4 = 5, cond = 500
  ))
  cases <- list()
  for (an in c("cl", "so4")) {
    cases[[paste0("tds_estimated_", an)]] <- suppressWarnings(balance_ions(estimated, anion = an))
    cases[[paste0("cond_measured_", an)]] <- suppressWarnings(balance_ions(measured, anion = an))
  }
  expect_golden(cases, "balance_anions")
})

test_that("regression: balance_ions fills NA ion slots", {
  testthat::skip_on_cran()
  cation_na <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 50, cl = 100, so4 = 80))
  anion_na <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 20, ca = 50, mg = 20, na = 50))
  cases <- list()
  for (cat in c("na", "k", "ca", "mg")) {
    cases[[paste0("na_slots_cation_", cat)]] <- suppressWarnings(balance_ions(cation_na, cation = cat))
  }
  for (an in c("cl", "so4")) {
    cases[[paste0("na_slots_anion_", an)]] <- suppressWarnings(balance_ions(anion_na, anion = an))
  }
  expect_golden(cases, "balance_na_slots")
})

test_that("regression: balance_ions on scenario waters", {
  testthat::skip_on_cran()
  w <- regression_waters()
  cases <- lapply(w, function(x) suppressWarnings(balance_ions(x)))
  cases$typical_so4_ca <- suppressWarnings(balance_ions(w$typical, anion = "so4", cation = "ca"))
  cases$typical_balanced_twice <- suppressWarnings(balance_ions(balance_ions(w$typical)))
  expect_golden(cases, "balance_scenarios")
})
