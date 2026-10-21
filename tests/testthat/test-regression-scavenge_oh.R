# Regression pins: scavenge_oh ----
# Golden files pin the returned OH radical scavenging term (s^-1). See tests/testthat/README.md.

test_that("regression: scavenge_oh scenario waters x H2O2 dose", {
  waters <- regression_waters()
  out <- list()
  for (nm in names(waters)) {
    for (dose in c(0, 5, 20)) {
      out[[paste0(nm, "_h2o2_", dose)]] <- suppressWarnings(scavenge_oh(waters[[nm]], dose = dose))
    }
  }
  expect_golden(out, "scavenge_oh_waters")
})

test_that("regression: scavenge_oh kdoc_adjust, nitrite/nitrate, and missing slots", {
  w <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100, tds = 250, doc = 3, no3 = 4, no2 = 0.2))
  expect_golden(
    suppressWarnings(list(
      default_k = scavenge_oh(w, dose = 10),
      kdoc_1.67e4 = scavenge_oh(w, dose = 10, kdoc_adjust = 1.67e4),
      kdoc_3.6e4 = scavenge_oh(w, dose = 10, kdoc_adjust = 3.6e4),
      high_ph_co3 = scavenge_oh(suppressWarnings(define_water(ph = 10, temp = 20, alk = 100, doc = 3)), dose = 10),
      no_doc = scavenge_oh(suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100)), dose = 10),
      di_water = scavenge_oh(suppressWarnings(define_water(ph = 7)), dose = 10)
    )),
    "scavenge_oh_options"
  )
})
