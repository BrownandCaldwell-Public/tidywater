# Regression pins: decarbonate_ph ----
# Golden files pin every slot of the decarbonated water. See tests/testthat/README.md.

test_that("regression: decarbonate_ph across waters and removal fractions", {
  w <- regression_waters()
  low_ph <- suppressWarnings(define_water(ph = 4, temp = 25, alk = 5, tds = 100))
  acidified <- suppressWarnings(define_water(ph = 5.5, temp = 12, alk = 150, tds = 300, ca = 40))
  waters <- c(w[c("typical", "low_alk", "hard_high_alk", "cold", "warm", "nutrients")],
    list(low_ph = low_ph, acidified = acidified)
  )
  cases <- list()
  for (nm in names(waters)) {
    for (f in c(0, 0.5, 0.9, 1)) {
      cases[[paste0(nm, "_", f)]] <- suppressWarnings(decarbonate_ph(waters[[nm]], co2_removed = f))
    }
  }
  expect_golden(cases, "decarbonate_ph")
})
