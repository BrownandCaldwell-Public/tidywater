# Regression pins: opensys_ph ----
# opensys_ph solves with uniroot at its default tolerance, so pins are loose: pH is
# rounded to 0.01 by solve_ph, and downstream slots range from ~1e-10 M to hundreds of
# mg/L. rel_tol = 1e-4 covers the solver noise for large slots, abs_tol = 1e-10 for tiny ones.

test_that("regression: opensys_ph across waters, partial pressures, and temperatures", {
  w <- regression_waters()
  waters <- w[c("typical", "low_alk", "hard_high_alk", "cold", "warm", "nutrients")]
  cases <- list()
  for (nm in names(waters)) {
    cases[[paste0(nm, "_default")]] <- suppressWarnings(opensys_ph(waters[[nm]]))
    for (pp in c(10^-4, 10^-3, 10^-2)) {
      cases[[paste0(nm, "_pp", signif(log10(pp), 3))]] <- suppressWarnings(opensys_ph(waters[[nm]], partialpressure = pp))
    }
  }
  for (t in c(1, 15, 35)) {
    tw <- suppressWarnings(define_water(ph = 7.5, temp = t, alk = 80, tds = 200))
    cases[[paste0("temp", t)]] <- suppressWarnings(opensys_ph(tw))
  }
  # Supersaturated low-pH water: only high pCO2 is solvable, since the required CO2
  # removal at lower pCO2 falls outside uniroot's [-100, 100] mg/L interval (errors).
  low_ph <- suppressWarnings(define_water(ph = 5, temp = 25, alk = 5, tds = 100))
  cases$low_ph_pp_2 <- suppressWarnings(opensys_ph(low_ph, partialpressure = 10^-2))
  expect_golden(cases, "opensys_ph", rel_tol = 1e-4, abs_tol = 1e-10)
})
