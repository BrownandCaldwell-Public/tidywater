# Regression pins: chemdose_inorgdemand ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.

test_that("regression: chemdose_inorgdemand Fe/Mn/NH3 combinations", {
  testthat::skip_on_cran()
  mk <- function(fe = 0, mn = 0, tot_nh3 = 0, ...) {
    suppressWarnings(define_water(ph = 7, temp = 20, alk = 70, tds = 200, fe = fe, mn = mn, tot_nh3 = tot_nh3, ...))
  }
  expect_golden(
    suppressWarnings(list(
      none = chemdose_inorgdemand(mk(), cl2_dose = 2),
      fe_only = chemdose_inorgdemand(mk(fe = 2), cl2_dose = 3),
      mn_only = chemdose_inorgdemand(mk(mn = 500), cl2_dose = 3),
      nh3_only = chemdose_inorgdemand(mk(tot_nh3 = 0.5), cl2_dose = 6),
      fe_mn = chemdose_inorgdemand(mk(fe = 1, mn = 200), cl2_dose = 4),
      fe_nh3 = chemdose_inorgdemand(mk(fe = 1.5, tot_nh3 = 0.3), cl2_dose = 5),
      mn_nh3 = chemdose_inorgdemand(mk(mn = 100, tot_nh3 = 0.8), cl2_dose = 8),
      fe_mn_nh3 = chemdose_inorgdemand(mk(fe = 2, mn = 1000, tot_nh3 = 1), cl2_dose = 20),
      fe_mn_nh3_exact_demand = chemdose_inorgdemand(mk(fe = 1, mn = 0, tot_nh3 = 0), cl2_dose = 0.64)
    )),
    "inorgdemand_combos"
  )
})

test_that("regression: chemdose_inorgdemand with pre-existing chlorine", {
  testthat::skip_on_cran()
  # By default free_chlorine is overwritten with the residual of cl2_dose only and the
  # pre-existing chlorine is dropped (#532); use_chlorine_slot = TRUE adds it to the dose.
  w <- suppressWarnings(define_water(
    ph = 7.5, temp = 15, alk = 50, tds = 150, fe = 0.5, mn = 50, tot_nh3 = 0.2, free_chlorine = 1.5
  ))
  cold <- suppressWarnings(define_water(
    ph = 8, temp = 5, alk = 90, tds = 250, fe = 0.2, mn = 20, tot_nh3 = 0.1, free_chlorine = 0.5,
    combined_chlorine = 0.3
  ))
  expect_golden(
    suppressWarnings(list(
      preexisting_free_cl = chemdose_inorgdemand(w, cl2_dose = 4),
      preexisting_free_and_combined_cl = chemdose_inorgdemand(cold, cl2_dose = 2.5)
    )),
    "inorgdemand_preexisting_cl"
  )
  rich <- suppressWarnings(define_water(
    ph = 7.5, temp = 15, alk = 50, tds = 150, fe = 0.5, mn = 50, tot_nh3 = 0.2, free_chlorine = 4
  ))
  expect_golden(
    suppressWarnings(list(
      slot_plus_dose = chemdose_inorgdemand(w, cl2_dose = 4, use_chlorine_slot = TRUE),
      slot_plus_dose_cold = chemdose_inorgdemand(cold, cl2_dose = 2.5, use_chlorine_slot = TRUE),
      slot_only = chemdose_inorgdemand(rich, use_chlorine_slot = TRUE),
      slot_missing_dose = chemdose_inorgdemand(rich)
    )),
    "inorgdemand_use_chlorine_slot"
  )
  # Demand above slot + dose is an error, not a negative residual.
  expect_error(suppressWarnings(chemdose_inorgdemand(w, use_chlorine_slot = TRUE)), "greater than the available chlorine")
})
