# Regression pins: chemdose_inorgdemand ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.

test_that("regression: chemdose_inorgdemand Fe/Mn/NH3 combinations", {
  mk <- function(fe = 0, mn = 0, tot_nh3 = 0, ...) {
    suppressWarnings(define_water(ph = 7, temp = 20, alk = 70, tds = 200, fe = fe, mn = mn, tot_nh3 = tot_nh3, ...))
  }
  expect_golden(
    suppressWarnings(list(
      none = chemdose_inorgdemand(mk(), cl_dose = 2),
      fe_only = chemdose_inorgdemand(mk(fe = 2), cl_dose = 3),
      mn_only = chemdose_inorgdemand(mk(mn = 500), cl_dose = 3),
      nh3_only = chemdose_inorgdemand(mk(tot_nh3 = 0.5), cl_dose = 6),
      fe_mn = chemdose_inorgdemand(mk(fe = 1, mn = 200), cl_dose = 4),
      fe_nh3 = chemdose_inorgdemand(mk(fe = 1.5, tot_nh3 = 0.3), cl_dose = 5),
      mn_nh3 = chemdose_inorgdemand(mk(mn = 100, tot_nh3 = 0.8), cl_dose = 8),
      fe_mn_nh3 = chemdose_inorgdemand(mk(fe = 2, mn = 1000, tot_nh3 = 1), cl_dose = 20),
      fe_mn_nh3_exact_demand = chemdose_inorgdemand(mk(fe = 1, mn = 0, tot_nh3 = 0), cl_dose = 0.64)
    )),
    "inorgdemand_combos"
  )
})

test_that("regression: chemdose_inorgdemand with pre-existing chlorine", {
  # Current behavior overwrites free_chlorine with the residual of cl_dose only; the
  # pre-existing chlorine is dropped. In question: issue #532.
  w <- suppressWarnings(define_water(
    ph = 7.5, temp = 15, alk = 50, tds = 150, fe = 0.5, mn = 50, tot_nh3 = 0.2, free_chlorine = 1.5
  ))
  cold <- suppressWarnings(define_water(
    ph = 8, temp = 5, alk = 90, tds = 250, fe = 0.2, mn = 20, tot_nh3 = 0.1, free_chlorine = 0.5,
    combined_chlorine = 0.3
  ))
  expect_golden(
    suppressWarnings(list(
      preexisting_free_cl = chemdose_inorgdemand(w, cl_dose = 4),
      preexisting_free_and_combined_cl = chemdose_inorgdemand(cold, cl_dose = 2.5)
    )),
    "inorgdemand_preexisting_cl"
  )
})
