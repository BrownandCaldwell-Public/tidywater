test_that("chemdose_inorgdemand calculates correct Cl residual", {

  water<-suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1))
  cl_dose <- 40

  cl_residual1 <- chemdose_inorgdemand(cl_dose=cl_dose, water= water)

  # manual check: mn = 1, fe = 2, nh3 = 1 (all in mg/L)
  tot_cl_scavenged <- 1 * 1.29 + 2 * 0.64 + 1 * 7.6
  cl_residual2 <- cl_dose - tot_cl_scavenged
  
  expect_equal(round(cl_residual1@free_chlorine, 1), round(cl_residual2, 1))

})
