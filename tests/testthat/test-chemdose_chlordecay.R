# Chemdose chlorine/chloramine ----

test_that("chemdose_chlordecay returns modeled chlorine/chloramine residual = 0 when chlorine dose is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2))
  Ct <- suppressWarnings(chemdose_chlordecay(water1, cl2_dose = 0, time = 8))

  expect_equal(water1@ocl, 0)
})

test_that("chemdose_chlordecay does not run when treatment_type isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))

  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, treatment = "rw"))
  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, treatment = treated))
})

test_that("chemdose_chlordecay warns when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = .1, uv254 = 0.01))
  water3 <- suppressWarnings(define_water(ph = 8, temp = 20, toc = 3, uv254 = 0.01))
  water4 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3, uv254 = 0.1))

  expect_warning(chemdose_chlordecay(water1, cl2_dose = 0.994, time = 1)) # chlorine out of bounds
  expect_warning(chemdose_chlordecay(water1, cl2_dose = 2, time = 121, treatment = "coag")) # time out of bounds
  expect_warning(chemdose_chlordecay(water2, cl2_dose = 0.995, time = 100)) # toc out of bounds
  expect_warning(chemdose_chlordecay(water3, cl2_dose = 2, time = 100, treatment = "coag")) # uv254 out of bounds
})

test_that("chemdose_chlordecay stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, br = 50, uv254 = 0.1))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2))
  water5 <- suppressWarnings(define_water(ph = 8, temp = 25, toc = 3, uv = 0.2))

  expect_error(chemdose_chlordecay(water1, cl_type = "chloramine", cl2_dose = 2, time = 1)) # missing uv254
  expect_error(chemdose_chlordecay(water2, cl2_dose = 2, time = 1, treatment = "coag")) # missing toc
  expect_no_error(suppressWarnings(chemdose_chlordecay(water3, cl2_dose = 4, time = 0.22, treatment = "coag"))) # raw doesn't require uv
  expect_error(chemdose_chlordecay(water5, time = 1, treatment = "coag")) # missing cl2_dose
  expect_error(chemdose_chlordecay(water5, cl2_dose = 4, treatment = "coag")) # missing time
})

test_that("chemdose_chlordecay works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- chemdose_chlordecay(water1, cl2_dose = 3, time = 8)
  water3 <- chemdose_chlordecay(water1, cl2_dose = 4, time = 3, treatment = "coag")
  # water4 <- chemdose_chlordecay(water1, cl_type = 'chloramine', cl2_dose = 4, time = 5, treatment = "coag")
  # water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 1, uv254 = 0.04, br = 50))
  # water6 <- chemdose_chlordecay(water5, cl_type = 'chloramine', cl2_dose = 6, time = 10)

  expect_equal(signif(water2@free_chlorine, 3), 1.83E-5)
  expect_equal(signif(water3@free_chlorine, 3), 4.53E-5)
  # expect_equal(signif(water4@free_chlorine,3), 7.23E-5)
  # expect_equal(signif(water6@free_chlorine,3), 1.1E-4)
})
