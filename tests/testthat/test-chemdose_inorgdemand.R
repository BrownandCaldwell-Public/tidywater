test_that("chemdose_inorgdemand calculates correct Cl residual", {

  water<-suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1))
  cl_dose <- 40

  cl_residual1 <- chemdose_inorgdemand(cl_dose=cl_dose, water= water)

  # manual check: mn = 1, fe = 2, nh3 = 1 (all in mg/L)
  tot_cl_scavenged <- 1 * 1.29 + 2 * 0.64 + 1 * 7.6
  cl_residual2 <- convert_units(cl_dose - tot_cl_scavenged, "cl2") # convert mg/L back to M
  
  expect_equal(round(cl_residual1@free_chlorine, 1), round(cl_residual2, 1))

})

test_that("chemdose_inorgdemand correctly updates oxidized species", {

  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1))
  cl_dose <- 40

  cl_residual1 <- chemdose_inorgdemand(cl_dose=cl_dose, water= water)
  
  expect_equal(cl_residual1@mn, 0)
  expect_equal(cl_residual1@fe, 0)
  expect_equal(cl_residual1@tot_nh3, 0)

})

test_that("chemdose_inorgdemand adds existing free chlorine to the dose", {
  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 3))
  cl_dose <- 15

  out <- chemdose_inorgdemand(water, cl_dose)

  # manual check: 3 mg/L existing + 15 mg/L dose - (1 * 1.29 + 2 * 0.64 + 1 * 7.6) demand = 7.83 mg/L as Cl2
  expect_equal(convert_units(out@free_chlorine, "cl2", "M", "mg/L"), 7.83)

  # existing chlorine alone can satisfy the demand with no dose
  water2 <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 12))
  out2 <- chemdose_inorgdemand(water2, 0)
  expect_equal(convert_units(out2@free_chlorine, "cl2", "M", "mg/L"), 12 - 10.17)
})

test_that("chemdose_inorgdemand errors when demand exceeds available chlorine", {
  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 1))

  expect_error(
    chemdose_inorgdemand(water, 5),
    "Inorganic demand is greater than the available chlorine"
  )
  expect_error(chemdose_inorgdemand(water, 5), "10.2 mg/L as Cl2, but only 6 mg/L as Cl2 is available")
  expect_error(chemdose_inorgdemand(water, 5), "at least 9.17 mg/L as Cl2")
  expect_no_error(chemdose_inorgdemand(water, 9.17))
})

test_that("chemdose_inorgdemand_df outputs are the same as base function, chemdose_inorgdemand", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, mn = 500, fe = 1, tot_nh3 = 0.5,
                                          free_chlorine = 1))
  water1 <- chemdose_inorgdemand(water0, 10)

  water2 <- suppressWarnings(define_water_df(
    data.frame(ph = 7.9, temp = 20, alk = 50, mn = 500, fe = 1, tot_nh3 = 0.5, free_chlorine = 1)
  )) %>%
    chemdose_inorgdemand_df(cl_dose = 10)

  expect_equal(water1, water2$oxidized[[1]])
  expect_false("cl_dose" %in% names(water2))
})

test_that("chemdose_inorgdemand_df accepts column inputs and plucks columns", {
  testthat::skip_on_cran()
  base <- suppressWarnings(define_water_df(dplyr::mutate(water_df, mn = 20, fe = 0.5, tot_nh3 = 0.2)))

  # unquoted column name and "use_col" give the same result
  unquoted <- base %>%
    dplyr::mutate(ClDose = seq(3, 14, 1)) %>%
    chemdose_inorgdemand_df(cl_dose = ClDose, pluck_cols = TRUE)
  use_col <- base %>%
    dplyr::mutate(cl_dose = seq(3, 14, 1)) %>%
    chemdose_inorgdemand_df(pluck_cols = TRUE, keep_dose_cols = TRUE)

  expect_equal(unquoted$oxidized_free_chlorine, use_col$oxidized_free_chlorine)
  expect_true("cl_dose" %in% names(use_col))

  expected <- sapply(seq_len(nrow(base)), function(i) {
    chemdose_inorgdemand(base$defined[[i]], seq(3, 14, 1)[i])@free_chlorine
  })
  expect_equal(unquoted$oxidized_free_chlorine, expected)
  expect_true(all(unquoted$oxidized_mn == 0 & unquoted$oxidized_fe == 0 & unquoted$oxidized_tot_nh3 == 0))
})

test_that("chemdose_inorgdemand_df accepts constant inputs and output options", {
  testthat::skip_on_cran()
  base <- suppressWarnings(define_water_df(dplyr::mutate(water_df, mn = 20, fe = 0.5, tot_nh3 = 0.2)))

  out <- chemdose_inorgdemand_df(base, cl_dose = 5, output_water = "chlorinated", pluck_cols = TRUE,
                                 water_prefix = FALSE)

  expected <- sapply(base$defined, function(w) chemdose_inorgdemand(w, 5)@free_chlorine)
  expect_equal(out$free_chlorine, expected)
  expect_true("chlorinated" %in% names(out))
  expect_false("cl_dose" %in% names(out))
})
