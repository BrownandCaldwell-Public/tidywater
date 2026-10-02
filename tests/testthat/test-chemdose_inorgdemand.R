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

test_that("chemdose_inorgdemand overwrites existing free chlorine by default and warns", {
  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 3))

  expect_warning(out <- chemdose_inorgdemand(water, 15), "will be overridden")

  # manual check: 15 mg/L dose - (1 * 1.29 + 2 * 0.64 + 1 * 7.6) demand = 4.83 mg/L as Cl2
  expect_equal(convert_units(out@free_chlorine, "cl2", "M", "mg/L"), 4.83)

  # no warning when there is no existing chlorine
  water0 <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1))
  expect_no_warning(chemdose_inorgdemand(water0, 15))
})

test_that("chemdose_inorgdemand adds existing free chlorine when use_chlorine_slot = TRUE", {
  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 3))

  expect_warning(out <- chemdose_inorgdemand(water, 15, use_chlorine_slot = TRUE), "summed with residual chlorine")

  # manual check: 3 mg/L existing + 15 mg/L dose - 10.17 mg/L demand = 7.83 mg/L as Cl2
  expect_equal(convert_units(out@free_chlorine, "cl2", "M", "mg/L"), 7.83)

  # existing chlorine alone can satisfy the demand when cl_dose is omitted
  water2 <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 12))
  expect_no_warning(out2 <- chemdose_inorgdemand(water2, use_chlorine_slot = TRUE))
  expect_equal(convert_units(out2@free_chlorine, "cl2", "M", "mg/L"), 12 - 10.17)
})

test_that("chemdose_inorgdemand errors on invalid or missing arguments", {
  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1))

  expect_error(chemdose_inorgdemand(water), "Missing value for `cl_dose`")
  expect_error(chemdose_inorgdemand(water, 5, use_chlorine_slot = "yes"), "must be `TRUE` or `FALSE`")
})

test_that("chemdose_inorgdemand errors when demand exceeds available chlorine", {
  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 1))

  # default: only cl_dose is available
  expect_error(
    suppressWarnings(chemdose_inorgdemand(water, 5)),
    "Inorganic demand is greater than the available chlorine"
  )
  expect_error(suppressWarnings(chemdose_inorgdemand(water, 5)), "10.2 mg/L as Cl2, but only 5 mg/L as Cl2 is available")
  expect_error(suppressWarnings(chemdose_inorgdemand(water, 5)), "at least 10.2 mg/L as Cl2")

  # use_chlorine_slot = TRUE: existing free chlorine counts toward the available chlorine
  expect_error(
    suppressWarnings(chemdose_inorgdemand(water, 5, use_chlorine_slot = TRUE)),
    "10.2 mg/L as Cl2, but only 6 mg/L as Cl2 is available"
  )
  expect_error(suppressWarnings(chemdose_inorgdemand(water, 5, use_chlorine_slot = TRUE)), "at least 9.17 mg/L as Cl2")
  expect_no_error(suppressWarnings(chemdose_inorgdemand(water, 9.17, use_chlorine_slot = TRUE)))
})

test_that("chemdose_inorgdemand_df outputs are the same as base function, chemdose_inorgdemand", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(ph = 7.9, temp = 20, alk = 50, mn = 500, fe = 1, tot_nh3 = 0.5,
                                          free_chlorine = 1))
  water1 <- suppressWarnings(chemdose_inorgdemand(water0, 10))
  water1_slot <- suppressWarnings(chemdose_inorgdemand(water0, 10, use_chlorine_slot = TRUE))

  base <- suppressWarnings(define_water_df(
    data.frame(ph = 7.9, temp = 20, alk = 50, mn = 500, fe = 1, tot_nh3 = 0.5, free_chlorine = 1)
  ))
  water2 <- suppressWarnings(chemdose_inorgdemand_df(base, cl_dose = 10))
  water2_slot <- suppressWarnings(chemdose_inorgdemand_df(base, cl_dose = 10, use_chlorine_slot = TRUE))

  expect_equal(water1, water2$oxidized[[1]])
  expect_equal(water1_slot, water2_slot$oxidized[[1]])
  expect_false("cl_dose" %in% names(water2))
})

test_that("chemdose_inorgdemand_df accepts column inputs and plucks columns", {
  testthat::skip_on_cran()
  base <- suppressWarnings(define_water_df(dplyr::mutate(water_df, mn = 20, fe = 0.5, tot_nh3 = 0.2)))
  slot <- rep(c(TRUE, FALSE), 6)

  # unquoted column name and "use_col" give the same result
  unquoted <- base %>%
    dplyr::mutate(ClDose = seq(3, 14, 1), UseSlot = slot) %>%
    chemdose_inorgdemand_df(cl_dose = ClDose, use_chlorine_slot = UseSlot, pluck_cols = TRUE) %>%
    suppressWarnings()
  use_col <- base %>%
    dplyr::mutate(cl_dose = seq(3, 14, 1), use_chlorine_slot = slot) %>%
    chemdose_inorgdemand_df(pluck_cols = TRUE, keep_dose_cols = TRUE) %>%
    suppressWarnings()

  expect_equal(unquoted$oxidized_free_chlorine, use_col$oxidized_free_chlorine)
  expect_true("cl_dose" %in% names(use_col))

  expected <- sapply(seq_len(nrow(base)), function(i) {
    suppressWarnings(chemdose_inorgdemand(base$defined[[i]], seq(3, 14, 1)[i], slot[i])@free_chlorine)
  })
  expect_equal(unquoted$oxidized_free_chlorine, expected)
  expect_true(all(unquoted$oxidized_mn == 0 & unquoted$oxidized_fe == 0 & unquoted$oxidized_tot_nh3 == 0))
})

test_that("chemdose_inorgdemand_df accepts constant inputs and output options", {
  testthat::skip_on_cran()
  base <- suppressWarnings(define_water_df(dplyr::mutate(water_df, mn = 20, fe = 0.5, tot_nh3 = 0.2)))

  # water_df has free chlorine in half the rows; the overwrite warning is reported once with a row count
  expect_warning(
    out <- chemdose_inorgdemand_df(base, cl_dose = 5, output_water = "chlorinated", pluck_cols = TRUE,
                                   water_prefix = FALSE),
    "6 rows affected"
  )

  expected <- sapply(base$defined, function(w) suppressWarnings(chemdose_inorgdemand(w, 5))@free_chlorine)
  expect_equal(out$free_chlorine, expected)
  expect_true("chlorinated" %in% names(out))
  expect_false("cl_dose" %in% names(out))
})

test_that("chemdose_inorgdemand_df uses the chlorine slot when cl_dose is omitted", {
  testthat::skip_on_cran()
  base <- suppressWarnings(define_water_df(
    dplyr::mutate(water_df, mn = 20, fe = 0.5, tot_nh3 = 0.2, free_chlorine = 4)
  ))

  out <- chemdose_inorgdemand_df(base, use_chlorine_slot = TRUE, pluck_cols = TRUE)

  expected <- sapply(base$defined, function(w) chemdose_inorgdemand(w, use_chlorine_slot = TRUE)@free_chlorine)
  expect_equal(out$oxidized_free_chlorine, expected)
  expect_error(chemdose_inorgdemand_df(base), "Missing value for `cl_dose`")
})
