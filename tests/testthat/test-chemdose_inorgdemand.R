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

test_that("chemdose_inorgdemand uses the free_chlorine slot only when use_chlorine_slot = TRUE", {
  water0 <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1))
  water3 <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 3))
  cl_dose <- 15
  slot_mgl <- convert_units(water3@free_chlorine, "cl2", "M", "mg/L")

  expect_warning(ignored <- chemdose_inorgdemand(water3, cl_dose), "will be overridden")
  expect_no_warning(used <- chemdose_inorgdemand(water3, cl_dose, use_chlorine_slot = TRUE))
  expect_no_warning(no_slot <- chemdose_inorgdemand(water0, cl_dose))

  # default ignores the slot: same result as a water with no existing chlorine
  expect_equal(ignored@free_chlorine, no_slot@free_chlorine)
  # use_chlorine_slot = TRUE adds exactly the slot's free chlorine to the residual
  expect_equal(
    convert_units(used@free_chlorine, "cl2", "M", "mg/L") - convert_units(ignored@free_chlorine, "cl2", "M", "mg/L"),
    slot_mgl
  )
  # manual check: 3 mg/L slot + 15 mg/L dose - (1 * 1.29 + 2 * 0.64 + 1 * 7.6) demand = 7.83 mg/L as Cl2
  expect_equal(convert_units(used@free_chlorine, "cl2", "M", "mg/L"), slot_mgl + cl_dose - 10.17)
  expect_equal(convert_units(used@free_chlorine, "cl2", "M", "mg/L"), 7.83)

  # a water with no free_chlorine slot can't use it
  water_na <- water0
  water_na@free_chlorine <- NA_real_
  expect_error(chemdose_inorgdemand(water_na, cl_dose, use_chlorine_slot = TRUE), "free_chlorine")
  expect_no_error(chemdose_inorgdemand(water_na, cl_dose))
})

test_that("chemdose_inorgdemand uses the free_chlorine slot when cl_dose is omitted", {
  water12 <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1, free_chlorine = 12))
  slot_mgl <- convert_units(water12@free_chlorine, "cl2", "M", "mg/L")

  # default use_chlorine_slot = FALSE: the slot is used, with a warning
  expect_warning(out_default <- chemdose_inorgdemand(water12), "`cl_dose` not specified")
  # use_chlorine_slot = TRUE: same result, no warning
  expect_no_warning(out_slot <- chemdose_inorgdemand(water12, use_chlorine_slot = TRUE))

  expect_equal(out_default, out_slot)
  # manual check: 12 mg/L slot - 10.17 mg/L demand = 1.83 mg/L as Cl2
  expect_equal(convert_units(out_slot@free_chlorine, "cl2", "M", "mg/L"), slot_mgl - 10.17)
})

test_that("chemdose_inorgdemand errors on invalid arguments", {
  water <- suppressWarnings(define_water(mn = 1000, fe = 2, tot_nh3 = 1))

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
  expect_false(any(c("oxidized_mn", "oxidized_fe", "oxidized_tot_nh3") %in% names(unquoted)))
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

  expected <- sapply(base$defined, function(w) chemdose_inorgdemand(w, use_chlorine_slot = TRUE)@free_chlorine)

  expect_no_warning(out_slot <- chemdose_inorgdemand_df(base, use_chlorine_slot = TRUE, pluck_cols = TRUE))
  expect_equal(out_slot$oxidized_free_chlorine, expected)

  # default use_chlorine_slot = FALSE also uses the slot, with one aggregated warning
  expect_warning(out_default <- chemdose_inorgdemand_df(base, pluck_cols = TRUE), "`cl_dose` not specified.*12 rows")
  expect_equal(out_default$oxidized_free_chlorine, expected)
})
