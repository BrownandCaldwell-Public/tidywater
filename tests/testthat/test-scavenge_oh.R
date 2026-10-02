test_that("scavenger works with alk params", {
  water_alk <- suppressWarnings(define_water(ph = 7, doc = 3, alk = 70, no3 = 4))
  water_no_alk <- suppressWarnings(define_water(ph = 7, doc = 3, no3 = 4))

  SC_alk <- scavenge_oh(water_alk, dose = 0)
  #no alk
  expect_warning(SC_base <- scavenge_oh(water_no_alk, dose = 0), "hco3")

  # no negative scavenging!
  expect_true(SC_base >0)
  expect_true(SC_alk >0)

  # alk contributes to scavenging via HCO3/CO3, so SC should be higher with alk
  expect_true(SC_alk > SC_base)

  # adding h2o2 increases scavenging
  sc_alk_h2o2 <- scavenge_oh(water_alk, dose = 10)
  expect_true(sc_alk_h2o2 > SC_alk)

  # numeric tests - values derived from UV-H2O2 spreadsheet tool
  expect_equal(round(SC_alk), 87162)
  expect_equal(round(SC_base), 75029)
  expect_equal(round(sc_alk_h2o2), 95100)

})


test_that("changing scavenger's rate constant for DOC works", {
  # water with experimental params
  water1 <- suppressWarnings(define_water(ph=7, doc=3, alk = 70, no3 = 4))
  h2o2_dose<-20

  background_SC <- scavenge_oh(water1, h2o2_dose)
  background_SC_adjusted <- scavenge_oh(water1, h2o2_dose, 48065)

  expect_error(expect_equal(background_SC, background_SC_adjusted))
})

test_that("scavenge_oh returns one value per dose", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3, alk = 70, no3 = 4))

  sc <- scavenge_oh(water1, dose = c(0, 10))
  expect_length(sc, 2)
  expect_equal(sc, c(scavenge_oh(water1, dose = 0), scavenge_oh(water1, dose = 10)))
})

test_that("scavenge_oh warns when DOC is missing", {
  water_no_doc <- suppressWarnings(define_water(ph = 7, alk = 70, no3 = 4))

  expect_warning(scavenge_oh(water_no_doc), "doc")
})

# scavenge_oh_df ----

test_that("scavenge_oh_df matches scavenge_oh row by row", {
  testthat::skip_on_cran()
  waters <- suppressWarnings(define_water_df(water_df))
  doses <- seq(0, 22, 2)

  df_out <- waters %>%
    dplyr::mutate(h2o2 = doses) %>%
    scavenge_oh_df(dose = h2o2, kdoc_adjust = 1.67e4)

  expected <- vapply(
    seq_len(nrow(waters)),
    function(i) scavenge_oh(waters$defined[[i]], dose = doses[i], kdoc_adjust = 1.67e4),
    numeric(1)
  )

  expect_equal(df_out$oh_scavenging, expected)
  expect_equal(nrow(df_out), nrow(waters))
})

test_that("scavenge_oh_df uses defaults and supports a custom output column", {
  testthat::skip_on_cran()
  waters <- suppressWarnings(define_water_df(water_df))

  default_out <- scavenge_oh_df(waters)
  named_out <- scavenge_oh_df(waters, output_column = "sc")

  expected <- vapply(waters$defined, scavenge_oh, numeric(1))
  expect_equal(default_out$oh_scavenging, expected)
  expect_equal(named_out$sc, expected)
  # default argument columns are dropped
  expect_false(any(c("dose", "kdoc_adjust", "oh_scavenging") %in% names(named_out)))
})

test_that("scavenge_oh_df gives the same result for column and constant inputs", {
  testthat::skip_on_cran()
  waters <- suppressWarnings(define_water_df(water_df))

  constant <- scavenge_oh_df(waters, dose = 10, kdoc_adjust = 48065)
  use_col <- waters %>%
    dplyr::mutate(dose = 10, kdoc_adjust = 48065) %>%
    scavenge_oh_df()
  unquoted_col <- waters %>%
    dplyr::mutate(h2o2 = 10, kdoc = 48065) %>%
    scavenge_oh_df(dose = h2o2, kdoc_adjust = kdoc)

  expect_equal(constant$oh_scavenging, use_col$oh_scavenging)
  expect_equal(constant$oh_scavenging, unquoted_col$oh_scavenging)
  expect_true("dose" %in% names(constant))
})

test_that("scavenge_oh_df crosses a vector of constant doses with each water", {
  testthat::skip_on_cran()
  waters <- suppressWarnings(define_water_df(water_df))

  out <- scavenge_oh_df(waters, dose = c(0, 10))

  expect_equal(nrow(out), 2 * nrow(waters))
  expect_equal(
    out$oh_scavenging,
    vapply(seq_len(nrow(out)), function(i) scavenge_oh(out$defined[[i]], dose = out$dose[i]), numeric(1))
  )
})

test_that("scavenge_oh_df reports a repeated warning once with a row count", {
  testthat::skip_on_cran()
  waters <- suppressWarnings(define_water_df(dplyr::select(water_df, -toc, -doc)))

  warnings <- character()
  withCallingHandlers(
    scavenge_oh_df(waters),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1)
  expect_match(warnings, "12 rows affected")
})

test_that("scavenge_oh_df errors when the input water column is missing", {
  testthat::skip_on_cran()
  expect_error(scavenge_oh_df(water_df))
  waters <- suppressWarnings(define_water_df(water_df))
  expect_error(scavenge_oh_df(waters, input_water = "not_a_column"))
})
