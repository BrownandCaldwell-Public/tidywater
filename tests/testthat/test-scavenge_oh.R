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

test_that("scavenge_oh_df outputs are the same as base function, scavenge_oh", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(
    ph = 7.9,
    temp = 20,
    alk = 50,
    tot_hard = 50,
    na = 20,
    k = 20,
    cl = 30,
    so4 = 20,
    tds = 200,
    cond = 100,
    toc = 2,
    doc = 1.8,
    uv254 = 0.05
  ))
  water1 <- scavenge_oh(water0)
  water2 <- scavenge_oh(water0, dose = 10, kdoc_adjust = 1.67e4)

  water3 <- water_df %>%
    dplyr::slice(1) %>%
    define_water_df() %>%
    scavenge_oh_df()
  water4 <- water_df %>%
    dplyr::slice(1) %>%
    define_water_df() %>%
    scavenge_oh_df(dose = 10, kdoc_adjust = 1.67e4)

  expect_equal(water1, water3$oh_scavenging)
  expect_equal(water2, water4$oh_scavenging)
})

test_that("scavenge_oh_df outputs data frame and can handle an output_column arg", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df()

  water1 <- scavenge_oh_df(water0)
  water2 <- scavenge_oh_df(water0, output_column = "sc")
  water3 <- scavenge_oh_df(water0, output_column = "dose")

  expect_s3_class(water1, "data.frame")
  expect_equal(ncol(water1), ncol(water0) + 1) # default dose and kdoc_adjust columns are dropped
  expect_equal(water1$oh_scavenging, water2$sc)
  expect_equal(water1$oh_scavenging, water3$dose) # output kept when named like a defaulted argument
})

test_that("scavenge_oh_df can use a column and/or function argument for dose and kdoc_adjust", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df()

  dose <- data.frame(dose = seq(2, 24, 2))
  water1 <- water0 %>%
    merge(dose) %>%
    scavenge_oh_df(kdoc_adjust = 48065)

  water2 <- water0 %>%
    scavenge_oh_df(dose = seq(2, 24, 2), kdoc_adjust = 48065)

  water3 <- water0 %>%
    merge(dose) %>%
    dplyr::rename(H2O2Dose = dose) %>%
    dplyr::mutate(kdoc_adjust = 48065) %>%
    scavenge_oh_df(dose = H2O2Dose)

  water4 <- water0 %>%
    merge(dose) %>%
    scavenge_oh_df(kdoc_adjust = c(2.5e4, 48065))

  expect_equal(water1$oh_scavenging, water2$oh_scavenging) # test different ways to input dose
  expect_equal(water1$oh_scavenging, water3$oh_scavenging)
  expect_equal(ncol(water3), ncol(water0) + 3) # adds cols for dose, kdoc_adjust, and oh_scavenging
  expect_equal(nrow(water4), 288) # joined correctly
})

test_that("scavenge_oh_df reports a repeated warning once with a row count", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water_df(dplyr::select(water_df, -toc, -doc)))

  expect_warning(scavenge_oh_df(water0), "12 rows affected")
})

test_that("scavenge_oh_df errors with a missing input water column or non-numeric arguments", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df()

  expect_error(scavenge_oh_df(water_df))
  expect_error(scavenge_oh_df(water0, input_water = "not_a_column"))
  expect_error(scavenge_oh_df(water0, dose = "ten"))
  expect_error(scavenge_oh_df(water0, kdoc_adjust = "high"))
})
