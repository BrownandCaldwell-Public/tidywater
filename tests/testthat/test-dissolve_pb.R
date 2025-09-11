# dissolve_pb----
library(dplyr)

test_that("dissolve_pb doesn't work without alkalinity and IS.", {
  water1 <- suppressWarnings(define_water(ph = 7, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 7, alk = 90))

  expect_error(dissolve_pb(water1))
  expect_error(dissolve_pb(water2))
})

test_that("dissolve_pb outputs total lead with various inputs for ionic strength", {
  water1 <- suppressWarnings(define_water(ph = 8, alk = 200, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 90, cond = 500))
  water3 <- suppressWarnings(define_water(ph = 8, alk = 90, tot_hard = 110, cl = 200))

  dissolved1 <- dissolve_pb(water1)
  dissolved2 <- dissolve_pb(water2)
  dissolved3 <- dissolve_pb(water3)

  expect_equal(signif(dissolved1$tot_dissolved_pb, 2), 1.1e-6)
  expect_equal(signif(dissolved2$tot_dissolved_pb, 2), 1.1e-6)
  expect_equal(signif(dissolved3$tot_dissolved_pb, 2), 1.1e-6)
})

test_that("dissolve_pb works.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 100, tds = 200, so4 = 120, cl = 50, tot_hard = 90)) %>%
    dissolve_pb()

  water2 <- suppressWarnings(define_water(ph = 7, alk = 100, temp = 25, cl = 100, tot_po4 = 2, so4 = 100, tot_hard = 50)) %>%
    dissolve_pb()

  water3 <- suppressWarnings(define_water(ph = 7, alk = 100, temp = 25, tds = 200)) %>%
    dissolve_pb(hydroxypyromorphite = "Zhu", pyromorphite = "Xie", laurionite = "Lothenbach")

  # starting wq in the app, except for pH. Raised pH here because this was outputting different
  # controlling solids when code had errors
  water4 <- suppressWarnings(define_water(
    ph = 8.5, temp = 25, alk = 100, tot_hard = 150, na = 25, k = 25, cl = 20,
    so4 = 40, cond = 500, toc = 3, doc = 3.2, uv = 0.07, br = 50, f = 2,
    fe = 5, mn = 10, al = 2
  )) %>%
    dissolve_pb()

  expect_equal(signif(water1$tot_dissolved_pb, 2), 1.2e-6)
  expect_equal(water1$controlling_solid, "Cerussite")

  expect_equal(signif(water2$tot_dissolved_pb, 2), 1.1e-8)
  expect_equal(water2$controlling_solid, "Pyromorphite")

  expect_equal(signif(water3$tot_dissolved_pb, 2), 1.2e-6)
  expect_equal(water3$controlling_solid, "Cerussite")
  expect_equal(water4$controlling_solid, "Hydrocerussite")
})

################################################################################*
################################################################################*
# dissolve_pb helper ----
# Check dissolve_pb_df outputs are the same as base function, dissolve_pb

test_that("dissolve_pb_df outputs are the same as base function, dissolve_pb", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50,
    ca = 13, mg = 4, na = 20, k = 20, cl = 30, so4 = 20, tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    dissolve_pb()

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    dissolve_pb_df()

  expect_equal(water1$tot_dissolved_pb, water2$defined_pb)
  expect_equal(water1$controlling_solid, water2$defined_controlling_solid)
})

# Check that output column is numeric

test_that("dissolve_pb_df outputs data frame", {
  testthat::skip_on_cran()
  water2 <- water_df %>%
    define_water_df() %>%
    dissolve_pb_df()

  expect_true(is.numeric(water2$defined_pb))
  expect_true(is.character(water2$defined_controlling_solid))
})

# Check that outputs are different depending on selected source
test_that("dissolve_pb_df processes different input constants", {
  testthat::skip_on_cran()
  water2 <- water_df %>%
    slice(3) %>%
    define_water_df() %>%
    dissolve_pb_df(water_prefix = F)

  water3 <- water_df %>%
    slice(3) %>%
    define_water_df() %>%
    dissolve_pb_df(pyromorphite = "Xie", water_prefix = F)

  expect_equal(water2$controlling_solid, water3$controlling_solid)
  expect_error(expect_equal(water2$pb, water3$pb))
})

# Check that the function stops due to errors in selected source
test_that("dissolve_pb_df errors work", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    define_water_df()

  expect_error(dissolve_pb_df(water1, hydroxypyromorphite = "schock"))
  expect_error(dissolve_pb_df(water1, pyromorphite = "Schock"))
  expect_error(dissolve_pb_df(water1, laurionite = "Lothebach"))
})
