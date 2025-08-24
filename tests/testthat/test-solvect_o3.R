library(dplyr)

test_that("solvect_o3 returns 0's for all outputs when time is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ozone <- solvect_o3(water1, time = 0, dose = 3, baffle = .2)

  expect_equal(ozone$ct_actual, 0)
  expect_equal(ozone$glog_removal, 0)
  expect_equal(ozone$vlog_removal, 0)
  expect_equal(ozone$clog_removal, 0)
  expect_error(solvect_o3(water1, dose = 3, baffle = .2))
})

test_that("solvect_o3 returns 0 for all outputs when dose is 0 or error when dose is missing", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ozone <- solvect_o3(water1, time = 10, dose = 0, baffle = .2)

  expect_equal(ozone$ct_actual, 0)
  expect_equal(ozone$glog_removal, 0)
  expect_equal(ozone$vlog_removal, 0)
  expect_equal(ozone$clog_removal, 0)
  expect_error(solvect_o3(water1, time = 10, baffle = .2))
})

test_that("solvect_o3 returns 0's for all outputs when baffle is 0 or missing", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ozone <- solvect_o3(water1, time = 10, dose = 3, baffle = 0)
  expect_equal(ozone$ct_actual, 0)
  expect_equal(ozone$glog_removal, 0)
  expect_equal(ozone$vlog_removal, 0)
  expect_equal(ozone$clog_removal, 0)
  expect_error(solvect_o3(water1, dose = 3, time = 10))
})

test_that("solvect_o3 throws an error when kd is 0", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  expect_error(solvect_o3(water1, time = 10, kd = 0, dose = 3, baffle = .2), "kd must be less than zero for decay curve")
})

test_that("solvect_o3 fails without ph, temp, alk, doc, uv, and br.", {
  water_ph <- suppressWarnings(define_water(alk = 50, toc = 5, uv254 = .1, br = 50))
  water_temp <- suppressWarnings(define_water(ph = 7.5, temp = NA_real_, alk = 50, toc = 5, uv254 = .1, br = 50))
  water_alk <- suppressWarnings(define_water(ph = 7.5, toc = 5, uv254 = .1, br = 50))
  water_doc <- suppressWarnings(define_water(ph = 7.5, alk = 50, uv254 = .1, br = 50))
  water_uv <- suppressWarnings(define_water(ph = 7.5, alk = 50, toc = 5, br = 50))
  water_br <- suppressWarnings(define_water(ph = 7.5, alk = 50, toc = 5, uv254 = .1))

  expect_error(solvect_o3(water_ph, time = 10, dose = 3, baffle = .5))
  expect_error(solvect_o3(water_temp, time = 10, dose = 3, baffle = .5))
  expect_error(solvect_o3(water_alk, time = 10, dose = 3, baffle = .5))
  expect_error(solvect_o3(water_doc, time = 10, dose = 3, baffle = .5))
  expect_error(solvect_o3(water_uv, time = 10, dose = 3, baffle = .5))
  expect_error(solvect_o3(water_br, time = 10, dose = 3, baffle = .5))
})

test_that("solvect_o3 works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, alk = 30, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  ozone <- solvect_o3(water1, time = 30, dose = 5, baffle = 0.3)
  ozone2 <- solvect_o3(water1, time = 30, dose = 5, baffle = 0.3, kd = -0.5)


  expect_equal(round(ozone$ct_actual, 2), 9.84)
  expect_equal(round(ozone$glog_removal, 2), 42.67)
  expect_equal(round(ozone$vlog_removal, 2), 86.93)
  expect_equal(round(ozone$clog_removal, 2), 2.51)

  expect_equal(round(ozone2$ct_actual, 2), 2.34)
  expect_equal(round(ozone2$glog_removal, 2), 10.13)
  expect_equal(round(ozone2$vlog_removal, 2), 20.64)
  expect_equal(round(ozone2$clog_removal, 2), 0.60)
})

# HELPERS ----
test_that("solvect_o3_df outputs are the same as base function, solvect_o3", {
  testthat::skip_on_cran()
  water0 <- define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13, mg = 4,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )
  water1 <- water0 %>%
    solvect_o3(time = 10, dose = 5, kd = -0.5, baffle = .7)

  water2 <- water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_df() %>%
    solvect_o3_df(time = 10, dose = 5, kd = -0.5, baffle = .7)

  kds <- data.frame(kd = seq(-.5, -.1, .1))
  doses <- data.frame(O3Dose = seq(1, 4, 1))
  water3 <- water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_df() %>%
    merge(kds) %>%
    merge(doses) %>%
    solvect_o3_df(time = 10, dose = O3Dose, baffle = .7)

  water4 <- solvect_o3(water0, dose = 2, time = 10, kd = -.3, baffle = .7)

  expect_equal(water1$glog_removal, water2$defined_glog_removal)
  expect_equal(water4$ct_actual, water3$defined_ct_actual[8])
})

# Check that output is a data frame

test_that("solvect_o3_df is a data frame", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df[1, ] %>%
    mutate(br = 50) %>%
    define_water_df() %>%
    solvect_o3_df(time = 10, dose = 5, kd = -0.5, baffle = .7))


  expect_true(is.data.frame(water1))
})

# Check solvect_o3_df can use a column or function argument for chemical residual

test_that("solvect_o3_df can use a column and/or function argument for time and residual", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    slice(1:4) %>%
    define_water_df()

  time <- data.frame(time = seq(2, 10, 2))
  water1 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    merge(time) %>%
    solvect_o3_df(dose = 5, kd = -0.5, baffle = .7))

  water2 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    solvect_o3_df(
      time = seq(2, 10, 2),
      dose = 5, kd = -0.5, baffle = .7
    ) %>%
    unique())

  water3 <- water_df %>%
    define_water_df() %>%
    merge(time) %>%
    solvect_o3_df(dose = c(5, 8), kd = -0.5, baffle = .7)

  expect_equal(water1$defined_glog_removal, water2$defined_glog_removal) # test different ways to input time
  expect_equal(ncol(water3), ncol(water0) + 4 + 4)
  expect_equal(nrow(water3), 120) # joined correctly
})
