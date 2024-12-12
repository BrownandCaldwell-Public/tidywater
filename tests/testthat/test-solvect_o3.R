
test_that("solvect_o3 returns 0's for all outputs when time is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ozone <- solvect_o3(water1, time = 0, dose = 3, baffle = .2)

  expect_equal(ozone$ct_actual, 0)
  expect_equal(ozone$glog_removal, 0)
  expect_equal(ozone$vlog_removal, 0)
  expect_equal(ozone$clog_removal, 0)
  expect_error(solvect_o3(water1, dose = 3, baffle = .2))
})

test_that("solvect_o3 returns NaNs for all outputs when dose is 0 or missing", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ozone <- solvect_o3(water1, time = 10, dose = 0, baffle = .2)

  expect_equal(ozone$ct_actual, NaN)
  expect_equal(ozone$glog_removal, NaN)
  expect_equal(ozone$vlog_removal, NaN)
  expect_equal(ozone$clog_removal, NaN)
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

test_that("solvect_o3 returns NaNs for all outputs when kd is 0", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ozone <- solvect_o3(water1, time = 10, kd = 0, dose = 3, baffle = .2)
  expect_equal(ozone$ct_actual, NaN)
  expect_equal(ozone$glog_removal, NaN)
  expect_equal(ozone$vlog_removal, NaN)
  expect_equal(ozone$clog_removal, NaN)
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
