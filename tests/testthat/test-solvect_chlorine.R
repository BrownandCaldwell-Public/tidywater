
test_that("solvect_chlorine returns 0's for ct_actual and giardia log when time is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct <- solvect_chlorine(water1, time = 0, residual = 5, baffle = .2)

  expect_equal(ct$ct_actual, 0)
  expect_equal(ct$glog_removal, 0)
  expect_error(solvect_chlorine(water1, residual = 5, baffle = .5))
})

test_that("solvect_chlorine returns 0's for ct_actual and giardia log when residual is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct <- solvect_chlorine(water1, time = 30, residual = 0, baffle = .2)

  expect_equal(ct$ct_actual, 0)
  expect_equal(ct$glog_removal, 0)
  expect_error(solvect_chlorine(water1, time = 30, baffle = .5))
})

test_that("solvect_chlorine returns 0's for ct_actual and giardia log when baffle is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct <- solvect_chlorine(water1, time = 30, residual = 5, baffle = 0)

  expect_equal(ct$ct_actual, 0)
  expect_equal(ct$glog_removal, 0)
  expect_error(solvect_chlorine(water1, time = 30, residual = 5))
})

test_that("solvect_chlorine fails without ph and temp.", {
  water_temp <- suppressWarnings(define_water(ph = 7.5, temp = NA_real_))
  water_ph <- suppressWarnings(define_water(temp = 30))

  expect_error(solvect_chlorine(water_temp, time = 30, residual = 5, baffle = 0.2))
  expect_error(solvect_chlorine(water_ph, time = 30, residual = 5, baffle = 0.2))
})

test_that("solvect_chlorine works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  ct <- solvect_chlorine(water1, time = 30, residual = 5, baffle = 0.3)


  expect_equal(round(ct$ct_required, 2), 18.52)
  expect_equal(round(ct$ct_actual), 45)
  expect_equal(round(ct$glog_removal, 2), 1.21)
})
