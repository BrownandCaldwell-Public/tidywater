# Bromate formation ----

test_that("ozonate_bromate returns no modeled bromate when ozone dose is 0 or time is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  bromate1 <- suppressWarnings(ozonate_bromate(water1, dose = 0, time = 10, model = "Sohn"))
  bromate2 <- suppressWarnings(ozonate_bromate(water1, dose = 2, time = 0, model = "Sohn"))

  expect_equal(bromate1@bro3, 0)
  expect_equal(bromate2@bro3, 0)
})

test_that("ozonate_bromate does not run when model isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1, br = 30))

  expect_error(ozonate_bromate(water1, model = "oops"))
  expect_error(ozonate_bromate(water1, water_type = 5))
})

test_that("ozonate_bromate stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1, br = 5))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, br = 50))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2, br = NA_real_))
  water5 <- suppressWarnings(define_water(ph = 8, temp = 25, toc = 3, uv = 0.2, br = 50))

  expect_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Ozekin")) # missing ph
  expect_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Ozekin")) # missing toc
  expect_error(ozonate_bromate(water4, dose = 4, time = 8, model = "Ozekin")) # missing br
  expect_error(ozonate_bromate(water5, time = 8, model = "Ozekin")) # missing dose
  expect_error(ozonate_bromate(water5, dose = 4, model = "Ozekin")) # missing time
})

test_that("ozonate_bromate stops working when models don't line up with ammonia inputs", {
  water1 <- suppressWarnings(define_water(ph = 8, alk = 50, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 50, toc = 3.5, uv254 = 0.1, br = 50, tot_nh3 = 2))

  expect_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Song")) # Song model requires ammonia
  expect_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Galey")) # Galey model does not use ammonia
  expect_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Siddiqui")) # Siddiqui model does not use ammonia

  expect_no_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Ozekin"))
  expect_no_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Ozekin"))

  expect_no_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Sohn"))
  expect_no_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Sohn"))
})


test_that("ozonate_bromate works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100, doc = 3.5, uv254 = 0.1, br = 50))
  water2 <- ozonate_bromate(water1, dose = 1, time = 10, model = "Ozekin")
  water3 <- ozonate_bromate(water1, dose = 1, time = 10, model = "Sohn")
  water4 <- ozonate_bromate(water1, dose = 1, time = 10, model = "Galey")

  water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100, doc = 3.5, uv254 = 0.1, br = 50, tot_nh3 = 1))
  water6 <- ozonate_bromate(water5, dose = 1, time = 10, model = "Song")
  water7 <- ozonate_bromate(water5, dose = 1, time = 10, model = "Ozekin")

  expect_equal(round(water2@bro3, 1), 1.3)
  expect_equal(round(water3@bro3, 1), 1.7)
  expect_equal(round(water4@bro3, 1), 2.4)
  expect_equal(round(water6@bro3, 1), 0.3)
  expect_equal(round(water7@bro3, 1), 1.2)
})
