# Chemdose DBP ----

test_that("chemdose_dbp returns no modeled DBPs when chlorine dose is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  dbps <- suppressWarnings(chemdose_dbp(water1, cl2 = 0, time = 8))

  expect_equal(dbps@tthm, 0)
})

test_that("chemdose_dbp does not run when water_type isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1, br = 30))

  expect_error(chemdose_dbp(water1, water_type = "raw"))
  expect_error(chemdose_dbp(water1, water_type = treated))
})

test_that("chemdose_dbp warns when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 30))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = .1, uv254 = 0.01, br = 30))
  water3 <- suppressWarnings(define_water(ph = 8, temp = 20, toc = 3, uv254 = 0.1, br = 30))
  water4 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3, uv254 = 0.1, br = 2))

  expect_warning(chemdose_dbp(water1, cl2 = 1, time = 8)) # chlorine out of bounds
  expect_warning(chemdose_dbp(water1, cl2 = 4, time = 1)) # time out of bounds
  expect_warning(chemdose_dbp(water2, cl2 = 2, time = 8, treatment = "gac")) # toc out of bounds
  expect_warning(chemdose_dbp(water3, cl2 = 4, time = 8, treatment = "coag")) # ph not set to 7.5
  expect_warning(chemdose_dbp(water4, cl2 = 4, time = 8)) # br out of bounds
})

test_that("chemdose_dbp stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1, br = 5))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, br = 50))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2, br = NA_real_))
  water5 <- suppressWarnings(define_water(ph = 8, temp = 25, toc = 3, uv = 0.2, br = 50))

  expect_error(chemdose_dbp(water1, cl2 = 4, time = 8)) # missing ph
  expect_error(chemdose_dbp(water2, cl2 = 4, time = 8)) # missing toc
  expect_error(chemdose_dbp(water3, cl2 = 4, time = 1, treatment = "coag")) # missing uv
  expect_no_error(suppressWarnings(chemdose_dbp(water3, cl2 = 4, time = 1, treatment = "raw"))) # raw doesn't require uv
  expect_error(chemdose_dbp(water4, cl2 = 4, time = 8)) # missing br
  expect_error(chemdose_dbp(water5, time = 8)) # missing cl2
  expect_error(chemdose_dbp(water5, cl2 = 4)) # missing time
})


test_that("chemdose_dbp works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- chemdose_dbp(water1, cl2 = 3, time = 8)
  water3 <- chemdose_dbp(water1, cl2 = 3, time = 8, treatment = "coag")
  water4 <- chemdose_dbp(water1, cl2 = 3, time = 72, treatment = "coag", location = "ds")
  water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 1, uv254 = 0.04, br = 50))
  water6 <- chemdose_dbp(water5, cl2 = 3, time = 8, treatment = "gac")

  expect_equal(round(water2@tthm), 68)
  expect_equal(round(water3@tthm), 59)
  expect_equal(round(water3@haa5), 48)
  expect_equal(round(water4@haa5), 69)
  expect_equal(round(water6@haa5), 12)
})
