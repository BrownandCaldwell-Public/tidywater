# Chemdose TOC ----

test_that("chemdose_dbp returns no modeled DBPs when chlorine dose is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2))
  dbps <-   suppressWarnings(chemdose_dbp(water1, cl2 = 0, br = 30, time = 8))

  expect_equal(dbps$modeled_dbp_ug.L, 0)
})

test_that("chemdose_dbp does not run when water_type or species isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))
  
  expect_error(chemdose_dbp(water1, water_type = "raw" ))
  expect_error(chemdose_dbp(water1, water_type = treated ))
  expect_error(chemdose_dbp(water1, species = "tthms" ))
  expect_error(chemdose_dbp(water1, species = haa5 ))
})

test_that("chemdose_dbp warns when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7.5, toc = .1, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, uv254 = 0.1))

  expect_warning(chemdose_dbp(water1, cl2 = 1, br = 50, time = 8))
  expect_warning(chemdose_dbp(water1, cl2 = 4, br = 5, time = 8))
  expect_warning(chemdose_dbp(water1, cl2 = 4, br = 50, time = 1))
  expect_warning(chemdose_dbp(water2, cl2 = 4, br = 50, time = 8, water_type = "untreated"))
  expect_warning(chemdose_dbp(water3, cl2 = 4, br = 50, time = 8))
})

test_that("chemdose_dbp stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2))
  
  expect_error(chemdose_dbp(water1, cl2 = 1, br = 50, time = 8))
  expect_error(chemdose_dbp(water2, cl2 = 4, br = 5, time = 8))
  expect_error(chemdose_dbp(water3, cl2 = 4, br = 50, time = 1))
  expect_error(chemdose_dbp(water4, br = 50, time = 8))
  expect_error(chemdose_dbp(water4, cl2 = 4, time = 8))
  expect_error(chemdose_dbp(water4, cl2 = 4, br = 50))
  
})


test_that("chemdose_dbp works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5, uv254 = 0.1))
  water2 <- chemdose_dbp(water1, cl2 = 3, br = 50, time = 8)
  water3 <- chemdose_dbp(water1, cl2 = 3, br = 50, time = 8, water_type = "untreated")
  water4 <- chemdose_dbp(water1, cl2 = 3, br = 50, time = 8, species = c("tthm", "haa5", "haa6", "haa9"))

  expect_equal(round(water2$modeled_dbp_ug.L), 59)
  expect_equal(round(water3$modeled_dbp_ug.L), 68)
  expect_equal(length(water4$modeled_dbp_ug.L), 4)
  
})



