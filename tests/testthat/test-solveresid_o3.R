# ozone decay tests here
test_that("solveresid_o3 returns the input residual when time is 0, or an error when time is missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))


  expect_equal(solveresid_o3(water1, time = 0, dose = 2), 2)
  expect_error(solveresid_o3(water1, dose = 2))
})

test_that("solveresid_o3 returns NaN when dose is 0, or error when dose is missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))

  expect_equal(solveresid_o3(water1, time = 30, dose = 0), NaN)
  expect_error(solveresid_o3(water1, time = 30))
})

test_that("solveresid_o3 fails without ph, temp, alk, doc, uv, br.", {
  water_ph <- suppressWarnings(define_water(alk = 50, toc = 5, uv254 = .1, br = 50))
  water_temp <- suppressWarnings(define_water(ph =7.5, temp = NA_real_, alk = 50, toc = 5, uv254 = .1, br = 50))
  water_alk <- suppressWarnings(define_water(ph =7.5, toc = 5, uv254 = .1, br = 50))
  water_doc <- suppressWarnings(define_water(ph =7.5, alk = 50, uv254 = .1, br = 50))
  water_uv <- suppressWarnings(define_water(ph =7.5, alk = 50, toc = 5, br = 50))
  water_br <- suppressWarnings(define_water(ph =7.5, alk = 50, toc = 5, uv254 = .1))

  expect_error(solveresid_o3(water_ph, time = 30, dose = 3))
  expect_error(solveresid_o3(water_temp, time = 30, dose = 3))
  expect_error(solveresid_o3(water_alk, time = 30, dose = 3))
  expect_error(solveresid_o3(water_doc, time = 30, dose = 3))
  expect_error(solveresid_o3(water_uv, time = 30, dose = 3))
  expect_error(solveresid_o3(water_br, time = 30, dose = 3))
})

test_that("solveresid_o3 works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, alk = 20, toc = 3.5, uv254 = 0.1, br = 50))

  expect_equal(round(solveresid_o3(water1, time = 30, dose = 3), 2), .38)
})



test_that("solveresid_o3_once outputs are the same as base function, solveresid_o3", {
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05, br =50
  )) %>%
    balance_ions() %>%
    solveresid_o3(time = 30, dose = 5)

  water2 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               mutate(br =50) %>%
                               define_water_chain() %>%
                               balance_ions_chain() %>%
                               solveresid_o3_once(input_water = "balanced_water", time = 30, dose = 5))

  expect_equal(water1, water2$o3resid)
})

# Check that output is a data frame

test_that("solveresid_o3_once is a data frame", {
  water1 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               mutate(br =50) %>%
                               define_water_chain() %>%
                               balance_ions_chain() %>%
                               solveresid_o3_once(input_water = "balanced_water", time = 30, dose = 5))


  expect_true(is.data.frame(water1))
})

# Check solveresid_o3_once can use a column or function argument for chemical dose

test_that("solveresid_o3_once can use a column and/or function argument for time and dose", {
  water0 <- water_df %>%
    define_water_chain()

  time <- data.frame(time = seq(2, 24, 2))
  water1 <- suppressWarnings(water_df %>%
                               mutate(br =50) %>%
                               define_water_chain() %>%
                               balance_ions_chain() %>%
                               cross_join(time) %>%
                               solveresid_o3_once(input_water = "balanced_water", dose = 5))

  water2 <- suppressWarnings(water_df %>%
                               mutate(br =50) %>%
                               define_water_chain() %>%
                               balance_ions_chain() %>%
                               solveresid_o3_once(input_water = "balanced_water", time = seq(2, 24,2), dose =5) %>%
                               unique()
                             )

  water3 <- water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    cross_join(time) %>%
    solveresid_o3_once(dose = c(5, 8))

  expect_equal(water1$o3resid, water2$o3resid) # test different ways to input time
  expect_equal(ncol(water3), ncol(water0) + 3) # adds cols for time, dose, and o3resid
  expect_equal(nrow(water3), 288) # joined correctly
})

