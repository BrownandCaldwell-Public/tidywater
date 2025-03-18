test_that("solvect_chlorine returns 0's for ct_actual and giardia log when arguments are 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct1 <- solvect_chlorine(water1, time = 0, residual = 5, baffle = .5)
  ct2 <- solvect_chlorine(water1, time = 30, residual = 0, baffle = .5)
  ct3 <- solvect_chlorine(water1, time = 30, residual = 5, baffle = 0)

  expect_equal(ct1$ct_actual, 0)
  expect_equal(ct2$glog_removal, 0)
  expect_equal(ct3$glog_removal, 0)
})

test_that("solvect_chlorine errors when arguments are missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))

  expect_error(solvect_chlorine(water1, time = 30, baffle = .5))
  expect_error(solvect_chlorine(water1, residual = 5, baffle = .5))
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

# HELPERS ----
test_that("solvect_chlorine_once outputs are the same as base function, solvect_chlorine", {
  water1 <- suppressWarnings(define_water(
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
    uv254 = 0.05,
    br = 50
  )) %>%
    solvect_chlorine(time = 30, residual = 5, baffle = .7)

  water2 <- suppressWarnings(
    water_df %>%
      slice(1) %>%
      mutate(br = 50) %>%
      define_water_chain() %>%
      solvect_chlorine_once(time = 30, residual = 5, baffle = .7)
  )

  expect_equal(water1$ct_required, water2$defined_water_ct_required)
})

# Check that output is a data frame

test_that("solvect_chlorine_once is a data frame", {
  water1 <- suppressWarnings(
    water_df %>%
      slice(1) %>%
      mutate(br = 50) %>%
      define_water_chain() %>%
      solvect_chlorine_once(time = 30, residual = 5, baffle = .5)
  )

  expect_true(is.data.frame(water1))
})

# Check solvect_chlorine_once can use column or function arguments

test_that("solvect_chlorine_once can use a column and/or function argument for time, residual, baffle", {
  water0 <- water_df %>%
    define_water_chain()

  time <- data.frame(time = seq(2, 24, 2))
  water1 <- suppressWarnings(
    water_df %>%
      mutate(br = 50) %>%
      define_water_chain() %>%
      cross_join(time) %>%
      solvect_chlorine_once(residual = 5, baffle = .5)
  )

  water2 <- suppressWarnings(
    water_df %>%
      mutate(br = 50) %>%
      define_water_chain() %>%
      solvect_chlorine_once(
        time = seq(2, 24, 2),
        residual = 5,
        baffle = .5
      ) %>%
      unique()
  )

  water3 <- water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    cross_join(time) %>%
    rename(ChlorTime = time) %>%
    solvect_chlorine_once(residual = c(5, 8), baffle = .5, time = ChlorTime)

  expect_equal(water1$defined_water_ct_required, water2$defined_water_ct_required) # test different ways to input time
  expect_equal(ncol(water3), ncol(water0) + 6) # adds cols for time, residual, baffle, and ct_actual, ct_req, glog_removal
  expect_equal(nrow(water3), 288) # joined correctly
})

test_that("solvect_chlorine_once correctly handles arguments with multiple values", {
  water <- water_df %>%
    slice(1:2) %>%
    define_water_chain()

  water1 <- water %>%
    solvect_chlorine_once(time = c(5, 10), residual = c(1, 2, 5), baffle = 0.5)
  water2 <- water %>%
    solvect_chlorine_once(time = 5, residual = c(2, 5), baffle = c(.5, .8))

  expect_equal(nrow(water) * 6, nrow(water1))
  expect_equal(nrow(water) * 4, nrow(water2))
})
