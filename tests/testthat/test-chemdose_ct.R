# Chemdose ct tests here

test_that("chemdose_ct returns 0's for ct_actual and giardia log when time is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct <- chemdose_ct(water1, time = 0, residual = 5, baffle = .2)

  expect_equal(ct$ct_actual, 0)
  expect_equal(ct$glog_removal, 0)
  expect_error(chemdose_ct(water1, residual = 5, baffle = .5))
})

test_that("chemdose_ct returns 0's for ct_actual and giardia log when residual is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct <- chemdose_ct(water1, time = 30, residual = 0, baffle = .2)

  expect_equal(ct$ct_actual, 0)
  expect_equal(ct$glog_removal, 0)
  expect_error(chemdose_ct(water1, time = 30, baffle = .5))
})

test_that("chemdose_ct returns 0's for ct_actual and giardia log when baffle is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct <- chemdose_ct(water1, time = 30, residual = 5, baffle = 0)

  expect_equal(ct$ct_actual, 0)
  expect_equal(ct$glog_removal, 0)
  expect_error(chemdose_ct(water1, time = 30, residual = 5))
})

test_that("chemdose_ct fails without ph and temp.", {
  water_temp <- suppressWarnings(define_water(ph = 7.5, temp = NA_real_))
  water_ph <- suppressWarnings(define_water(temp = 30))

  expect_error(chemdose_ct(water_temp, time = 30, residual = 5, baffle = 0.2))
  expect_error(chemdose_ct(water_ph, time = 30, residual = 5, baffle = 0.2))
})

test_that("chemdose_ct works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  ct <- chemdose_ct(water1, time = 30, residual = 5, baffle = 0.3)


  expect_equal(round(ct$ct_required, 2), 18.52)
  expect_equal(round(ct$ct_actual), 45)
  expect_equal(round(ct$glog_removal, 2), 1.21)
})


# HELPERS ----
test_that("chemdose_ct_once outputs are the same as base function, chemdose_ct", {
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )) %>%
    chemdose_ct(time = 30, residual = 5, baffle = .7)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_ct_once(time = 30, residual = 5, baffle = .7))

  expect_equal(water1$ct_required, water2$defined_water_ct_required)
})

# Check that output is a data frame

test_that("chemdose_ct_once is a data frame", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_ct_once(time = 30, residual = 5))


  expect_true(is.data.frame(water1))
})

# Check chemdose_ct_once can use a column or function argument for chemical residual

test_that("chemdose_ct_once can use a column and/or function argument for time and residual", {
  water0 <- water_df %>%
    define_water_chain()

  time <- data.frame(time = seq(2, 24, 2))
  water1 <- suppressWarnings(water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    cross_join(time) %>%
    chemdose_ct_once(residual = 5, baffle = .5))

  water2 <- suppressWarnings(water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_ct_once(
      time = seq(2, 24, 2),
      residual = 5, baffle = .5
    ) %>%
    unique())

  water3 <- water_df %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    cross_join(time) %>%
    chemdose_ct_once(residual = c(5, 8))

  expect_equal(water1$defined_water_ct_required, water2$defined_water_ct_required) # test different ways to input time
  expect_equal(ncol(water3), ncol(water0) + 5) # adds cols for time, residual, and ct_actual, ct_req, glog_removal
  expect_equal(nrow(water3), 288) # joined correctly
})
