test_that("solvect_chlorine returns 0's for ct_actual and giardia log when arguments are 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  ct1 <- suppressWarnings(solvect_chlorine(water1, time = 0, residual = 5, baffle = .5))
  ct2 <- suppressWarnings(solvect_chlorine(water1, time = 30, residual = 0, baffle = .5))
  ct3 <- suppressWarnings(solvect_chlorine(water1, time = 30, residual = 5, baffle = 0))

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

test_that("solvect_chlorine correctly uses free_chlorine slot", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50, free_chlorine = 1))
  ct <- solvect_chlorine(water1, time = 30, residual = 5, baffle = 0.3)
  ct_use <- suppressWarnings(solvect_chlorine(water1, time = 30, residual = 5, baffle = 0.3, free_cl_slot = "slot_only"))
  ct_use2 <- suppressWarnings(solvect_chlorine(water1, time = 30, baffle = 0.3, free_cl_slot = "slot_only")) # no residual argument
  ct_use3 <- solvect_chlorine(water1, time = 30, residual = 5, baffle = 0.3, free_cl_slot = "sum_with_residual")


  expect_error(expect_equal(round(ct$ct_required, 2), round(ct_use$ct_required, 2)))
  expect_equal(round(ct_use2$ct_required), 10)
  expect_equal(round(ct_use$ct_required, 2), round(ct_use2$ct_required, 2))
  expect_error(expect_equal(round(ct_use$ct_required, 2), round(ct_use3$ct_required, 2)))
  expect_equal(round(ct_use3$ct_required), 19)
  expect_error(solvect_chlorine(water1, time = 30, baffle = 0.3)) # no residual argument or water slot
})

test_that("solvect_chlorine works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  ct <- solvect_chlorine(water1, time = 30, residual = 5, baffle = 0.3)


  expect_equal(round(ct$ct_required, 2), 18.52)
  expect_equal(round(ct$ct_actual), 45)
  expect_equal(round(ct$glog_removal, 2), 1.21)
})

test_that("solvect_chlorine determines virus log removal", {
  water1 <- suppressWarnings(define_water(ph=7.5, temp=20, toc=3.5, uv254=0.1, br=50))
  water2 <- suppressWarnings(define_water(ph=7.5, temp=5, toc=3.5, uv254=0.1, br=50))
  water3 <- suppressWarnings(define_water(ph=7.5, temp=22, toc=3.5, uv254=0.1, br=50))
  water4 <- suppressWarnings(define_water(ph=10, temp=20, toc=3.5, uv254=0.1, br=50))
  water5 <- suppressWarnings(define_water(ph=9.5, temp=25, toc=3.5, uv254=0.1, br=50))

  ct1 <- solvect_chlorine(water1, time = 30, residual = 5, baffle = 0.3)
  ct2 <- solvect_chlorine(water2, time = 10, residual = 2, baffle = 0.3)

  expect_equal(ct1$vlog_removal, 4.0)
  expect_equal(ct2$vlog_removal, 3.0)
  expect_warning(solvect_chlorine(water3, time = 10, residual = 2, baffle = 0.3)) # estimated temp
  expect_warning(solvect_chlorine(water4, time = 10, residual = 2, baffle = 0.3)) # ph or time out of range
  expect_warning(solvect_chlorine(water5, time = 20, residual = 2, baffle = 0.3)) #estimated pH
})

test_that("solvect_chlorine warns appropriately about virus log removal", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 15, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- suppressWarnings(define_water(ph = 5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  water3 <- suppressWarnings(define_water(ph = 12, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))


  expect_warning(solvect_chlorine(water1, time = 0.5, residual = 1, baffle = 0.3)) # contact time out of range
  expect_warning(solvect_chlorine(water2, time = 30, residual = 5, baffle = 0.3)) # pH out of range
  expect_warning(solvect_chlorine(water3, time = 30, residual = 5, baffle = 0.3))
})

# HELPERS ----
test_that("solvect_chlorine_df outputs are the same as base function, solvect_chlorine", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )) %>%
    solvect_chlorine(time = 30, residual = 5, baffle = .7)

  water2 <- suppressWarnings(water_df[1,] %>%
    transform(br = 50) %>%
    define_water_df() %>%
    solvect_chlorine_df(time = 30, residual = 5, baffle = .7))

  expect_equal(water1$ct_required, water2$defined_ct_required)
})

# Check that output is a data frame

test_that("solvect_chlorine_df is a data frame", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df[1,] %>%
    transform(br = 50) %>%
    define_water_df() %>%
    solvect_chlorine_df(time = 30, residual = 5, baffle = .5))

  expect_true(is.data.frame(water1))
})

# Check solvect_chlorine_df can use column or function arguments

test_that("solvect_chlorine_df can use a column and/or function argument for time, residual, baffle", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df()

  time <- data.frame(time = seq(2, 24, 2))
  water1 <- water_df %>%
    transform(br = 50) %>%
    define_water_df() %>%
    merge(time) %>%
    suppressWarnings(solvect_chlorine_df(residual = 5, baffle = .5))

  water2 <- water_df %>%
    transform(br = 50) %>%
    define_water_df() %>%
    suppressWarnings(solvect_chlorine_df(
      time = seq(2, 24, 2),
      residual = 5, baffle = .5,
      water_prefix = FALSE
    ))

  water3 <- suppressWarnings(water_df %>%
    transform(br = 50) %>%
    define_water_df() %>%
    merge(time) %>%
    dplyr::rename(ChlorTime = time) %>%
    solvect_chlorine_df(residual = c(5, 8), baffle = .5, time = ChlorTime))

  expect_equal(water1$defined_ct_required, water2$ct_required) # test different ways to input time
  expect_equal(ncol(water3), ncol(water0) + 7) # adds cols for time, residual, baffle, and ct_actual, ct_req, glog_removal, vlog_removal
  expect_equal(nrow(water3), 288) # joined correctly
})

test_that("solvect_chlorine_df correctly handles arguments with multiple values", {
  testthat::skip_on_cran()
  water <- water_df[1:2,] %>%
    define_water_df()

  water1 <- water %>%
    solvect_chlorine_df(time = c(5, 10), residual = c(1, 2, 5), baffle = 0.5)
  water2 <- water %>%
    solvect_chlorine_df(time = 5, residual = c(2, 5), baffle = c(.5, .8))

  expect_equal(nrow(water) * 6, nrow(water1))
  expect_equal(nrow(water) * 4, nrow(water2))
})

test_that("solvect_chlorine_df correctly uses free_chlorine slot", {
  testthat::skip_on_cran()

  residual_df <-suppressWarnings(water_df %>%
    define_water_df() %>%
    chemdose_ph_df(naocl = 10) %>%
    solvect_chlorine_df(time = 30, residual = 5, baffle = 0.3))

  free_cl_slot_df <-suppressWarnings(water_df %>%
    define_water_df() %>%
    chemdose_ph_df(naocl = 10) %>%
    solvect_chlorine_df(time = 30, residual = 5, baffle = 0.3, free_cl_slot = "slot_only"))

  expect_error(expect_equal(residual_df$defined_ct_required, free_cl_slot_df$dosed_chem_ct_required))
})
