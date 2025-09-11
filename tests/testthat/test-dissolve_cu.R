# dissolve_cu

test_that("DIC or alk is required for dissolve_cu", {
  water1 <- suppressWarnings(define_water(ph = 8, toc = 2.5, tot_po4 = 2))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 45, tot_po4 = 2))

  dissolved <- dissolve_cu(water2)

  expect_error(dissolve_cu(water1))
  expect_no_error(dissolve_cu(water2))
  expect_equal(signif(dissolved$cu, 2), 0.49)
})

test_that("pH is required", {
  water <- suppressWarnings(define_water(alk = 60, tds = 200, tot_po4 = 2))

  expect_error(dissolve_cu(water))
})

# add a test for warnings for tot_po4 and pH ranges
test_that("warning when po4 is zero", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 60, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 4, alk = 60, tot_po4 = 2))

  expect_warning(dissolve_cu(water1))
  expect_warning(dissolve_cu(water2))
})

# test that dissolve_cu works
test_that("dissolve_cu works.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 100, tds = 200, so4 = 120, cl = 50, tot_po4 = 2)) %>%
    dissolve_cu()

  water2 <- suppressWarnings(define_water(ph = 8, alk = 100, temp = 25, cl = 100, tot_po4 = 2)) %>%
    dissolve_cu()

  water3 <- suppressWarnings(define_water(ph = 7, alk = 80, temp = 25, tds = 200, tot_po4 = 2)) %>%
    dissolve_cu()

  water4 <- suppressWarnings(define_water(ph = 7, alk = 100, temp = 25, ca = 100, tot_po4 = 0.5)) %>%
    dissolve_cu()

  expect_equal(signif(water1$cu, 2), 1.9)
  expect_equal(signif(water2$cu, 2), 0.78)
  expect_equal(signif(water3$cu, 2), 1.6)
  expect_equal(signif(water4$cu, 2), 1.9)
})

################################################################################*
################################################################################*
# dissolve_cu helper ----
# Check dissolve_cu_df outputs are the same as base function, dissolve_cu

test_that("dissolve_cu_df outputs are the same as base function, dissolve_cu", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50,
    ca = 13, mg = 4, na = 20, k = 20, cl = 30, tot_po4 = 2, tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    dissolve_cu()

  water2 <- suppressWarnings(water_df %>%
    dplyr::slice(1) %>%
    dplyr::mutate(tot_po4 = 2) %>%
    define_water_df() %>%
    dissolve_cu_df())

  expect_equal(water1$cu, water2$defined_cu)
})

# Check that output column is numeric

test_that("dissolve_cu_df outputs data frame", {
  testthat::skip_on_cran()
  water <- water_df %>%
    dplyr::mutate(tot_po4 = 2) %>%
    define_water_df() %>%
    dissolve_cu_df(water_prefix = FALSE)

  expect_true(is.numeric(water$cu))
})
