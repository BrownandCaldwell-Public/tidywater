#dissolve_cu

test_that("DIC or alk is required for dissolve_cu", {
  water1 <- suppressWarnings(define_water(ph = 8, toc = 2.5, tot_po4 = 2))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 45, tot_po4 = 2))

  dissolved <- dissolve_cu(water2)

  expect_error(dissolve_cu(water1))
  expect_no_error(dissolve_cu(water2))
  expect_equal(signif(dissolved$tot_dissolved_cu, 2), 0.33)
})

test_that("pH is required", {
  water <- suppressWarnings(define_water(alk = 60, tds = 200, tot_po4 = 2))

  expect_error(dissolve_cu(water))
})

#add a test for warnings for tot_po4 and pH ranges
test_that("warning when po4 is zero", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 60, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 4, alk = 60, tot_po4 = 2))
  
  expect_warning(dissolve_cu(water1))
  expect_warning(dissolve_cu(water2))
})

#test that dissolve_cu works
test_that("dissolve_cu works.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 100, tds = 200, so4 = 120, cl = 50, tot_po4 = 2)) %>%
    dissolve_cu()
  
  water2 <- suppressWarnings(define_water(ph = 8, alk = 100, temp = 25, cl = 100, tot_po4 = 2)) %>%
    dissolve_cu()
  
  water3 <- suppressWarnings(define_water(ph = 7, alk = 80, temp = 25, tds = 200, tot_po4=2)) %>%
    dissolve_cu()
  
  water4 <- suppressWarnings(define_water(ph = 7, alk = 100, temp = 25, ca = 100, tot_po4 = 5)) %>%
    dissolve_cu()
  
  expect_equal(signif(water1$tot_dissolved_cu, 2), 1.2)
  expect_equal(signif(water2$tot_dissolved_cu, 2), 0.52)
  expect_equal(signif(water3$tot_dissolved_cu, 2), 1.1)
  expect_equal(signif(water4$tot_dissolved_cu, 2), 0.68)
})

################################################################################*
################################################################################*
# dissolve_cu helper ----
# Check dissolve_cu_once outputs are the same as base function, dissolve_cu

test_that("dissolve_cu_once outputs are the same as base function, dissolve_cu", {
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50,
    ca = 13, mg = 4, na = 20, k = 20, cl = 30, tot_po4 = 2, tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    balance_ions() %>%
    dissolve_cu()
  
  water2 <- suppressWarnings(water_df %>%
                               mutate(tot_po4 = 2) %>%
                               slice(1) %>%
                               define_water_chain() %>%
                               balance_ions_chain() %>%
                               dissolve_cu_once(input_water = "balanced_water"))
  
  expect_equal(water1$tot_dissolved_cu, water2$tot_dissolved_cu)
})

# Check that output column is numeric

test_that("dissolve_cu_once outputs data frame", {
  water1 <- suppressWarnings(water_df %>%
                               mutate(tot_po4 = 2) %>%
                               define_water_chain() %>%
                               balance_ions_chain() %>%
                               dissolve_cu_once(input_water = "balanced_water"))
  
  expect_true(is.numeric(water1$tot_dissolved_cu))
})