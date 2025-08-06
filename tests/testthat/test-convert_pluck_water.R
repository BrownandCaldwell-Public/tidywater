# convert_water ----

# Test convertWater converts a water class input to a dataframe
test_that("convert water creates a dataframe", {
  testthat::skip_on_cran()
  water1 <- define_water(
    ph = 6.7, temp = 20, alk = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10,
    cl = 10, so4 = 10, toc = 3.5, doc = 3.2, uv254 = 0.1
  )
  df_water <- convert_water(water1)
  expect_true(is.data.frame(df_water))
})

test_that("convert water works", {
  water1 <- define_water(
    ph = 6.7, temp = 20, alk = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10,
    cl = 10, so4 = 10, toc = 3.5, doc = 3.2, uv254 = 0.1
  )
  df_water <- convert_water(water1)
  expect_equal(water1@ph, df_water$ph)
  expect_equal(water1@tot_co3, df_water$tot_co3)
})

test_that("convert water mg works", {
  water1 <- suppressWarnings(define_water(
    ph = 6.7, temp = 20, alk = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10,
    cl = 10, so4 = 50, tot_po4 = 3.2, tot_nh3 = 0.54, free_chlorine = 2.1
  ))
  df_water <- convert_watermg(water1)
  expect_equal(6.7, df_water$ph)
  expect_equal(10, df_water$na)
  expect_equal(50, df_water$so4)
  expect_equal(3.2, df_water$tot_po4)
  expect_equal(0.54, df_water$tot_nh3)
  expect_equal(2.1, df_water$free_chlorine)
})

################################################################################*
################################################################################*
# pluck_waters----
test_that("pluck_water works", {
  water1 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    pluck_water(parameter = "tot_co3"))

  tot_co3_water <- purrr::pluck(water1, 1, 4)
  tot_co3_pluck <- water1 %>%
    dplyr::slice(4)

  water2 <- water_df %>%
    dplyr::slice(4) %>%
    define_water_df() %>%
    balance_ions_df() %>%
    pluck_water(input_water = c("defined", "balanced"), parameter = c("na", "cl", "ph"))

  expect_equal(ncol(water1), 2)
  expect_equal(tot_co3_water@tot_co3, tot_co3_pluck$defined_tot_co3)
  expect_equal(ncol(water2), 8)
  expect_equal(water_df$ph[4], water2$defined_ph[1])
  expect_failure(expect_equal(water2$defined_na, water2$balanced_na)) # check that Na is being plucked from 2 different waters
})

test_that("pluck_water inputs must be waters and water slots", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    define_water_df("raw") %>%
    transform(ohno = "not a water")
  water2 <- water_df

  expect_error(water1 %>% pluck_water("raw", c("oops", "ca")))
  expect_error(water2 %>% pluck_water("na", c("na", "ca")))
  expect_error(water1 %>% pluck_water(c("raw", "ohno"), c("na", "ca")))
})

test_that("pluck_water all works", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    define_water_df("raw") %>%
    chemdose_toc_df("raw", "coag", alum = 10) %>%
    pluck_water(c("raw", "coag"), "all")

  water2 <- water_df %>%
    define_water_df("raw") %>%
    chemdose_toc_df("raw", "coag", alum = 10) %>%
    pluck_water(c("raw", "coag"), c("doc", "hco3"))

  expect_equal(water1$raw_hco3, water2$raw_hco3)
  expect_equal(water1$coag_doc, water2$coag_doc)
})
