# convert_water ----

# Test convertWater converts a water class input to a dataframe
test_that("convert water creates a dataframe", {
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
  water1 <- define_water(
    ph = 6.7, temp = 20, alk = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10,
    cl = 10, so4 = 50
  )
  df_water <- convert_watermg(water1)
  expect_equal(6.7, df_water$ph)
  expect_equal(10, df_water$na)
  expect_equal(50, df_water$so4)
})

################################################################################*
################################################################################*
# pluck_waters----
test_that("pluck_water works", {
  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    pluck_water(parameter = "tot_co3"))

  tot_co3_water <- purrr::pluck(water1, 1, 4)
  tot_co3_pluck <- water1 %>% slice(4)

  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    pluck_water(input_water = c("defined_water", "balanced_water"), parameter = "na"))

  expect_equal(ncol(water1), 2)
  expect_equal(tot_co3_water@tot_co3, tot_co3_pluck$defined_water_tot_co3)
  expect_equal(ncol(water2), 4)
  expect_failure(expect_equal(water2$defined_water_na, water2$balanced_water_na)) # check that Na is being plucked from 2 different waters
})

test_that("pluck_water inputs must be waters and water slots", {
  water1 <- water_df %>%
    define_water_chain("raw") %>%
    mutate(ohno = "not a water")
  water2 <- water_df

  expect_error(water1 %>% pluck_water("raw", c("oops", "ca")))
  expect_error(water2 %>% pluck_water("na", c("na", "ca")))
  expect_error(water1 %>% pluck_water(c("raw", "ohno"), c("na", "ca")))
})
