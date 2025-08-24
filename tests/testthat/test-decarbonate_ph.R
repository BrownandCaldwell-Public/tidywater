test_that("Decarbonate errors when inputs are wrong", {
  water0 <- suppressWarnings(define_water(ph = 4, temp = 25, alk = 5))
  water1 <- suppressWarnings(define_water(temp = 25, alk = 5, toc = 5))

  expect_error(decarbonate_ph(co2_removed = .5))
  expect_error(decarbonate_ph(water = list(ph = 7, toc = 5), co2_removed = .5))
  expect_error(decarbonate_ph(water = water0, co2_removed = "50%"))
  expect_error(decarbonate_ph(water = water0, co2_removed = 50))
  expect_error(decarbonate_ph(water = water1, co2_removed = .5))
})

test_that("Decarbonate works", {
  water0 <- suppressWarnings(define_water(ph = 4, temp = 25, alk = 5, cond = 50))

  water1 <- water0 %>%
    decarbonate_ph(co2_removed = .95)

  expect_equal(round(water1@ph, 1), 5.0)
  expect_equal(water0@toc, water1@toc)
  expect_s4_class(water1, "water")
})


test_that("Decarbonate df takes and returns correct argument types and classes.", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df("test")

  water1 <- decarbonate_ph_df(water0, "test", "decarb", co2_removed = .9)
  water2 <- water0 %>%
    dplyr::mutate(removal = .9) %>%
    decarbonate_ph_df("test", "decarb", co2_removed = removal)

  expect_error(decarbonate_ph_df(water_df, co2_removed = .9))
  expect_error(decarbonate_ph_df(water0))
  expect_s4_class(water1$decarb[[1]], "water")
  expect_equal(water1$decarb, water2$decarb)
})

test_that("Decarbonate df pluck_cols works the same as pluck_water.", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df("test")

  water1 <- decarbonate_ph_df(water0, "test", "decarb", co2_removed = .9) %>%
    pluck_water(c("decarb"), c("ph", "alk"))
  water2 <- decarbonate_ph_df(water0, "test", "decarb", co2_removed = .9, pluck_cols = TRUE)

  expect_equal(water1$decarb_ph, water2$decarb_ph)
  expect_equal(water1$decarb_alk, water2$decarb_alk)
  expect_equal(ncol(water1), ncol(water2))
})
