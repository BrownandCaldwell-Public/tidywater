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
  water0 <- suppressWarnings(define_water(ph = 4, temp = 25, alk = 5))
  water1 <- water0 %>%
    decarbonate_ph(co2_removed = .95)

  expect_equal(round(water1@ph, 1), 5.0)
  expect_equal(water0@toc, water1@toc)
  expect_s4_class(water1, "water")
})


test_that("Decarbonate chain takes and returns correct argument types and classes.", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_chain("test")

  water1 <- decarbonate_ph_chain(water0, "test", "decarb", co2_removed = .9)
  water2 <- water0 %>%
    mutate(removal = .9) %>%
    decarbonate_ph_chain("test", "decarb", co2_removed = removal)

  expect_error(decarbonate_ph_chain(water_df, co2_removed = .9))
  expect_error(decarbonate_ph_chain(water0))
  expect_s4_class(water1$decarb[[1]], "water")
  expect_equal(water1$decarb, water2$decarb)

})
