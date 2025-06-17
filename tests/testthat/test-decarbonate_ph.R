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
  expect_equal(round(water1@alk, 2), 4.97)
  expect_equal(water1@h2co3, water0@h2co3 * .05)

})


test_that("Decarbonate works", {
  water0 <- define_water(ph = 4, temp = 25, alk = 5)
  water1 <- water0 %>%
    decarbonate_ph(co2_removed = .95)
})
