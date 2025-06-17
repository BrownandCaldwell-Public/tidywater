test_that("Decarbonate works", {
  water0 <- define_water(ph = 4, temp = 25, alk = 5)
  water1 <- water0 %>%
    decarbonate_ph(co2_removed = .95)
})
