# opensys_ph ----

test_that("opensys_ph preserves carbonate balance", {
  water1 <- define_water(ph = 7, alk = 10)
  water2 <- opensys_ph(water1)
  
  expect_equal(water2@tot_co3, sum(water2@h2co3, water2@hco3, water2@co3))
  expect_equal(water2@h2co3, 10^-5)
})