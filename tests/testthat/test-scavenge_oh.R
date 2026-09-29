test_that("scavenger works with alk params", {
  water_alk <- suppressWarnings(define_water(ph = 7, doc = 3, alk = 70, no3 = 4))
  water_no_alk <- suppressWarnings(define_water(ph = 7, doc = 3, no3 = 4))

  SC_alk <- scavenge_oh(water_alk, dose = 0)
  #no alk
  SC_base <- scavenge_oh(water_no_alk, dose = 0)

  # no negative scavenging!
  expect_true(SC_base >0)
  expect_true(SC_alk >0)

  # alk contributes to scavenging via HCO3/CO3, so SC should be higher with alk
  expect_true(SC_alk > SC_base)

  # adding h2o2 increases scavenging
  sc_alk_h2o2 <- scavenge_oh(water_alk, dose = 10)
  expect_true(sc_alk_h2o2 > SC_alk)

  # numeric tests - values derived from UV-H2O2 spreadsheet tool
  expect_equal(round(SC_alk), 87162)
  expect_equal(round(SC_base), 75029)
  expect_equal(round(sc_alk_h2o2), 95103)

})


test_that("changing scavenger's rate constant for DOC works", {
  # water with experimental params
  water1 <- suppressWarnings(define_water(ph=7, doc=3, alk = 70, no3 = 4))
  h2o2_dose<-20

  background_SC <- scavenge_oh(water1, h2o2_dose)
  background_SC_adjusted <- scavenge_oh(water1, h2o2_dose, 48065)

  expect_error(expect_equal(background_SC, background_SC_adjusted))
})
