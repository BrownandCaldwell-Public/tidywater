test_that("scavenger works with alk params", {
  water_alk <- suppressWarnings(define_water(ph = 7, doc = 3, alk = 70, no3 = 4))
  water_no_alk <- suppressWarnings(define_water(ph = 7, doc = 3, no3 = 4))

  SC_alk <- oh_scavenger(water_alk, dose = 0)
  #no alk
  SC_base <- suppressWarnings(oh_scavenger(water_no_alk, dose = 0))

  # no negative scavenging!
  expect_true(SC_base >0)
  expect_true(SC_alk >0)

  # alk contributes to scavenging via HCO3/CO3, so SC should be higher with alk
  expect_true(SC_alk > SC_base)

  # adding h2o2 increases scavenging
  sc_alk_h2o2 <- oh_scavenger(water_alk, dose = 20)
  expect_true(sc_alk_h2o2 > SC_alk)
})


test_that("changing scavenger's rate constant for DOC works", {
  # water with experimental params
  water1 <- suppressWarnings(define_water(ph=7, doc=3, alk = 70, no3 = 4))
 h2o2_dose<-20

  backround_SC <- oh_scavenger(water1, h2o2_dose)
  backround_SC_adjusted <- oh_scavenger(water1, h2o2_dose, 48065)

  expect_error(expect_equal(backround_SC, backround_SC_adjusted))
})
