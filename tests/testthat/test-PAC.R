# PAC_TOC ----

test_that("pac_toc returns no modeled DOC value when PAC dose is 0.", {
  water1 <- suppressWarnings(define_water(doc=2.5, uv254=0.05, toc=3.5))
  water2 <- suppressWarnings(pac_toc(water1, type="wood", time = 18, dose=0))
  #expected value of doc when no PAC is added - would equal starting value
  expect_equal(water2@doc, water1@doc)
})

test_that("pac_toc defaults to bituminous when type isn't specified.", {
  water1 <- suppressWarnings(define_water(doc=2.5, uv254=0.05, toc=3.5))
  dosed1 <- pac_toc(water1, time = 18, dose = 5)
  dosed2 <- pac_toc(water1, type = "bituminous", time = 18, dose = 5)
  expect_equal(dosed1@doc, dosed2@doc)
})

test_that("pac_toc warns when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(doc=2.5, uv254=0.05, toc=50))

  expect_warning(pac_toc(water1, dose=31, time = 50)) # dose is out of bounds
  expect_warning(pac_toc(water1, dose=15, time = 1441)) # duration is out of bounds

})

test_that("pac_toc stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(doc=2.5, uv254=.05,toc=3.5))

  expect_error(pac_toc(water1, dose=15)) # missing time
  expect_error(pac_toc(water1, time=50)) # missing dose
  expect_error(pac_toc(dose=15, time=50)) # missing water
  expect_no_error(suppressWarnings(pac_toc(water1, dose=15, time=50, type="wood"))) # runs without errors with all inputs given correctly
})

test_that("Input water is s4 class", {
  water1 <-suppressWarnings(define_water(doc=3.5,uv254=.1))
  dosed_water <- pac_toc(water1, dose=15,time=50)
  expect_s4_class(dosed_water, "water")
})


test_that("pac_toc works", {
  water1 <- suppressWarnings(define_water(doc=2.5, uv254=.05))
  water2 <- pac_toc(water1, dose=15, time = 50, type="bituminous")
  expect_equal(round(water2@doc, 2), 1.94)
  expect_equal(round(water2@uv254,3), 0.032)

  water3 <- pac_toc(water1, dose=15, time=50, type="wood")
  expect_equal(round(water3@doc,2), 2.19)

  water4 <- pac_toc(water1, dose=15, time=50, type="lignite")
  expect_equal(round(water4@doc,2), 2.10)
})

test_that("Error when an unaccepted PAC type is entered.", {
  water1 <- suppressWarnings(define_water(doc=2.5, uv254=.05))
  expect_error(pac_toc(water1, dose=15, time=50, type="invalid type"))
})



