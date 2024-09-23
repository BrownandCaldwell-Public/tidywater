# Chemdose BDOC ----

test_that("chemdose_bdoc returns an error when water is absent or input incorrectly.", {
  expect_error(chemdose_bdoc(EBCT = 10, ozonated = TRUE))
  expect_error(chemdose_bdoc(water = list(ph = 7, toc = 5), EBCT = 10, ozonated = TRUE))
})

test_that("chemdose_bdoc returns an error or warning when arguments are input improperly or missing.", {
  water <-suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))

  expect_error(chemdose_bdoc(water, ozonated = TRUE))
  expect_error(chemdose_bdoc(water, EBCT = "4",  ozonated = TRUE))

  expect_warning(chemdose_bdoc(water, EBCT = 4, ozonated = 2))
  expect_warning(chemdose_bdoc(water, EBCT = 4,  ozonated = "TRUE"))
})

test_that("chemdose_bdoc returns an error when TOC is missing.", {
  water_no_toc <-suppressWarnings(define_water(ph = 7, temp = 15, alk = 100))
  expect_error(chemdose_bdoc(water_no_toc, EBCT = 10, ozonated = TRUE))
})

test_that("chemdose_bdoc calculates correct TOC removal for non-ozonated water.", {
  water <- suppressWarnings(define_water(ph = 7, temp = 15, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))
  dosed_water <- suppressWarnings(chemdose_bdoc(water, EBCT = 10, ozonated = FALSE))

  # Expect that TOC is reduced correctly using non-ozonated parameters
  expect_equal(round(dosed_water@toc, 2), 4.41)  # Expected TOC after treatment
  expect_equal(round(dosed_water@doc, 2), 3.81)  # Expected DOC (BDOC fraction of TOC)
})

test_that("chemdose_bdoc calculates correct TOC removal for ozonated water.", {
  water <- suppressWarnings(define_water(ph = 7, temp = 15, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))
  dosed_water <- suppressWarnings(chemdose_bdoc(water, EBCT = 10,ozonated = TRUE))

  # Expect that TOC is reduced correctly using ozonated parameters
  expect_equal(round(dosed_water@toc, 2), 4.32)  # Expected TOC after treatment
  expect_equal(round(dosed_water@doc, 2), 3.65)  # Expected DOC (BDOC fraction of TOC)
})

test_that("chemdose_bdoc correctly handles temperatures and non-ozonated water.", {
  water <-suppressWarnings(define_water(ph = 7, temp = 45, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))

  # the Bad Place temperature, non-ozonated
  dosed_water_high <- suppressWarnings(chemdose_bdoc(water, EBCT = 10, ozonated = FALSE))
  expect_equal(round(dosed_water_high@toc, 2), 4.33)

  # the Medium Place temperature, non-ozonated
  water@temp <- 19
  dosed_water_med<- suppressWarnings(chemdose_bdoc(water, EBCT = 10, ozonated = FALSE))
  expect_equal(round(dosed_water_med@toc, 2), 4.41)

  # the Good Place temperature, non-ozonated
  water@temp <- 5
  dosed_water_low <- suppressWarnings(chemdose_bdoc(water, EBCT = 10, ozonated = FALSE))
  expect_equal(round(dosed_water_low@toc, 2), 4.74)

})

test_that("chemdose_bdoc correctly handles temperatures and ozonated water.", {
  water <-suppressWarnings(define_water(ph = 7, temp = 45, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))

  # the Bad Place temperature, non-ozonated
  dosed_water_high <- suppressWarnings(chemdose_bdoc(water, EBCT = 10))
  expect_equal(round(dosed_water_high@toc, 2), 3.83)

  # the Medium Place temperature, non-ozonated
  water@temp <- 19
  dosed_water_med<- suppressWarnings(chemdose_bdoc(water, EBCT = 10))
  expect_equal(round(dosed_water_med@toc, 2), 4.32)

  # the Good Place temperature, non-ozonated
  water@temp <- 5
  dosed_water_low <- suppressWarnings(chemdose_bdoc(water, EBCT = 10))
  expect_equal(round(dosed_water_low@toc, 2), 4.61)

})

