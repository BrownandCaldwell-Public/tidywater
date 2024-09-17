# Chemdose BDOC ----

test_that("chemdose_bdoc returns an error when no water is supplied.", {
  expect_error(chemdose_bdoc(EBCT = 10, temp = 15, ozonated = TRUE),
               "No source water defined. Create a water using the 'define_water' function.")
})

test_that("chemdose_bdoc returns an error when input is not of class 'water'.", {
  expect_error(chemdose_bdoc(water = list(ph = 7, toc = 5), EBCT = 10, temp = 15, ozonated = TRUE),
               "Input water must be of class 'water'. Create a water using 'define_water'.")
})

test_that("chemdose_bdoc returns an error when TOC is missing.", {
  water_no_toc <- define_water(ph = 7, temp = 15, alk = 100)
  expect_error(chemdose_bdoc(water_no_toc, EBCT = 10, temp = 15, ozonated = TRUE),
               "Water is missing a TOC parameter. Make sure TOC is specified.")
})

test_that("chemdose_bdoc calculates correct TOC removal for non-ozonated water.", {
  water <- define_water(ph = 7, temp = 15, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1)
  dosed_water <- suppressWarnings(chemdose_bdoc(water, EBCT = 10, temp = 15, ozonated = FALSE))
  
  # Expect that TOC is reduced correctly using non-ozonated parameters
  expect_equal(round(dosed_water@toc, 2), 4.15)  # Expected TOC after treatment
  expect_equal(round(dosed_water@doc, 2), 0.83)  # Expected DOC (BDOC fraction of TOC)
})

test_that("chemdose_bdoc calculates correct TOC removal for ozonated water.", {
  water <- define_water(ph = 7, temp = 15, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1)
  dosed_water <- suppressWarnings(chemdose_bdoc(water, EBCT = 10, temp = 15, ozonated = TRUE))
  
  # Expect that TOC is reduced correctly using ozonated parameters
  expect_equal(round(dosed_water@toc, 2), 3.86)  # Expected TOC after treatment
  expect_equal(round(dosed_water@doc, 2), 1.16)  # Expected DOC (BDOC fraction of TOC)
})

test_that("chemdose_bdoc correctly handles high and low temperatures.", {
  water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1)
  
  # High temperature, ozonated
  dosed_water_high <- suppressWarnings(chemdose_bdoc(water, EBCT = 10, temp = 25, ozonated = TRUE))
  expect_equal(round(dosed_water_high@toc, 2), 3.18)  # Expected TOC after high temp ozonated
  
  # Low temperature, non-ozonated
  dosed_water_low <- suppressWarnings(chemdose_bdoc(water, EBCT = 10, temp = 5, ozonated = FALSE))
  expect_equal(round(dosed_water_low@toc, 2), 4.70)  # Expected TOC after low temp non-ozonated
})
