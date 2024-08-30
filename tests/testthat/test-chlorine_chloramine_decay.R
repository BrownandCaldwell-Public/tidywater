# Chemdose chlorine/chloramine ----

test_that("chemdose_cl2 returns no modeled chlorine/chloramine residual when chlorine dose is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2))
  Ct <- suppressWarnings(chemdose_cl2(water1, cl2_dose = 0, time = 8, treatment = 'raw', cl_type = 'chlorine'))

  expect_equal(water1@ocl, 0)
})

test_that("chemdose_cl2 does not run when water_type isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))

  expect_error(chemdose_cl2(water1, water_type = "raw"))
  expect_error(chemdose_cl2(water1, water_type = treated))
})

test_that("chemdose_cl2 warns when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = .1, uv254 = 0.01))
  water3 <- suppressWarnings(define_water(ph = 8, temp = 20, toc = 3, uv254 = 0.1))
  water4 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3, uv254 = 0.1))

  expect_warning(chemdose_cl2(water1, cl2 = 1, time = 8)) # chlorine out of bounds
  expect_warning(chemdose_cl2(water1, cl2 = 4, time = 1)) # time out of bounds
  expect_warning(chemdose_cl2(water2, cl2 = 2, time = 8, treatment = "gac")) # toc out of bounds
  expect_warning(chemdose_cl2(water3, cl2 = 4, time = 8, treatment = "coag")) # ph not set to 7.5
  expect_warning(chemdose_cl2(water4, cl2 = 4, time = 8)) # br out of bounds
})

test_that("chemdose_cl2 stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, br = 50))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2))
  water5 <- suppressWarnings(define_water(ph = 8, temp = 25, toc = 3, uv = 0.2))

  expect_error(chemdose_cl2(water1, cl2 = 4, time = 8)) # missing ph
  expect_error(chemdose_cl2(water2, cl2 = 4, time = 8)) # missing toc
  expect_error(chemdose_cl2(water3, cl2 = 4, time = 1, treatment = "coag")) # missing uv
  expect_no_error(suppressWarnings(chemdose_cl2(water3, cl2 = 4, time = 1, treatment = "raw"))) # raw doesn't require uv
  expect_error(chemdose_cl2(water4, cl2 = 4, time = 8)) # missing br
  expect_error(chemdose_cl2(water5, time = 8)) # missing cl2
  expect_error(chemdose_cl2(water5, cl2 = 4)) # missing time

})


test_that("chemdose_cl2 works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- chemdose_cl2(water1, cl2 = 3, time = 8)
  water3 <- chemdose_cl2(water1, cl2 = 3, time = 8, treatment = "coag")
  water4 <- chemdose_cl2(water1, cl2 = 3, time = 72, treatment = "coag", location = "ds")
  water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 1, uv254 = 0.04, br = 50))
  water6 <- chemdose_cl2(water5, cl2 = 3, time = 8, treatment = "gac")

  expect_equal(round(water2@ocl), 68)
  expect_equal(round(water3@ocl), 59)
  expect_equal(round(water3@haa5), 48)
  expect_equal(round(water4@haa5), 69)
  expect_equal(round(water6@haa5), 12)
})






# refer to uniroot template (acid_base function)
# only modify coeffs in if loops
# cl2 -- cl2_dose
# modeled_cl -- cl2_t / Ct
# check with sierra for function naming convention 

#-------------------------------------
# setwd("C:/Users/PChen/Rscripts/tidywater/R")
# library(tidywater,tidyverse)

# devtools::load_all()
# example_cl2 <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2)) %>%
# chemdose_cl2(cl2 = 2, time = 8, treatment = "raw", cl_type = "chlorine")

# water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2))
# test <- suppressWarnings(chemdose_cl2(water1, cl2_dose = 1, time = 8, treatment = 'raw', cl_type = 'chlorine'))

#-------------------------------------
# time <- 0
# for i = 1:10 {
#   time <- time + i
Ct <- chemdose_cl2(water1, cl2_dose = 20, time = 0, cl_type = 'chlorine', treatment = 'raw')
Ct@tot_ocl
# }



