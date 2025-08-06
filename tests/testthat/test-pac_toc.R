# PAC_TOC ----
library(dplyr)

test_that("pac_toc returns no removed DOC value when PAC dose is 0.", {
  water1 <- suppressWarnings(define_water(doc = 2.5, uv254 = 0.05, toc = 3.5))
  water2 <- suppressWarnings(pac_toc(water1, type = "wood", time = 18, dose = 0))
  # expected value of doc when no PAC is added - would equal starting value
  expect_equal(water2@doc, water1@doc)
})

test_that("No water defined, no default listed", {
  expect_error(pac_toc(dose = 15, time = 50, type = "wood")) # argument water is missing, with no default
})


test_that("pac_toc defaults to bituminous when type isn't specified.", {
  water1 <- suppressWarnings(define_water(doc = 2.5, uv254 = 0.05, toc = 3.5))
  dosed1 <- pac_toc(water1, time = 18, dose = 5)
  dosed2 <- pac_toc(water1, type = "bituminous", time = 18, dose = 5)
  expect_equal(dosed1@doc, dosed2@doc)
})

test_that("pac_toc errors when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(doc = 2.5, uv254 = 0.05, toc = 50))
  expect_error(pac_toc(water1, dose = 31, time = 50)) # dose is out of bounds
  expect_error(pac_toc(water1, dose = 15, time = 1441)) # duration is out of bounds
})

test_that("pac_toc stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(doc = 2.5, uv254 = .05, toc = 3.5))

  expect_error(pac_toc(water1, dose = 15)) # missing time
  expect_error(pac_toc(water1, time = 50)) # missing dose
  expect_error(pac_toc(dose = 15, time = 50)) # missing water
  expect_no_error(suppressWarnings(pac_toc(water1, dose = 15, time = 50, type = "wood"))) # runs without errors with all inputs given correctly
})

test_that("Input water is s4 class", {
  water1 <- suppressWarnings(define_water(doc = 3.5, uv254 = .1))
  dosed_water <- pac_toc(water1, dose = 15, time = 50)
  expect_s4_class(dosed_water, "water")
})


test_that("pac_toc works", {
  water1 <- suppressWarnings(define_water(doc = 2.5, uv254 = .05))
  water2 <- pac_toc(water1, dose = 15, time = 50, type = "bituminous")
  expect_equal(round(water2@doc, 2), 1.94)
  expect_equal(round(water2@uv254, 3), 0.032)

  water3 <- pac_toc(water1, dose = 15, time = 50, type = "wood")
  expect_equal(round(water3@doc, 2), 2.19)

  water4 <- pac_toc(water1, dose = 15, time = 50, type = "lignite")
  expect_equal(round(water4@doc, 2), 2.10)

  water5 <- suppressWarnings(define_water(doc = 1, uv254 = .05))
  water6 <- suppressWarnings(pac_toc(water5, dose = 15, time = 20))
  expect_equal(round(water6@doc, 2), 0.91)
})

test_that("Error when an unaccepted PAC type is entered.", {
  water1 <- suppressWarnings(define_water(doc = 2.5, uv254 = .05))
  expect_error(pac_toc(water1, dose = 15, time = 50, type = "invalid type"))
})

################################################################################*
################################################################################*
# pac_toc helpers ----

test_that("pac_toc_df outputs are the same as base function, pac_toc", {
  testthat::skip_on_cran()
  water0 <- define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13, mg = 4,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )
  water1 <- water0 %>%
    pac_toc(dose = 10, time = 10)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    pac_toc_df(dose = 10, time = 10, output_water = "pac", pluck_cols = TRUE)

  types <- data.frame(type = c("wood", "lignite"))
  doses <- data.frame(PACDose = seq(10, 16, 2))
  water3 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    merge(types) %>%
    merge(doses) %>%
    pac_toc_df(dose = PACDose, time = 10, output_water = "pac") %>%
    pluck_water("pac", "doc")

  water4 <- pac_toc(water0, time = 10, dose = 16, type = "lignite")

  expect_equal(water1@doc, water2$pac_doc)
  expect_equal(water1@uv254, water2$pac_uv254)
  expect_equal(water4@doc, water3$pac_doc[8])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("pac_toc_df output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               define_water_df("raw") %>%
                               pac_toc_df(input_water = "raw", time = 10, dose = 4))

  water2 <- purrr::pluck(water1, "paced", 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_df("raw") %>%
    mutate(
      dose = 4,
      time = 10
    ) %>%
    pac_toc_df(input_water = "raw", output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check pac_toc_df can use a column or function argument for chemical dose

test_that("pac_toc_df can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               define_water_df("raw") %>%
                               pac_toc_df(input_water = "raw", time = 50, dose = 10, pluck_cols = TRUE))

  water2 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               define_water_df("raw") %>%
                               mutate(
                                 time = 50,
                                 dose = 10
                                 ) %>%
                               pac_toc_df(input_water = "raw", pluck_cols = TRUE))

  # test that pluck_col does the same as pluck_water
  water3 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               define_water_df("raw") %>%
                               mutate(time = 50) %>%
                               pac_toc_df(input_water = "raw", dose = 10) %>%
                               pluck_water("paced", c("toc", "doc", "uv254")))

  expect_equal(water1$paced_doc, water2$paced_doc) # test different ways to input args
  expect_equal(water1$paced_uv254, water2$paced_uv254)

  # Test that inputting time/dose separately (in column and as an argument)  gives same results
  expect_equal(water1$paced_doc, water3$paced_doc)


  water4 <- water_df %>%
    slice(1:4) %>%
    define_water_df("raw") %>%
    mutate(time = c(20, 20, 50, 50)) %>%
    pac_toc_df(input_water = "raw", output_water = "pac", dose = c(10, 20))
  water4b <- water4[water4$dose == 10, ]

  water5 <- water_df %>%
    slice(1:4) %>%
    define_water_df("raw") %>%
    mutate(PACtime = c(20, 20, 50, 50)) %>%
    pac_toc_df(input_water = "raw", output_water = "pac", dose = c(10, 20), time = PACtime)

  water6 <- water_df %>%
    slice(1:4) %>%
    define_water_df("raw") %>%
    mutate(time = c(20, 20, 50, 50)) %>%
    merge(data.frame(dose = c(10, 20))) %>%
    pac_toc_df(input_water = "raw", output_water = "pac")

  water7 <- water_df %>%
    slice(1:4) %>%
    define_water_df("raw") %>%
    mutate(
      PACtime = c(20, 20, 50, 50),
      type = "bituminous"
    ) %>%
    pac_toc_df(input_water = "raw", output_water = "pac", dose = 10, time = PACtime)

  expect_equal(water4$pac, water5$pac)
  expect_equal(water4$pac, water6$pac)
  expect_equal(water4b$pac, water7$pac)
})

test_that("pac_toc_df errors with argument + column for same param", {
  testthat::skip_on_cran()
  water <- water_df %>%
    define_water_df("raw")
  expect_error(water %>%
    mutate(dose = 5) %>%
    pac_toc_df(input_water = "raw", time = 50, dose = 10))
  expect_error(water %>%
    mutate(time = 5) %>%
    pac_toc_df(input_water = "raw", time = 50, dose = 10))
})

test_that("pac_toc_df correctly handles arguments with multiple numbers", {
  testthat::skip_on_cran()
  water <- water_df %>%
    define_water_df("raw")

  water1 <- water %>%
    pac_toc_df("raw", time = c(10, 20), dose = 5)
  water2 <- water %>%
    pac_toc_df("raw", time = 20, dose = seq(10, 30, 10))

  expect_equal(nrow(water) * 2, nrow(water1))
  expect_equal(nrow(water) * 3, nrow(water2))
})
