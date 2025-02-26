# PAC_TOC ----

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
test_that("pac_toc_once outputs are the same as base function, pac_toc", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    pac_toc(dose = 10, time = 10)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    pac_toc_once(dose = 10, time = 10))

  expect_equal(water1@doc, water2$doc)
  expect_equal(water1@toc, water2$toc)
  expect_equal(water1@uv254, water2$uv254)
})

# Check that output is a data frame

test_that("pac_toc_once is a data frame", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    pac_toc_once(
      input_water = "defined_water",
      dose = 5, time = 10
    ))

  expect_true(is.data.frame(water1))
})

# Check pac_toc_once can use a column or function argument for chemical dose

test_that("pac_toc_once can use a column or function argument for dose", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    pac_toc_once(
      input_water = "raw",
      dose = 5, time = 10
    ))
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    mutate(
      dose = 5, time = 10
    ) %>%
    pac_toc_once("raw"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    mutate(dose = 5) %>%
    pac_toc_once(input_water = "raw", time = 10))

  expect_equal(water1$doc, water2$doc) # test different ways to input args
  expect_equal(water1$uv254, water2$uv254)

  # Test that inputting dose and time separately (in column and as an argument) gives same results
  expect_equal(water1$doc, water3$doc)
})

test_that("pac_toc_chain outputs are the same as base function, pac_toc", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    pac_toc(dose = 10, time = 10)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    pac_toc_chain(dose = 10, time = 10, output_water = "pac") %>%
    pluck_water("pac", c(
      "doc", "toc", "uv254"
    )))

  expect_equal(water1@doc, water2$pac_doc)
  expect_equal(water1@uv254, water2$pac_uv254)
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("pac_toc_chain output is list of water class objects, and can handle an ouput_water arg", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    pac_toc_chain(input_water = "raw", time = 10, dose = 4))

  water2 <- purrr::pluck(water1, "pac_water", 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_chain("raw") %>%
    mutate(
      dose = 4,
      time = 10
    ) %>%
    pac_toc_chain(input_water = "raw", output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[5]), "diff_name") # check if output_water arg works
})

# Check pac_toc_chain can use a column or function argument for chemical dose

test_that("pac_toc_chain can use a column or function argument for chemical dose", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    pac_toc_chain(input_water = "raw", time = 50, dose = 10) %>%
    pluck_water("pac_water", c("doc", "uv254")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    mutate(
      time = 50,
      dose = 10,
    ) %>%
    pac_toc_chain(input_water = "raw") %>%
    pluck_water("pac_water", c("doc", "uv254")))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    mutate(time = 50) %>%
    pac_toc_chain(input_water = "raw", dose = 10) %>%
    pluck_water("pac_water", c("doc", "uv254")))

  water4 <- water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    mutate(pactime = 50) %>%
    pac_toc_chain(input_water = "raw", dose = 10, time = .$pactime) %>%
    pluck_water("pac_water", c("doc", "uv254"))

  expect_equal(water1$pac_water_doc, water2$pac_water_doc) # test different ways to input args
  expect_equal(water1$pac_water_uv254, water2$pac_water_uv254)

  # Test that inputting time/dose separately (in column and as an argument)  gives save results
  expect_equal(water1$pac_water_doc, water3$pac_water_doc)
  expect_equal(water1$pac_water_doc, water4$pac_water_doc)
})

test_that("pac_toc_chain errors with argument + column for same param", {
  water <- water_df %>%
    define_water_chain("raw")
  expect_error(water %>%
    mutate(dose = 5) %>%
    pac_toc_chain(input_water = "raw", time = 50, dose = 10))
  expect_error(water %>%
    mutate(time = 5) %>%
    pac_toc_chain(input_water = "raw", time = 50, dose = 10))
})

test_that("pac_toc_chain correctly handles arguments with multiple numbers", {
  water <- water_df %>%
    define_water_chain("raw")

  water1 <- water %>%
    pac_toc_chain("raw", time = c(10, 20), dose = 5)
  water2 <- water %>%
    pac_toc_chain("raw", time = 20, dose = seq(10, 30, 10))

  expect_equal(nrow(water) * 2, nrow(water1))
  expect_equal(nrow(water) * 3, nrow(water2))
})
