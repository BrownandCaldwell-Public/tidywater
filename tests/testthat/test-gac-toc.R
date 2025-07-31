# GAC_TOC ----

test_that("No water defined, no default listed", {
  water <- water_df[1, ]

  expect_error(gac_toc(media_size = "8x30", ebct = 10)) # argument water is missing, with no default
  expect_error(gac_toc(water)) # water is not a defined water object
})

test_that("gac_toc returns error if inputs are misspelled or missing.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  water2 <- suppressWarnings(define_water(temp = 25, tot_hard = 100, toc = 3.5)) # ph is not defined
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 25, tot_hard = 100)) # toc is not defined

  expect_error(gac_toc(water1, media_size = "11x40", model = "Zachman", bed_vol = 10000))
  expect_error(gac_toc(water1, ebct = 15, model = "Zachman", bed_vol = 10000))
  expect_error(gac_toc(water1, model = "Zachmann", bed_vol = 10000))
  expect_error(gac_toc(water1, model = "Zachman"))
  expect_error(gac_toc(water2, model = "Zachman", bed_vol = 10000))
  expect_error(gac_toc(water3, model = "Zachman", bed_vol = 10000))
})

test_that("gac_toc defaults to correct values.", {
  water <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))

  dosed1 <- gac_toc(water, bed_vol = 15000)
  dosed2 <- gac_toc(water, ebct = 10, model = "Zachman", media_size = "12x40", bed_vol = 15000, pretreat = "coag")

  expect_equal(dosed1@toc, dosed2@toc)
  expect_equal(dosed1@doc, dosed2@doc)
  expect_equal(dosed1@uv254, dosed2@uv254)
})

test_that("Output water is s4 class", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  dosed_water <- gac_toc(water1, model = "WTP", bed_vol = 15000)
  expect_s4_class(dosed_water, "water")
})

test_that("gac_toc works", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  water2 <- gac_toc(water1, model = "Zachman", bed_vol = 8000)
  water3 <- gac_toc(water1, model = "WTP", bed_vol = 10000)
  water4 <- gac_toc(water1, model = "Zachman", bed_vol = 10000)
  water5 <- gac_toc(water1, model = "Zachman", bed_vol = 10000, pretreat = "o3biof")
  water6 <- gac_toc(water1, ebct = 20, model = "Zachman", bed_vol = 10000)
  water7 <- gac_toc(water1, media_size = "8x30", model = "Zachman", bed_vol = 8000)

  expect_equal(round(water2@doc, 2), 1.90)
  expect_false(identical(water2@doc, water3@doc))
  expect_false(identical(water2@doc, water4@doc))
  expect_false(identical(water2@uv254, water5@uv254))
  expect_false(identical(water2@doc, water6@doc))
  expect_false(identical(water2@doc, water7@doc))
})

################################################################################*
################################################################################*
# gac_toc helpers ----

test_that("gac_toc_df outputs are the same as base function, gac_toc", {
  testthat::skip_on_cran()
  water0 <- define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13, mg = 4,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )

  water1 <- water0 %>%
    gac_toc(model = "WTP", bed_vol = 15000)

  water2 <- water_df[1,] %>%
    define_water_df() %>%
    gac_toc_df(model = "WTP", bed_vol = 15000, media_size = "12x40", ebct = 10, output_water = "gac", pluck_cols = TRUE)

  expect_equal(water1@toc, water2$gac_toc)
  expect_equal(water1@doc, water2$gac_doc)
  expect_equal(water1@uv254, water2$gac_uv254)
})

# Test that output is a column of water class lists
test_that("gac_toc_df output is list of water class objects", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df[1,] %>%
    define_water_df("raw") %>%
    transform(
      model = "Zachman",
      media_size = "12x40",
      ebct = 10,
      bed_vol = 10000,
      pretreat = "coag"
    ) %>%
    gac_toc_df(input_water = "raw"))

  water2 <- purrr::pluck(water1, "gaced", 1)

  expect_s4_class(water2, "water") # check class
})

# Check gac_toc_df can use a column or function argument for chemical dose and both methods gives same results
test_that("gac_toc_df can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df[1,] %>%
    define_water_df("raw") %>%
    gac_toc_df(input_water = "raw", model = "WTP", bed_vol = 15000, pluck_cols = TRUE))

  water2 <- suppressWarnings(water_df[1,] %>%
    define_water_df("raw") %>%
    transform(
      model = "WTP",
      bed_vol = 15000
    ) %>%
    gac_toc_df(input_water = "raw", pluck_cols = TRUE))

  # check that pluck_cols does the same as pluck_water
  water3 <- suppressWarnings(water_df[1,] %>%
    define_water_df("raw") %>%
    transform(model = "WTP") %>%
    gac_toc_df(input_water = "raw", bed_vol = 15000) %>%
    pluck_water("gaced", c("toc", "doc", "uv254")))

  expect_equal(water1$gaced_doc, water2$gaced_doc) # test different ways to input args
  expect_equal(water1$gaced_uv254, water2$gaced_uv254)

  # Test that inputting time/dose separately (in column and as an argument)  gives same results
  expect_equal(water1$gaced_doc, water3$gaced_doc)
  expect_equal(ncol(water2), ncol(water3))
})

# Check that errors with argument + column for same param
test_that("gac_toc_df errors with argument + column for same param", {
  testthat::skip_on_cran()
  water <- water_df %>%
    define_water_df("raw")
  expect_error(water %>%
    transform(model = "WTP") %>%
    gac_toc_df(input_water = "raw", model = "WTP", bed_vol = 15000))
  expect_error(water %>%
    transform(bed_vol = 15000) %>%
    gac_toc_df(input_water = "raw", model = "WTP", bed_vol = 15000))
})

# Check that correctly handles arguments with multiple numbers
test_that("gac_toc_df correctly handles arguments with multiple numbers", {
  testthat::skip_on_cran()
  water <- water_df %>%
    define_water_df("raw")

  water1 <- water %>%
    gac_toc_df("raw", model = "Zachman", bed_vol = c(8000, 10000))
  water2 <- water %>%
    gac_toc_df("raw", model = c("Zachman", "WTP"), bed_vol = 10000)

  expect_equal(nrow(water) * 2, nrow(water1))
  expect_equal(nrow(water) * 2, nrow(water2))
})
