# GACBV-TOC ----

test_that("No water defined, no default listed", {
  expect_error(gacbv_toc(media_size = "8x30", ebct = 10)) # argument water is missing, with no default
})

test_that("gacbv_toc returns error if inputs are misspelled or missing.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  water2 <- suppressWarnings(define_water(temp = 25, tot_hard = 100, toc = 3.5)) # ph is not defined
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 25, tot_hard = 100))# toc is not defined
  
  expect_error(gacbv_toc(water1, media_size = "11x40", model = "Zachman", target_doc = 0.8))
  expect_error(gacbv_toc(water1, ebct = 15, model = "Zachman", target_doc = 0.8))
  expect_error(gacbv_toc(water1, model = "Zachmann", target_doc = 0.8))
  expect_error(gacbv_toc(water1, model = "Zachman"))
  expect_error(gacbv_toc(water2, model = "Zachman", target_doc = 0.8))
  expect_error(gacbv_toc(water3, model = "Zachman", target_doc = 0.8))
})

test_that("gacbv_toc defaults to correct values.", {
  water <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  
  bv1 <- gacbv_toc(water, model = "WTP", target_doc = 0.8)
  bv2 <- gacbv_toc(water, ebct = 10, model = "WTP", media_size = "12x40", target_doc = 0.8)
  
  expect_equal(bv1, bv2)
})

test_that("gacbv_toc works.", {
  water <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  
  bv1 <- gacbv_toc(water, model = "WTP", target_doc = 0.8)
  bv2 <- gacbv_toc(water, model = "Zachman", target_doc = 0.8)
  bv3 <- gacbv_toc(water, model = "WTP", target_doc = c(0.6, 0.8, 1))
  
  expect_true(is.numeric(bv1))
  expect_equal(bv1, 20000)
  expect_false(identical(bv1, bv2))
  expect_true(is.vector(bv3))
})

################################################################################*
################################################################################*
# gacbv_toc helpers ----

test_that("gacbv_toc_once outputs are the same as base function, gacbv_toc", {
  testthat::skip_on_cran()
  water0 <- define_water(7.9, 20, 50,
                         tot_hard = 50, ca = 13, mg = 4,
                         na = 20, k = 20, cl = 30, so4 = 20,
                         tds = 200, cond = 100,
                         toc = 2, doc = 1.8, uv254 = 0.05
  )
  
  water1 <- water0 %>%
    gacbv_toc(model = "WTP", target_doc = 0.8)
  
  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    gacbv_toc_once(model = "WTP", target_doc = 0.8, media_size = "12x40", ebct = 10)
  
  expect_equal(list(water1), water2$bed_volume)
})

# Test that output is a data frame with the correct number of columns
test_that("gacbv_toc_once output is data frame", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               define_water_chain("raw") %>%
                               mutate(model = "Zachman",
                                      media_size = "12x40",
                                      ebct = 10,
                                      target_doc = 0.6))
  
  water1 <- water0 %>%
    gacbv_toc_once(input_water = "raw")
  
  expect_true(is.data.frame(water1))
  expect_equal(ncol(water0), ncol(water1) - 1)
})

