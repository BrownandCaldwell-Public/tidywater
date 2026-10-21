# GACBV-TOC ----

test_that("No water defined, no default listed", {
  water <- water_df[1, ]

  expect_error(gac_toc(media_size = "8x30", ebct = 10)) # argument water is missing, with no default
  expect_error(gac_toc(water)) # water is not a defined water object
})

test_that("gacbv_toc returns error if inputs are misspelled or missing.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  water2 <- suppressWarnings(define_water(temp = 25, tot_hard = 100, toc = 3.5)) # ph is not defined
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 25, tot_hard = 100)) # toc is not defined

  expect_error(gacbv_toc(water1, media_size = "11x40", model = "Zachman", target_doc = 0.8))
  expect_error(gacbv_toc(water1, ebct = 15, model = "Zachman", target_doc = 0.8))
  expect_error(gacbv_toc(water1, model = "Zachmann", target_doc = 0.8))
  expect_error(gacbv_toc(water1, model = "Zachman"))
  expect_error(gacbv_toc(water2, model = "Zachman", target_doc = 0.8))
  expect_error(gacbv_toc(water3, model = "Zachman", target_doc = 0.8))
})

test_that("gacbv_toc defaults to correct values.", {
  water <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))

  bv1 <- gacbv_toc(water, target_doc = 0.8)
  bv2 <- gacbv_toc(water, ebct = 10, model = "Zachman", media_size = "12x40", target_doc = 0.8)

  expect_equal(bv1, bv2)
})

test_that("gacbv_toc works.", {
  water <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))

  bv1 <- gacbv_toc(water, model = "WTP", target_doc = 0.8)
  bv2 <- gacbv_toc(water, model = "Zachman", target_doc = 0.8)
  bv3 <- gacbv_toc(water, model = "WTP", target_doc = c(0.6, 0.8, 1))

  expect_true(is.data.frame(bv1))
  expect_equal(bv1$bed_volume, 3300)
  expect_false(identical(bv1$bed_volume, bv2$bed_volume))
  expect_true(is.data.frame(bv3))
  expect_error(gacbv_toc(water, target_doc = 0))
})

test_that("gacbv_toc bed volume increases with target_doc.", {
  water <- define_water(ph = 7.5, alk = 80, tds = 250, toc = 3, doc = 2.85, uv254 = 0.08)

  bv_wtp <- gacbv_toc(water, model = "WTP", target_doc = c(0.8, 1, 1.5))$bed_volume
  bv_zach <- gacbv_toc(water, model = "Zachman", media_size = "12x40", target_doc = c(1, 1.5))$bed_volume

  expect_length(unique(bv_wtp), 3)
  expect_true(all(diff(bv_wtp) > 0))
  expect_length(unique(bv_zach), 2)
  expect_true(all(diff(bv_zach) > 0))
})

test_that("gacbv_toc returns the bed volume where C/C0 matches target_doc / doc on the gacrun_toc curve.", {
  water <- define_water(ph = 7.5, alk = 80, tds = 250, toc = 3, doc = 2.85, uv254 = 0.08)

  curve <- gacrun_toc(water, model = "WTP")
  bv <- gacbv_toc(water, model = "WTP", target_doc = 1)$bed_volume

  expect_equal(curve$x_norm[curve$bv == bv], 1 / water@doc, tolerance = 0.02)
  expect_true(bv < max(curve$bv)) # not simply the end of the breakthrough curve
})

################################################################################*
################################################################################*
# gacbv_toc helpers ----

test_that("gacbv_toc_df outputs are the same as base function, gacbv_toc", {
  testthat::skip_on_cran()
  water0 <- define_water(
    7.9,
    20,
    50,
    tot_hard = 50,
    ca = 13,
    mg = 4,
    na = 20,
    k = 20,
    cl = 30,
    so4 = 20,
    tds = 200,
    cond = 100,
    toc = 2,
    doc = 1.8,
    uv254 = 0.05
  )

  water1 <- water0 %>%
    gacbv_toc(model = "WTP", target_doc = 0.8)

  water2 <- water_df %>%
    dplyr::slice(1) %>%
    define_water_df() %>%
    gacbv_toc_df(model = "WTP", target_doc = 0.8, media_size = "12x40", ebct = 10)

  expect_equal(water1$bed_volume, water2$defined_bed_volume)
})

# Test that output is a data frame with the correct number of columns
test_that("gacbv_toc_df output is data frame", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(
    water_df[1, ] %>%
      define_water_df("raw") %>%
      transform(
        model = "Zachman",
        media_size = "12x40",
        ebct = 10,
        target_doc = 0.6
      )
  )

  water1 <- water0 %>%
    gacbv_toc_df(input_water = "raw")

  expect_true(is.data.frame(water1))
  expect_equal(ncol(water0), ncol(water1) - 1)
})
