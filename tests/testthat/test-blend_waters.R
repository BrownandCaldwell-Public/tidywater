# Blend waters ----

test_that("Blend waters gives error when ratios don't sum to 1 and runs otherwise.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- define_water(ph = 5, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water3 <- define_water(ph = 10, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)

  expect_error(blend_waters(c(water1, water2, water3), c(.5, .5, .5)))
  expect_no_error(blend_waters(c(water1, water2, water3), c(1 / 3, 1 / 3, 1 / 3)))
})

test_that("Blend waters outputs same water when ratio is 1 or the blending waters have the same parameters.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- water1
  water3 <- define_water(ph = 10, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)

  blend1 <- blend_waters(c(water1, water3), c(1, 0))
  blend2 <- blend_waters(c(water1, water3), c(0, 1))
  expect_equal(water1, blend1)
  expect_equal(water3, blend2)

  blend3 <- blend_waters(c(water1, water2), c(.5, .5))
  expect_equal(water1, blend3)
})

test_that("Blend waters conserves temperature and alkalinity.", {
  water2 <- define_water(ph = 7, temp = 20, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1) # same as water1
  water3 <- define_water(ph = 10, temp = 10, alk = 200, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)

  blend1 <- blend_waters(c(water2, water3), c(.5, .5))
  expect_equal(blend1@alk, 150)
  expect_equal(blend1@temp, 15)
})

test_that("Blend waters conserves DOC.", {
  water2 <- define_water(ph = 7, temp = 20, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 5, uv254 = .1) # same as water1
  water3 <- define_water(ph = 10, temp = 10, alk = 200, 0, 0, 0, 0, 0, 0, cond = 100, toc = 3, doc = 3, uv254 = .1)

  blend1 <- blend_waters(c(water2, water3), c(.5, .5))
  expect_equal(blend1@doc, 4)
})

test_that("Blend waters correctly handles list of estimated parameters.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tds = 100) %>%
    chemdose_ph(naoh = 5))
  water2 <- define_water(ph = 7, temp = 25, alk = 100, cond = 100) %>%
    balance_ions()
  water3 <- suppressWarnings(define_water(ph = 10, temp = 10, alk = 200, tot_hard = 100, cl = 100, na = 100))

  blend1 <- suppressWarnings(blend_waters(c(water1, water2), c(.5, .5)))
  blend2 <- suppressWarnings(blend_waters(c(water2, water3), c(.5, .5)))
  blend3 <- blend_waters(c(water1), c(1))

  expect_equal(blend1@estimated, "_cond_tds_na")
  expect_equal(blend2@estimated, "_tds_na_ca_mg_cond")
  expect_equal(blend3@estimated, water1@estimated)
})

test_that("Blend waters warns when some slots are NA.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 100, tot_hard = 100))
  water2 <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 100, na = 100))

  expect_warning(blend_waters(c(water1, water2), c(.5, .5)), "ca.+na")
})

test_that("Blend waters warns about chloramines.", {
  water1 <- suppressWarnings(define_water(7, 20, 50, free_chlorine = 2))
  water2 <- suppressWarnings(define_water(7.5, 20, 100, tot_nh3 = 2))

  water3 <- suppressWarnings(define_water(8, 22, 13, combined_chlorine = 3))
  water4 <- suppressWarnings(define_water(8, 22, 13) %>% chemdose_ph(nh4oh = 30))

  expect_warning(blend_waters(c(water1, water2), c(.5, .5)), "breakpoint+")
  expect_warning(blend_waters(c(water3, water4), c(.25, .75)), "breakpoint+")
  expect_no_warning(blend_waters(c(water1, water3), c(.20, .80)))
})

################################################################################*
################################################################################*
# blend_waters helpers ----
# Test that blend_waters_chain outputs are the same as base function, blend_waters
test_that("blend_waters_chain outputs are the same as base function, blend_waters", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    balance_ions()

  water2 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    balance_ions() %>%
    chemdose_ph(naoh = 20)

  blend1 <- blend_waters(waters = c(water1, water2), ratios = c(.4, .6))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6)))

  blend2 <- purrr::pluck(water2, 5, 1)

  expect_equal(blend1, blend2)
})

# Test that output is a column of water class lists, and changing the output column name works
test_that("blend_waters_chain outputs a column of water class lists, and output_water arg works", {
  testthat::skip_on_cran()
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6), output_water = "testoutput"))

  blend2 <- purrr::pluck(water2, 5, 1)

  expect_s4_class(blend2, "water") # check class
  expect_equal(names(water2[5]), "testoutput") # check output_water arg
})


# Check that this function can handle different ways to input ratios
test_that("blend_waters_chain can handle different ways to input ratios", {
  testthat::skip_on_cran()
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c(.4, .6)))

  blend2 <- purrr::pluck(water2, "blended_water", 1)

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 20) %>%
    mutate(
      ratio1 = .4,
      ratio2 = .6
    ) %>%
    blend_waters_chain(waters = c("balanced_water", "dosed_chem_water"), ratios = c("ratio1", "ratio2")))

  blend3 <- purrr::pluck(water3, "blended_water", 1)

  expect_equal(blend2, blend3) # test different ways to input ratios
})

test_that("blend_waters_chain can handle water columns mixed with objects", {
  testthat::skip_on_cran()
  water4 <- water_df %>%
    slice(1:3) %>%
    define_water_chain("A")
  water5 <- define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )

  blend4 <- water4 %>%
    blend_waters_chain(c("A", water5), c(.5, .5))

  final <- purrr::pluck(blend4, "blended_water", 1)

  expect_s4_class(final, "water")
})
