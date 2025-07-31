# Define water -----
test_that("Define water outputs water class.", {
  # Disregard warnings, they are expected here.
  suppressWarnings({
    water1 <- define_water(
      ph = 7, temp = 25, alk = 100, tot_hard = 0,
      ca = 0, mg = 0, na = 0, k = 0, cl = 0, so4 = 0,
      toc = 5, doc = 4.8, uv254 = .1
    )
    water2 <- define_water(temp = 25, tot_hard = 50)
    water3 <- define_water(ph = 7, temp = 25, alk = 100)
  })
  expect_s4_class(water1, "water")
  expect_s4_class(water2, "water")
  expect_s4_class(water3, "water")
})

test_that("Define water calculates correct carbonate balance.", {
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
  })

  expect_equal(water1@ph, 7)
  expect_equal(signif(water1@tot_co3, 2), 0.0024)
  expect_equal(signif(water1@hco3, 2), 0.002)
})

test_that("Define water calculates correct TDS/IS/cond.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, cond = 312))
  water3 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 100, cl = 100, so4 = 30))

  expect_true(grepl("cond", water1@estimated))
  expect_true(grepl("tds", water2@estimated))
  expect_true(grepl("cond", water3@estimated) & grepl("tds", water3@estimated))

  expect_equal(round(water1@cond), 312)
  expect_equal(round(water2@tds), 200)
  expect_equal(signif(water1@is, 2), .005)
  expect_equal(signif(water2@is, 2), .005)
  expect_equal(signif(water3@is, 2), .005)
  expect_equal(round(water3@cond), 315)
  expect_equal(round(water3@tds), 201)
})


test_that("Define water gives missing value warnings.", {
  expect_warning(
    define_water(
      alk = 100, temp = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10, cl = 10, so4 = 10, tds = 100,
      doc = 5, toc = 5, uv254 = .1, br = 50
    ),
    "Missing.+pH.+"
  )
  expect_warning(
    define_water(
      ph = 7, temp = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10, cl = 10, so4 = 10, tds = 100,
      doc = 5, toc = 5, uv254 = .1, br = 50
    ),
    "Missing.+alkalinity.+"
  )
  expect_warning(
    define_water(
      ph = 7, alk = 100, temp = 20, tot_hard = 70, ca = 10, mg = 10, na = 10, k = 10, cl = 10, so4 = 10,
      toc = 5, uv254 = .1, br = 50
    ),
    "Missing.+DOC+"
  )
})

test_that("Define water warns about chloramines.", {
  expect_warning(
    define_water(
      ph = 7, alk = 100, temp = 20, na = 10, k = 10, cl = 10, so4 = 10,
      free_chlorine = 2, tot_nh3 = 3
    ),
    "breakpoint+"
  )

  expect_warning(
    define_water(
      ph = 7, alk = 100, temp = 20, na = 10, k = 10, cl = 10, so4 = 10,
      combined_chlorine = 2, tot_nh3 = 3
    ),
    "breakpoint+"
  )

  expect_warning(
    define_water(
      ph = 7, alk = 100, temp = 20, na = 10, k = 10, cl = 10, so4 = 10,
      free_chlorine = 2, combined_chlorine = 4, tot_nh3 = 3
    ),
    "breakpoint+"
  )
})


test_that("Define water doesn't output carbonate when pH or alk aren't provided.", {
  # Disregard warnings, they are expected here.
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25)
    water2 <- define_water(temp = 25, alk = 50)
  })

  expect_equal(water1@tot_co3, NA_real_)
  expect_equal(water2@tot_co3, NA_real_)
  expect_equal(water1@alk, NA_real_)
  expect_equal(water2@ph, NA_real_)
})

test_that("define_water handles organics inputs correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 7, doc = 3.5, toc = 3.4))

  expect_equal(water1@doc, 3.325)
  expect_equal(round(water2@toc, 3), 3.684)
  expect_equal(water3@uv254, NA_real_)
})

test_that("define_water correctly specifies when estimates are used.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 50, na = 100, cl = 100))
  water2 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(
    ph = 7, temp = 25, alk = 100, tot_hard = 50, na = 100, cl = 100, toc = 3.5, doc = 3.5,
    ca = 40, tds = 100
  ))

  expect_true(grepl("tds", water1@estimated))
  expect_true(grepl("cond", water1@estimated))
  expect_true(grepl("ca", water1@estimated))
  expect_true(grepl("doc", water2@estimated))

  expect_false(grepl("tds", water3@estimated))
  expect_false(grepl("ca", water3@estimated))
  expect_false(grepl("doc", water3@estimated))
  expect_true(grepl("cond", water3@estimated))
})

test_that("define_water correctly calculates dic", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100))

  dic_mg <- water1@tot_co3 * tidywater::mweights$dic * 1000
  expect_equal(dic_mg, water1@dic)
})

# define_water helpers ----
# Test that define_water_df outputs are the same as base function, define_water.

test_that("define_water_df output is the same as define_water", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_df(water_df[1,], output_water = "new_name"))
  water3 <- purrr::pluck(water2, 1, 1)

  # check pluck_cols
  water4 <- suppressWarnings(define_water_df(water_df[1,], pluck_cols = TRUE))

  expect_equal(water1, water3)
  expect_true(ncol(water4) == 5) # defined, ph, and alk
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("define_water_df outputs a water class and the output water argument works", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_df(water_df[1,], output_water = "new_name"))
  water3 <- purrr::pluck(water2, 1, 1)

  expect_s4_class(water3, "water")
})

# Check that this function can be piped to the next one and can handle a different output_water arg

test_that("define_water_df can be piped", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ))
  # water2 <- convert_Water(water1)

  water2 <- suppressWarnings(define_water_df(water_df[1,], output_water = "new_name"))

  water3 <- water2 %>% balance_ions_df("new_name")

  expect_equal(names(water2[1]), "new_name")
  expect_equal(ncol(water3), 2)
})

test_that("define_water_df correctly calculates dic", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    define_water_df() %>%
    pluck_water(parameter = "dic")
  water1 <- water1[1,]


  water2 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ))
  dic_mol <- water2@tot_co3 * tidywater::mweights$dic * 1000


  expect_equal(dic_mol, water1$defined_dic)
})
