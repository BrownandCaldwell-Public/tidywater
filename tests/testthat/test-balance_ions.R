# Balance Ions ----

test_that("Balance ions doesn't alter carbonate system.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  water2 <- balance_ions(water1)
  expect_equal(water1@ph, water2@ph)
  expect_equal(water1@tot_co3, water2@tot_co3)
  expect_equal(water1@hco3, water2@hco3)
})

test_that("Balance ions anions and cations only accept specific ions.", {
  expect_error(define_water(ph = 7, temp = 25, alk = 100, na = 30, cl = 10) %>% balance_ions(anion = "co3", cation = "ca"))
  expect_error(define_water(ph = 7, temp = 25, alk = 100, na = 2, so4 = 30) %>% balance_ions(anion = "so4", cation = "calcium"))
})

test_that("Balance ions doesn't alter Ca, Mg, PO4, or OCl if not specified.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  water2 <- balance_ions(water1)
  expect_equal(water1@ca, water2@ca)
  expect_equal(water1@mg, water2@mg)
  expect_equal(water1@free_chlorine, water2@free_chlorine)
  expect_equal(water1@tot_po4, water2@tot_po4)
})

test_that("Balance ions alters specified ions.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  water2 <- balance_ions(water1, cation = "ca")
  water3 <- suppressWarnings(define_water(7, 25, 100, tot_hard = 150))
  water4 <- balance_ions(water3, anion = "so4")

  expect_error(expect_equal(water1@ca, water2@ca)) # calcium updated in water 2, so they should not be equal
  expect_equal(water1@na, water2@na) # sodium not updated, so they should be equal

  expect_error(expect_equal(water3@so4, water4@so4)) # sulfate updated in water 4, so they should not be equal
  expect_equal(water3@cl, water4@cl) # chloride not updated, so they should be equal
})

test_that("Balance ions doesn't alter organics.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  water2 <- balance_ions(water1)
  expect_equal(water1@toc, water2@toc)
  expect_equal(water1@doc, water2@doc)
  expect_equal(water1@uv254, water2@uv254)
})

test_that("Balance ions results in neutral charge.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 70, 10, 10, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  water2 <- balance_ions(water1)
  expect_equal(water2@na + water2@ca * 2 + water2@mg * 2 + water2@k -
    (water2@cl + 2 * water2@so4 + water2@hco3 + 2 * water2@co3 + water2@h2po4 + 2 * water2@hpo4 + 3 * water2@po4) +
    water2@h - water2@oh - water2@ocl, 0)

  water3 <- define_water(ph = 7, temp = 25, alk = 100, 70, 10, 10, 10, 10, 10, 10, free_chlorine = 2, tot_po4 = 1, toc = 5, doc = 4.8, uv254 = .1, br = 50)
  water4 <- balance_ions(water3)
  expect_equal(water4@na + water4@ca * 2 + water4@mg * 2 + water4@k -
    (water4@cl + 2 * water4@so4 + water4@hco3 + 2 * water4@co3 + water4@h2po4 + 2 * water4@hpo4 + 3 * water4@po4) +
    water4@h - water4@oh - water4@ocl, 0)

  water5 <- balance_ions(water3, cation = "mg")
  expect_equal(water5@na + water5@ca * 2 + water5@mg * 2 + water5@k -
    (water5@cl + 2 * water5@so4 + water5@hco3 + 2 * water5@co3 + water5@h2po4 + 2 * water5@hpo4 + 3 * water5@po4) +
    water5@h - water5@oh - water5@ocl, 0)

  water6 <- balance_ions(water1, anion = "so4")
  expect_equal(water6@na + water6@ca * 2 + water6@mg * 2 + water6@k -
    (water6@cl + 2 * water6@so4 + water6@hco3 + 2 * water6@co3 + water6@h2po4 + 2 * water6@hpo4 + 3 * water6@po4) +
    water6@h - water6@oh - water6@ocl, 0)
})

test_that("Balance ions only updates TDS/cond/IS when appropriate.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tds = 100))
  water2 <- balance_ions(water1)
  water3 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, cond = 100))
  water4 <- balance_ions(water3)
  water5 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, na = 100, tot_hard = 100, cl = 100, so4 = 100))
  water6 <- balance_ions(water5)

  expect_false(grepl("tds", water2@estimated))
  expect_equal(round(water1@tds), round(water2@tds))
  expect_false(grepl("cond", water4@estimated))
  expect_equal(round(water3@tds), round(water4@tds))
  expect_true(grepl("cond", water5@estimated) & grepl("tds", water5@estimated))
  expect_true(grepl("cond", water6@estimated) & grepl("tds", water6@estimated))
  expect_error(expect_equal(round(water5@tds), round(water6@tds)))
  expect_error(expect_equal(signif(water5@is, 2), signif(water6@is, 2)))
})

################################################################################*
################################################################################*
# balance_ions helpers ----

# Test that balance_ions_chain outputs are the same as base function, balance_ions.

test_that("balance_ions_chain outputs are the same as base function, balance_ions", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ))
  water2 <- balance_ions(water1)

  water3 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_chain()

  water4 <- purrr::pluck(water3, 2, 1)

  expect_equal(water2, water4) # check against base

  water5 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ))
  water6 <- balance_ions(water5, anion = "so4", cation = "mg")

  water7 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_chain(anion = "so4", cation = "mg")

  water8 <- purrr::pluck(water7, 2, 1)

  expect_equal(water6, water8) # check against base
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("balance_ions_chain output is a column of water class lists", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_chain()
  water2 <- purrr::pluck(water1, 2, 1)

  expect_s4_class(water2, "water") # check class
})

# Check that this function can be piped to the next one
test_that("balance_ions_chain can be piped and handle an output_water argument", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water_chain(slice(water_df, 1))) %>%
    balance_ions_chain(output_water = "different_column") %>%
    chemdose_ph_chain(naoh = 20)

  expect_equal(names(water1[2]), "different_column") # check output_water arg
  expect_equal(ncol(water1), 4) # check if pipe worked
})
