# Calculate corrosion ----
test_that("most indices won't work without ca, cl, so4", {
  water <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tds = 238))

  expect_equal(suppressWarnings(calculate_corrosion(water, index = "aggressive"))@aggressive, NA_real_)
  expect_error(suppressWarnings(calculate_corrosion(water, index = "ryznar")))
  expect_error(suppressWarnings(calculate_corrosion(water, index = "langelier")))
  expect_error(suppressWarnings(calculate_corrosion(water, index = "ccpp")))
  expect_equal(suppressWarnings(calculate_corrosion(water, index = "larsonskold"))@larsonskold, NA_real_)
  expect_equal(suppressWarnings(calculate_corrosion(water, index = "csmr"))@csmr, NA_real_)
})

test_that("function catches index typos", {
  water <- suppressWarnings(define_water(
    ph = 8, temp = 25, alk = 200, tds = 238,
    tot_hard = 100, cl = 40, so4 = 40
  ))

  expect_error(calculate_corrosion(water, index = "csr"))
  expect_error(calculate_corrosion(water, index = c("aggressive", "ccccp")))
  expect_error(calculate_corrosion(water, index = c("ai", "ryznar", "ccpp", "csmr", "langelier")))
  expect_no_error(calculate_corrosion(water, index = c("ryznar", "csmr", "larsonskold"))) # no error
})

test_that("warnings are present when parameters used in calculations are estimated by tidywater.", {
  water1 <- suppressWarnings(define_water(8, 25, 200, 200))
  water2 <- suppressWarnings(define_water(8, 25, 200, 200, na = 100, cl = 100)) %>% balance_ions()

  expect_warning(calculate_corrosion(water1, index = "aggressive"))
  expect_warning(calculate_corrosion(water2, index = "csmr"))
  expect_warning(calculate_corrosion(water2, index = "larsonskold"))
})

test_that("aggressive index works", {
  suppressWarnings({
    water1 <- define_water(ph = 8, temp = 25, alk = 200, ca = 80) %>%
      calculate_corrosion(index = "aggressive")

    water2 <- define_water(ph = 8, temp = 25, alk = 15, ca = 80) %>%
      calculate_corrosion(index = "aggressive")

    water3 <- define_water(ph = 8, temp = 25, alk = 15, ca = 60) %>%
      calculate_corrosion(index = "aggressive")
  })

  expect_equal(round(water1@aggressive), 13) # high alk
  expect_equal(round(water2@aggressive), 11) # low alk
  expect_equal(round(water3@aggressive), 11) # use tot_hard instead of ca_hard
})

test_that("csmr works", {
  suppressWarnings({
    water1 <- define_water(ph = 8, temp = 25, alk = 200, cl = 100, so4 = 1) %>%
      calculate_corrosion(index = "csmr")

    water2 <- define_water(ph = 8, temp = 25, cl = 2, so4 = 150) %>%
      calculate_corrosion(index = "csmr")

    water3 <- define_water(ph = 8, temp = 25, alk = 15, tot_hard = 150, so4 = 5) %>%
      balance_ions() %>%
      calculate_corrosion(index = "csmr")
  })

  expect_equal(round(water1@csmr), 100) # high cl, low so4
  expect_equal(round(water2@csmr, 2), 0.01) # low cl high so4
  expect_equal(round(water3@csmr), 18) # use balance ions to get chloride
})

test_that("larsonskold works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, cl = 100, so4 = 1)) %>%
    calculate_corrosion(index = "larsonskold")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, cl = 2, so4 = 150)) %>%
    calculate_corrosion(index = "larsonskold")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, cl = 50, so4 = 30)) %>%
    balance_ions() %>%
    calculate_corrosion(index = "larsonskold")

  water4 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, cl = 150, so4 = 150)) %>%
    calculate_corrosion(index = "larsonskold")

  expect_equal(round(water1@larsonskold, 1), 0.7) # high cl, low so4
  expect_equal(round(water2@larsonskold, 1), 0.8) # low cl high so4
  expect_equal(round(water3@larsonskold, 2), 0.51) # use balance ions to get chloride
  expect_equal(round(water4@larsonskold), 74) # low alk
})

test_that("Corrosion index calculations work when IS is NA.", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 200))

  expect_no_error(calculate_corrosion(water1, index = c("langelier", "ryznar", "ccpp")))
})

# test answers will probably change as we figure out which ph_s to use. For now, I'm using MWH's ph_s.
# tests will stay the same though
test_that("langelier works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca = 40, tds = 173)) %>%
    calculate_corrosion(index = "langelier")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca = 40, tds = 56)) %>%
    calculate_corrosion(index = "langelier")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172)) %>%
    calculate_corrosion(index = "langelier")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca = 20, tds = 30)) %>%
    calculate_corrosion(index = "langelier")

  expect_equal(round(water1@langelier, 1), 0.8) # high alk
  expect_equal(round(water2@langelier, 1), -0.9) # low alk
  expect_equal(round(water3@langelier, 1), 0.7) # use tot_hard to get ca
  expect_equal(round(water4@langelier), -2) # low ph, alk, and hard to simulte highly corrosive water
})

# test answers will probably change as we figure out which ph_s to use. For now, I'm using MWH's ph_s.
# tests will stay the same though
test_that("ryznar works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca = 40, tds = 173)) %>%
    calculate_corrosion(index = "ryznar")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca = 40, tds = 56)) %>%
    calculate_corrosion(index = "ryznar")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172)) %>%
    calculate_corrosion(index = "ryznar")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca = 20, tds = 30)) %>%
    calculate_corrosion(index = "ryznar")

  expect_equal(round(water1@ryznar), 6) # high alk
  expect_equal(round(water2@ryznar), 10) # low alk
  expect_equal(round(water3@ryznar), 7) # use tot_hard to get ca
  expect_equal(round(water4@ryznar), 11) # low ph, alk, and hard to simulte highly corrosive water
})

test_that("ccpp works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca = 40, tds = 173)) %>%
    calculate_corrosion(index = "ccpp")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca = 40, tds = 56)) %>%
    calculate_corrosion(index = "ccpp")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172)) %>%
    calculate_corrosion(index = "ccpp")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca = 20, tds = 30)) %>%
    calculate_corrosion(index = "ccpp")

  water5 <- suppressWarnings(define_water(ph = 6.85, temp = 25, alk = 80, ca = 32, tds = 90)) %>%
    calculate_corrosion(index = "ccpp")

  expect_equal(round(water1@ccpp), 17) # high alk
  expect_equal(round(water2@ccpp, 1), -1.2) # low alk
  expect_equal(round(water3@ccpp), 16) # use tot_hard to get ca
  expect_equal(round(water4@ccpp), -4) # low ca
  expect_equal(round(water5@ccpp), -33) # low pH
})

################################################################################*
################################################################################*
# calculate_corrosion helpers ----
# Check calculate_corrosion_once outputs are the same as base function, calculate_corrosion

test_that("calculate_corrosion_once outputs are the same as base function, calculate_corrosion", {
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ) %>%
    balance_ions() %>%
    calculate_corrosion())

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water"))

  expect_equal(water1@langelier, water2$langelier)
  expect_equal(water1@ryznar, water2$ryznar)
  expect_equal(water1@aggressive, water2$aggressive)
  expect_equal(water1@csmr, water2$csmr)
  expect_equal(water1@ccpp, water2$ccpp)
  expect_equal(water1@larsonskold, water2$larsonskold)
})

test_that("function catches index typos", {
  water <- suppressWarnings(water_df %>%
    define_water_chain())

  expect_error(calculate_corrosion_chain(water, index = "csr"))
  expect_error(calculate_corrosion_chain(water, index = c("aggressive", "ccccp")))
  expect_no_error(calculate_corrosion_chain(water, index = c("aggressive", "ccpp"))) # no error
  expect_error(calculate_corrosion_once(water, index = "langlier"))
  expect_error(calculate_corrosion_once(water, index = c("ai", "ccccp")))
  expect_no_error(calculate_corrosion_chain(water, index = c("ryznar", "csmr", "larsonskold"))) # no error
})

# Check that output is a data frame

test_that("calculate_corrosion_once is a data frame", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water"))

  expect_true(is.data.frame(water1))
})

# Check calculate_corrosion_once outputs an appropriate number of indices

test_that("calculate_corrosion_once outputs an appropriate number of indices", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water", index = c("aggressive", "csmr")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(naoh = 5) %>%
    balance_ions_chain() %>%
    calculate_corrosion_once(input_water = "balanced_water"))

  water3 <- water1 %>%
    select_if(names(water1) %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))
  water4 <- water2 %>%
    select_if(names(water2) %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))

  expect_error(expect_equal(length(water1), length(water2))) # waters with different indices shouldn't be equal
  expect_equal(length(water3), 2) # indices selected in fn should match # of output index columns
  expect_equal(length(water4), 6)
})


# Test that calculate_corrosion_chain outputs are the same as base function, calculate_corrosion
test_that("calculate_corrosion_chain outputs the same as base, calculate_corrosion", {
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ) %>%
    balance_ions() %>%
    calculate_corrosion())

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water"))

  water3 <- purrr::pluck(water2, 3, 1)

  expect_equal(water1, water3) # check against base
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("calculate_corrosion_chain output is list of water class objects, and can handle an ouput_water arg", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water"))

  water2 <- purrr::pluck(water1, 3, 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[4]), "diff_name") # check if output_water arg works
})


# Check that variety of ways to input chemicals work
test_that("calculate_corrosion_chain can handle different forms of CaCO3", {
  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water"))

  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water", form = "aragonite"))

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    calculate_corrosion_chain(input_water = "balanced_water", form = "vaterite"))

  pluck1 <- purrr::pluck(water1, 3)
  pluck2 <- purrr::pluck(water2, 3)
  pluck3 <- purrr::pluck(water3, 3)

  expect_error(expect_equal(pluck1, pluck2))
  expect_error(expect_equal(pluck2, pluck3))
  expect_error(expect_equal(pluck1, pluck3))
})
