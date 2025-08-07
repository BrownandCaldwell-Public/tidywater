# Calculate corrosion ----
test_that("most indices won't work without ca, cl, so4", {
  water <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tds = 238))

  expect_error(suppressWarnings(calculate_corrosion(water, index = "aggressive")))
  expect_error(suppressWarnings(calculate_corrosion(water, index = "ryznar")))
  expect_error(suppressWarnings(calculate_corrosion(water, index = "langelier")))
  expect_error(suppressWarnings(calculate_corrosion(water, index = "ccpp")))
  expect_error(suppressWarnings(calculate_corrosion(water, index = "larsonskold")))
  expect_error(suppressWarnings(calculate_corrosion(water, index = "csmr")))
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
  water2 <- suppressWarnings(define_water(8, 25, 200, 200, na = 100, cl = 100)) %>% balance_ions(anion = "so4")

  expect_warning(calculate_corrosion(water1, index = "aggressive"))
  expect_warning(calculate_corrosion(water2, index = "csmr"))
  expect_warning(calculate_corrosion(water2, index = "larsonskold"))
})

test_that("aggressive index works", {
  suppressWarnings({
    water1 <- define_water(ph = 8, temp = 25, alk = 200, ca = 80)
    index1 <- calculate_corrosion(water1, index = "aggressive")

    water2 <- define_water(ph = 8, temp = 25, alk = 15, ca = 80)
    index2 <- calculate_corrosion(water2, index = "aggressive")

    water3 <- define_water(ph = 8, temp = 25, alk = 15, ca = 60)
    index3 <- calculate_corrosion(water3, index = "aggressive")
  })

  expect_equal(round(index1$aggressive), 13) # high alk
  expect_equal(round(index2$aggressive), 11) # low alk
  expect_equal(round(index3$aggressive), 11) # use tot_hard instead of ca_hard
})

test_that("csmr works", {
  suppressWarnings({
    water1 <- define_water(ph = 8, temp = 25, alk = 200, cl = 100, so4 = 1)
    index1 <- calculate_corrosion(water1, index = "csmr")

    water2 <- define_water(ph = 8, temp = 25, cl = 2, so4 = 150)
    index2 <- calculate_corrosion(water2, index = "csmr")

    water3 <- define_water(ph = 8, temp = 25, alk = 15, tot_hard = 150, so4 = 5) %>%
      balance_ions()
    index3 <- calculate_corrosion(water3, index = "csmr")
  })

  expect_equal(round(index1$csmr), 100) # high cl, low so4
  expect_equal(round(index2$csmr, 2), 0.01) # low cl high so4
  expect_equal(round(index3$csmr), 18) # use balance ions to get chloride
})

test_that("larsonskold works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, cl = 100, so4 = 1))
  index1 <- calculate_corrosion(water1, index = "larsonskold")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, cl = 2, so4 = 150))
  index2 <- calculate_corrosion(water2, index = "larsonskold")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, cl = 50, so4 = 30)) %>%
    balance_ions()
  index3 <- calculate_corrosion(water3, index = "larsonskold")

  water4 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, cl = 150, so4 = 150))
  index4 <- calculate_corrosion(water4, index = "larsonskold")

  expect_equal(round(index1$larsonskold, 1), 0.7) # high cl, low so4
  expect_equal(round(index2$larsonskold, 1), 0.8) # low cl high so4
  expect_equal(round(index3$larsonskold, 2), 0.51) # use balance ions to get chloride
  expect_equal(round(index4$larsonskold), 74) # low alk
})

test_that("Corrosion index calculations work when IS is NA.", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 200))

  expect_no_error(calculate_corrosion(water1, index = c("langelier", "ryznar", "ccpp")))
})

# test answers will probably change as we figure out which ph_s to use. For now, I'm using MWH's ph_s.
# tests will stay the same though
test_that("langelier works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca = 40, tds = 173))
  index1 <- calculate_corrosion(water1, index = "langelier")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca = 40, tds = 56))
  index2 <- calculate_corrosion(water2, index = "langelier")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172))
  index3 <- calculate_corrosion(water3, index = "langelier")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca = 20, tds = 30))
  index4 <- calculate_corrosion(water4, index = "langelier")

  expect_equal(round(index1$langelier, 1), 0.8) # high alk
  expect_equal(round(index2$langelier, 1), -0.9) # low alk
  expect_equal(round(index3$langelier, 1), 0.7) # use tot_hard to get ca
  expect_equal(round(index4$langelier), -2) # low ph, alk, and hard to simulte highly corrosive water
})

# test answers will probably change as we figure out which ph_s to use. For now, I'm using MWH's ph_s.
# tests will stay the same though
test_that("ryznar works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca = 40, tds = 173))
  index1 <- calculate_corrosion(water1, index = "ryznar")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca = 40, tds = 56))
  index2 <- calculate_corrosion(water2, index = "ryznar")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172))
  index3 <- calculate_corrosion(water3, index = "ryznar")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca = 20, tds = 30))
  index4 <- calculate_corrosion(water4, index = "ryznar")

  expect_equal(round(index1$ryznar), 6) # high alk
  expect_equal(round(index2$ryznar), 10) # low alk
  expect_equal(round(index3$ryznar), 7) # use tot_hard to get ca
  expect_equal(round(index4$ryznar), 11) # low ph, alk, and hard to simulte highly corrosive water
})

test_that("ccpp works", {
  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca = 40, tds = 173))
  index1 <- calculate_corrosion(water1, index = "ccpp")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca = 40, tds = 56))
  index2 <- calculate_corrosion(water2, index = "ccpp")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172))
  index3 <- calculate_corrosion(water3, index = "ccpp")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca = 20, tds = 30))
  index4 <- calculate_corrosion(water4, index = "ccpp")

  water5 <- suppressWarnings(define_water(ph = 6.85, temp = 25, alk = 80, ca = 32, tds = 90))
  index5 <- calculate_corrosion(water5, index = "ccpp")

  water6 <- suppressWarnings(define_water(ph = 5, alk = 20, ca = 32, tds = 90))
  index6 <- calculate_corrosion(water6, index = "ccpp")

  water7 <- define_water(
    ph = 10.4, alk = 250, ca = 116, na = 300, mg = 2.5, k = 4.5, cl = 200, so4 = 420,
    tot_nh3 = 12, tot_po4 = 6
  )
  index7 <- calculate_corrosion(water7, index = "ccpp")

  water8 <- define_water(ph = 12, alk = 5000, ca = 500, mg = 100, tds = 10000)
  index8 <- calculate_corrosion(water8, index = "ccpp")

  expect_equal(round(index1$ccpp), 16) # high alk
  expect_equal(round(index2$ccpp, 1), -1.3) # low alk
  expect_equal(round(index3$ccpp), 16) # use tot_hard to get ca
  expect_equal(round(index4$ccpp), -4) # low ca
  expect_equal(round(index5$ccpp), -34) # low pH
  expect_equal(round(index6$ccpp), -328) # extra low pH
  expect_equal(round(index7$ccpp), 247)
  expect_equal(round(index8$ccpp), 1249)
  expect_error(suppressWarnings(define_water(ph = 14, alk = 20, ca = 32, tds = 90)) %>%
    calculate_corrosion(index = "ccpp")) # high pH is out of uniroot bounds
})

test_that("calculate_corrosion output is a data frame", {
  water0 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tds = 238, ca = 80, cl = 30, so4 = 30))
  water1 <- calculate_corrosion(water0)

  expect_true(is.data.frame(water1))
  expect_true("aggressive" %in% colnames(water1))
  expect_true("ryznar" %in% colnames(water1))
  expect_true("ccpp" %in% colnames(water1))
  expect_true("csmr" %in% colnames(water1))
  expect_true("larsonskold" %in% colnames(water1))
  expect_true("langelier" %in% colnames(water1))
})

################################################################################*
################################################################################*
# calculate_corrosion helpers ----
# Check calculate_corrosion_df outputs are the same as base function, calculate_corrosion

test_that("calculate_corrosion_df outputs are the same as base function, calculate_corrosion", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ) %>%
    calculate_corrosion())

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    calculate_corrosion_df()

  expect_equal(water1$langelier, water2$defined_langelier)
  expect_equal(water1$ryznar, water2$defined_ryznar)
  expect_equal(water1$aggressive, water2$defined_aggressive)
  expect_equal(water1$csmr, water2$defined_csmr)
  expect_equal(water1$ccpp, water2$defined_ccpp)
  expect_equal(water1$larsonskold, water2$defined_larsonskold)
})

test_that("function catches index typos", {
  testthat::skip_on_cran()
  water <- suppressWarnings(water_df %>%
    define_water_df())

  expect_error(calculate_corrosion_df(water, index = "csr"))
  expect_error(calculate_corrosion_df(water, index = c("aggressive", "ccccp")))
  expect_no_error(calculate_corrosion_df(water, index = c("aggressive", "ccpp"))) # no error
  expect_error(calculate_corrosion_df(water, index = "langlier"))
  expect_error(calculate_corrosion_df(water, index = c("ai", "ccccp")))
  expect_no_error(calculate_corrosion_df(water, index = c("ryznar", "csmr", "larsonskold"))) # no error
})

# Check that output is a data frame

test_that("calculate_corrosion_df is a data frame", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_df() %>%
    calculate_corrosion_df(input_water = "defined"))

  expect_true(is.data.frame(water1))
  expect_true("defined_aggressive" %in% colnames(water1))
  expect_true("defined_ryznar" %in% colnames(water1))
  expect_true("defined_ccpp" %in% colnames(water1))
  expect_true("defined_csmr" %in% colnames(water1))
  expect_true("defined_larsonskold" %in% colnames(water1))
  expect_true("defined_langelier" %in% colnames(water1))
})

# Check calculate_corrosion_df outputs an appropriate number of indices

test_that("calculate_corrosion_df outputs an appropriate number of indices", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_df() %>%
    calculate_corrosion_df(input_water = "defined", index = c("aggressive", "csmr")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_df() %>%
    mutate(naoh = 5) %>%
    calculate_corrosion_df(input_water = "defined"))

  water3 <- water1 %>%
    select_if(names(water1) %in% c(
      "defined_aggressive", "defined_ryznar", "defined_langelier",
      "defined_ccpp", "defined_larsonskold", "defined_csmr"
    ))

  water4 <- water2 %>%
    select_if(names(water2) %in% c(
      "defined_aggressive", "defined_ryznar", "defined_langelier",
      "defined_ccpp", "defined_larsonskold", "defined_csmr"
    ))

  expect_error(expect_equal(length(water1), length(water2))) # waters with different indices shouldn't be equal
  expect_equal(length(water3), 2) # indices selected in fn should match # of output index columns
  expect_equal(length(water4), 6)
})
