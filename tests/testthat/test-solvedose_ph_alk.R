# Solve Dose pH ----

test_that("Solve dose pH produces a warning and returns NA when target pH is unreachable but runs otherwise.", {
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_warning(solvedose_ph(water4, 6, "naoh"))
  expect_warning(solvedose_ph(water4, 6, "co2"))
  expect_equal(suppressWarnings(solvedose_ph(water4, 6, "co2")), NA)
  expect_no_warning(solvedose_ph(water4, 9, "naoh"))
  expect_no_error(solvedose_ph(water4, 9, "naoh"))
})

test_that("Solve dose pH doesn't run when target pH is out of range.", {
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_error(solvedose_ph(water4, 20, "naoh"))
})

test_that("Solve dose pH returns the correct values.", {
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  # these are based on current tidywater outputs
  expect_equal(solvedose_ph(water4, 11, "naoh"), 40.3)
  expect_equal(solvedose_ph(water4, 7, "co2"), 3.7)
  co2dose <- solvedose_ph(water4, 7, "co2")
  expect_equal(round(chemdose_ph(water4, co2 = co2dose)@ph, 1), 7)
})

test_that("Solve dose pH doesn't error when target pH is close to starting.", {
  water1 <- define_water(
    ph = 7.01, temp = 19, alk = 100, tot_hard = 100,
    ca = 26, mg = 8, tot_po4 = 1, tds = 200
  )

  expect_no_error(solvedose_ph(water1, 7, "h2so4"))

  water2 <- define_water(
    ph = 7.99, temp = 19, alk = 150, tot_hard = 100,
    ca = 26, mg = 8, free_chlorine = 1, tds = 200
  )

  expect_no_error(solvedose_ph(water2, 8, "naoh"))
})


# Solve Dose Alkalinity ----

test_that("Solve dose alk produces a warning and returns NA when target alk is unreachable but runs otherwise.", {
  water5 <- define_water(8, 20, 50, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_warning(solvedose_alk(water5, 20, "naoh"))
  expect_equal(suppressWarnings(solvedose_alk(water5, 100, "h2so4")), NA)
  expect_no_warning(solvedose_alk(water5, 100, "naoh"))
  expect_no_error(solvedose_alk(water5, 100, "naoh"))
})

test_that("Solve dose alk works.", {
  water5 <- define_water(8, 20, 50, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  # these are based on current tidywater outputs
  expect_equal(solvedose_alk(water5, 100, "naoh"), 40)
  expect_equal(solvedose_alk(water5, 10, "h2so4"), 39.3)
  naohdose <- solvedose_alk(water5, 100, "naoh")
  expect_equal(signif(chemdose_ph(water5, naoh = naohdose)@alk, 2), 100)
})

################################################################################*
################################################################################*
# solvedose_ph helper ----
# Check solvedose_ph_once outputs are the same as base function, solvedose_ph

test_that("solvedose_ph_once outputs are the same as base function, solvedose_ph", {
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )) %>%
    balance_ions() %>%
    solvedose_ph(target_ph = 9.2, chemical = "naoh")

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", target_ph = 9.2, chemical = "naoh"))

  expect_equal(water1, water2$dose_required)
})

# Check that output is a data frame

test_that("solvedose_ph_once outputs data frame", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", target_ph = 9.2, chemical = "naoh"))

  expect_true(is.data.frame(water2))
})

# test different ways to input chemical
test_that("solvedose_ph_once can handle different input formats", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", target_ph = 9.2, chemical = "naoh"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(
      target_ph = 9.2,
      chemical = "naoh"
    ) %>%
    balance_ions_chain() %>%
    solvedose_ph_once(input_water = "balanced_water", output_column = "caustic_dose"))

  expect_equal(water2$dose_required, water3$caustic_dose)
})

################################################################################*
################################################################################*
# solvedose_alk helper ----
# Check solvedose_alk_once outputs are the same as base function, solvedose_alk

test_that("solvedose_alk_once outputs are the same as base function, solvedose_alk", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50)) %>%
    balance_ions() %>%
    solvedose_alk(target_alk = 100, chemical = "naoh")

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", target_alk = 100, chemical = "naoh"))

  expect_equal(round(water1), round(water2$dose_required))
})

# Check that output is a data frame

test_that("solvedose_alk_once outputs data frame", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", target_alk = 100, chemical = "naoh"))

  expect_true(is.data.frame(water2))
})

# test different ways to input chemical
test_that("solvedose_alk_once can handle different input formats", {
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", target_alk = 100, chemical = "na2co3"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(
      target_alk = 100,
      chemical = "na2co3"
    ) %>%
    balance_ions_chain() %>%
    solvedose_alk_once(input_water = "balanced_water", output_column = "soda_ash"))

  expect_equal(water2$dose_required, water3$soda_ash)
})
