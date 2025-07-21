# Chemdose DBP ----

test_that("chemdose_dbp returns no modeled DBPs when chlorine dose is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  dbps <- suppressWarnings(chemdose_dbp(water1, cl2 = 0, time = 8))

  expect_equal(dbps@tthm, 0)
})

test_that("chemdose_dbp does not run when water_type isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1, br = 30))

  expect_error(chemdose_dbp(water1, water_type = "raw"))
  expect_error(chemdose_dbp(water1, water_type = treated))
})

test_that("chemdose_dbp warns when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 30))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = .1, uv254 = 0.01, br = 30))
  water3 <- suppressWarnings(define_water(ph = 8, temp = 20, toc = 3, uv254 = 0.1, br = 30))
  water4 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3, uv254 = 0.1, br = 2))

  expect_warning(chemdose_dbp(water1, cl2 = 1, time = 8)) # chlorine out of bounds
  expect_warning(chemdose_dbp(water1, cl2 = 4, time = 1)) # time out of bounds
  expect_warning(chemdose_dbp(water2, cl2 = 2, time = 8, treatment = "gac")) # toc out of bounds
  expect_warning(chemdose_dbp(water3, cl2 = 4, time = 8, treatment = "coag")) # ph not set to 7.5
  expect_warning(chemdose_dbp(water4, cl2 = 4, time = 8)) # br out of bounds
})

test_that("chemdose_dbp warns about chloramines", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50, tot_nh3 = 3))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 30, toc = 2, uv254 = 0.01, br = 30)) %>%
    chemdose_ph(nh42so4 = 3)

  expect_warning(chemdose_dbp(water1, cl2 = 2, time = 8), "breakpoint+")
  expect_warning(chemdose_dbp(water2, cl2 = 4, time = 8), "breakpoint+")
})

test_that("chemdose_dbp stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1, br = 5))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, br = 50))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2, br = NA_real_))
  water5 <- suppressWarnings(define_water(ph = 8, temp = 25, toc = 3, uv = 0.2, br = 50))

  expect_error(chemdose_dbp(water1, cl2 = 4, time = 8)) # missing ph
  expect_error(chemdose_dbp(water2, cl2 = 4, time = 8)) # missing toc
  expect_error(chemdose_dbp(water3, cl2 = 4, time = 1, treatment = "coag")) # missing uv
  expect_no_error(suppressWarnings(chemdose_dbp(water3, cl2 = 4, time = 1, treatment = "raw"))) # raw doesn't require uv
  expect_error(chemdose_dbp(water4, cl2 = 4, time = 8)) # missing br
  expect_error(chemdose_dbp(water5, time = 8)) # missing cl2
  expect_error(chemdose_dbp(water5, cl2 = 4)) # missing time
})


test_that("chemdose_dbp works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- chemdose_dbp(water1, cl2 = 3, time = 8)
  water3 <- chemdose_dbp(water1, cl2 = 3, time = 8, treatment = "coag")
  water4 <- chemdose_dbp(water1, cl2 = 3, time = 72, treatment = "coag", location = "ds")
  water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 1, uv254 = 0.04, br = 50))
  water6 <- chemdose_dbp(water5, cl2 = 3, time = 8, treatment = "gac")

  expect_equal(round(water2@tthm), 68)
  expect_equal(round(water3@tthm), 59)
  expect_equal(round(water3@haa5), 48)
  expect_equal(round(water4@haa5), 69)
  expect_equal(round(water6@haa5), 12)
})

test_that("users can provide their own dbp coefficients.", {
  coeff <- data.frame(A = 5E-2, a = 1, b = .5, c = .5, d = .5, e = 1, f = .5, ph_const = 7.5, ID = "chcl3")

  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50)) %>%
    chemdose_dbp(cl2 = 2, time = 8)

  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50)) %>%
    chemdose_dbp(cl2 = 2, time = 8, coeff = coeff)

  expect_true(water1@tthm == water2@tthm) # did not use custom coeff for tthm
  expect_false(water1@chcl3 == water2@chcl3)
  expect_equal(round(water2@chcl3, 2), 58.78)
})

test_that("chemdose_dbp works when correction is T/F", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50)) %>%
    chemdose_dbp(cl2 = 2, time = 8)

  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50)) %>%
    chemdose_dbp(cl2 = 2, time = 8, correction = FALSE)

  expect_true(identical(water1@tthm, water2@tthm)) # correction factor is 1
  expect_false(identical(water1@chbr2cl, water2@chbr2cl)) # these compounds have other correction factors
  expect_false(identical(water1@haa5, water2@haa5))
})

################################################################################*
################################################################################*
# chemdose_dbp helpers ----

test_that("chemdose_dbp_chain outputs are the same as base function, chemdose_dbp", {
  testthat::skip_on_cran()
  water0 <- define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13, mg = 4,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )
  water1 <- water0 %>%
    chemdose_dbp(cl2 = 10, time = 8)

  water2 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_dbp_chain(cl2 = 10, time = 8, output_water = "chlor", pluck_cols = "all")

  times <- tibble(time = seq(4, 10, 2))
  treatment <- tibble(Location = c("raw", "coag", "gac"))
  water3 <- suppressWarnings(water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    cross_join(times) %>%
    cross_join(treatment) %>%
    chemdose_dbp_chain(cl2 = 10, treatment = Location, output_water = "chlor") %>%
    pluck_water("chlor", c("tthm", "haa5")))

  water4 <- suppressWarnings(chemdose_dbp(water0, cl2 = 10, time = 8, treatment = "coag"))
  water5 <- suppressWarnings(chemdose_dbp(water0, cl2 = 10, time = 4, treatment = "gac"))

  expect_equal(water1@chcl3, water2$chlor_chcl3)
  expect_equal(water1@chcl2br, water2$chlor_chcl2br)
  expect_equal(water1@chbr2cl, water2$chlor_chbr2cl)
  expect_equal(water1@chbr3, water2$chlor_chbr3)
  expect_equal(water1@tthm, water2$chlor_tthm)
  expect_equal(water1@mcaa, water2$chlor_mcaa)
  expect_equal(water1@dcaa, water2$chlor_dcaa)
  expect_equal(water1@tcaa, water2$chlor_tcaa)
  expect_equal(water1@mbaa, water2$chlor_mbaa)
  expect_equal(water1@dbaa, water2$chlor_dbaa)
  expect_equal(water1@haa5, water2$chlor_haa5)

  expect_equal(water4@tthm, water3$chlor_tthm[8])
  expect_equal(water5@haa5, water3$chlor_haa5[3])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_dbp_chain output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 60) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water", time = 8, cl2 = 4))

  water2 <- purrr::pluck(water1, "disinfected", 1)

  water3 <- suppressWarnings(water_df %>%
    mutate(br = 60) %>%
    define_water_chain() %>%
    mutate(
      cl2 = 4,
      time = 8
    ) %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check chemdose_dbp_chain can use a column or function argument for chemical dose

test_that("chemdose_dbp_chain can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water", time = 120, cl2 = 10, pluck_cols = TRUE))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    mutate(
      time = 120,
      cl2 = 10,
    ) %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water", pluck_cols = TRUE))

  # test that pluck_cols does the same as pluck_Water
  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    mutate(time = 120) %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water", cl2 = 10) %>%
    pluck_water("disinfected", c("tthm", "haa5")))

  expect_equal(water1$disinfected_tthm, water2$disinfected_tthm) # test different ways to input args
  expect_equal(water1$disinfected_haa5, water2$disinfected_haa5)

  # Test that inputting time/cl2 separately (in column and as an argument)  gives same results
  expect_equal(water1$disinfected_tthm, water3$disinfected_tthm)
  expect_equal(water2$disinfected_haa5, water3$disinfected_haa5)
})

test_that("chemdose_dbp_chain errors with argument + column for same param", {
  testthat::skip_on_cran()
  water <- water_df %>%
    mutate(br = 80) %>%
    define_water_chain("water")
  expect_error(water %>%
    mutate(cl2 = 5) %>%
    chemdose_dbp_chain(input_water = "water", time = 120, cl2 = 10))
  expect_error(water %>%
    mutate(time = 5) %>%
    chemdose_dbp_chain(input_water = "water", time = 120, cl2 = 10))

  times <- tibble(time = seq(4, 10, 2))
  water1 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    cross_join(times)

  water2 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(treatment = "raw")

  expect_error(chemdose_dbp_chain(water1, cl2 = 10, time = 10, output_water = "chlor"))
  expect_error(chemdose_dbp_chain(water2, cl2 = 10, time = 10, treatment = "raw", output_water = "chlor"))
})

test_that("chemdose_dbp_chain correctly handles arguments with multiple numbers", {
  testthat::skip_on_cran()
  water <- water_df %>%
    mutate(br = 80) %>%
    define_water_chain("water")

  water1 <- water %>%
    chemdose_dbp_chain("water", time = c(60, 120), cl2 = 5)
  water2 <- water %>%
    chemdose_dbp_chain("water", time = 120, cl2 = seq(2, 4, 1))

  expect_equal(nrow(water) * 2, nrow(water1))
  expect_equal(nrow(water) * 3, nrow(water2))
})

test_that("users can provide their own dbp coefficients to chemdose_dbp_chain.", {
  coeff <- data.frame(A = 5E-2, a = 1, b = .5, c = .5, d = .5, e = 1, f = .5, ph_const = 7.5, ID = "chcl3")

  water1 <- water_df %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    chemdose_dbp_chain("defined", cl2 = 2, time = 120) %>%
    pluck_water("disinfected", c("tthm", "chcl3"))

  water2 <- water_df %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    chemdose_dbp_chain("defined", cl2 = 2, time = 120, coeff = coeff) %>%
    pluck_water("disinfected", c("tthm", "chcl3"))

  expect_equal(water1$disinfected_tthm, water2$disinfected_tthm) # no custom coeff inputted for tthm
  expect_false(identical(water1$disinfected_chcl3, water2$disinfected_chcl3))
  expect_false(any(is.na(water2$disinfected_chcl3)) || identical(water2$disinfected_chcl3, 0))
})
