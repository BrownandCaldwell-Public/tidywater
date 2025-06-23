# Chemdose chlorine/chloramine ----

test_that("chemdose_chlordecay returns modeled chlorine/chloramine residual = 0 when chlorine dose is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2) %>%
    chemdose_chlordecay(cl2_dose = 0, time = 8))
  water2 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, free_chlorine = 2, combined_chlorine = 1) %>%
    chemdose_chlordecay(cl2_dose = 0, time = 8, cl_type = "chloramine"))

  expect_equal(water1@free_chlorine, 0)
  expect_equal(water2@combined_chlorine, 0)
})

test_that("chemdose_chlordecay does not run when arguments are supplied incorrectly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))

  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, treatment = "rw"))
  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, treatment = treated))
  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, cl_type = "cl2"))
  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, cl_type = 4))
  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, use_chlorine_slot = 4))
})

test_that("chemdose_chlordecay warns when inputs are out of model range", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = .1, uv254 = 0.01))
  water3 <- suppressWarnings(define_water(ph = 8, temp = 20, toc = 3, uv254 = 0.01))
  water4 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3, uv254 = 0.1))

  expect_warning(chemdose_chlordecay(water1, cl2_dose = 0.994, time = 1)) # chlorine out of bounds
  expect_warning(chemdose_chlordecay(water1, cl2_dose = 2, time = 121, treatment = "coag")) # time out of bounds
  expect_warning(chemdose_chlordecay(water2, cl2_dose = 0.995, time = 100)) # toc out of bounds
  expect_warning(chemdose_chlordecay(water3, cl2_dose = 2, time = 100, treatment = "coag")) # uv254 out of bounds
})

test_that("chemdose_chlordecay warns about chloramines", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50, tot_nh3 = 3))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 30, toc = 2, uv254 = 0.01, br = 30)) %>%
    chemdose_ph(nh42so4 = 3)
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))

  expect_warning(chemdose_chlordecay(water1, cl2_dose = 2, time = 8, cl_type = "chloramine"), "breakpoint+")
  expect_warning(chemdose_chlordecay(water2, cl2 = 4, time = 8), "breakpoint+")
  expect_no_warning(chemdose_chlordecay(water3, cl2 = 4, time = 8))
})

test_that("chemdose_chlordecay stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, br = 50, uv254 = 0.1))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2))
  water5 <- suppressWarnings(define_water(ph = 8, temp = 25, toc = 3, uv = 0.2))

  expect_error(chemdose_chlordecay(water1, cl_type = "chloramine", cl2_dose = 2, time = 1)) # missing uv254
  expect_error(chemdose_chlordecay(water2, cl2_dose = 2, time = 1, treatment = "coag")) # missing toc
  expect_no_error(suppressWarnings(chemdose_chlordecay(water3, cl2_dose = 4, time = 0.22, treatment = "coag"))) # raw doesn't require uv
  expect_error(chemdose_chlordecay(water5, time = 1, treatment = "coag")) # missing cl2_dose
  expect_error(chemdose_chlordecay(water5, cl2_dose = 4, treatment = "coag")) # missing time
})

test_that("chemdose_chlordecay correctly uses use_chlorine_slot", {
  water0 <- suppressWarnings(
    define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50, free_chlorine = 2, combined_chlorine = 3)
  )

  ###* CHLORINE ----
  water1 <- suppressWarnings(water0 %>%
    chemdose_chlordecay(cl_type = "chlorine", time = 10, cl2_dose = 6))

  water2 <- suppressWarnings(water0 %>%
    chemdose_chlordecay(cl_type = "chlorine", time = 10, cl2_dose = 6, use_chlorine_slot = TRUE))

  # compare output of using slot to not using slot
  expect_error(expect_equal(water2@free_chlorine, water1@free_chlorine))
  # check that free_chlorine was still calculated
  expect_error(expect_equal(water2@free_chlorine, 0))
  # check that the correct warning is thrown
  expect_warning(chemdose_chlordecay(water1, cl_type = "chlorine", time = 10, cl2_dose = 6, use_chlorine_slot = TRUE), "summed")
  # check that combined chlorine wasn't used in either permutation
  expect_equal(water1@combined_chlorine, water0@combined_chlorine)
  expect_equal(water2@combined_chlorine, water0@combined_chlorine)

  # use slot but no dose
  water3 <- water0 %>%
    chemdose_chlordecay(cl_type = "chlorine", time = 10, use_chlorine_slot = TRUE)

  # compare output of using slot w/o a dose to not using slot
  expect_error(expect_equal(water3@free_chlorine, water1@free_chlorine))
  # check that free_chlorine was still calculated
  expect_error(expect_equal(water3@free_chlorine, 0))
  # check that combined chlorine wasn't used in either permutation
  expect_equal(water3@combined_chlorine, water0@combined_chlorine)

  ###* CHLORAMINE ----
  water4 <- suppressWarnings(water0 %>%
    chemdose_chlordecay(cl_type = "chloramine", time = 10, cl2_dose = 6))

  water5 <- suppressWarnings(water0 %>%
    chemdose_chlordecay(cl_type = "chloramine", time = 10, cl2_dose = 6, use_chlorine_slot = TRUE))

  # compare output of using slot to not using slot
  expect_error(expect_equal(water5@combined_chlorine, water4@combined_chlorine))

  # check that combined_chlorine was still calculated
  expect_error(expect_equal(water5@combined_chlorine, 0))
  # check that the correct warning is thrown
  expect_warning(water4 %>% chemdose_chlordecay(cl_type = "chloramine", time = 10, cl2_dose = 6, use_chlorine_slot = TRUE), "summed")
  # check that free_chlorine wasn't used in either permutation
  expect_equal(water4@free_chlorine, water0@free_chlorine)
  expect_equal(water5@free_chlorine, water0@free_chlorine)

  # what if water slot is NA - check that combined_chlor still calc'd correctly
  water6 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50, free_chlorine = 2) %>%
    chemdose_chlordecay(cl_type = "chloramine", time = 10, cl2_dose = 6, use_chlorine_slot = TRUE))

  # check that output combined_chlor is the same as if not using the water slot (and that it doesn't accidentally use free_chlor instead)
  expect_equal(water4@combined_chlorine, water6@combined_chlorine)
  # check that free_chlor = 2 and cl2_dose = 6 did not calculate the same thing as water 2, which specified chlorine (not chloramine)
  expect_error(expect_equal(water2@free_chlorine, water6@free_chlorine))
  expect_error(expect_equal(water2@combined_chlorine, water6@combined_chlorine))

  # check that free_cl slot is unchanged
  expect_equal(convert_units(water6@free_chlorine, "cl2", "M", "mg/L"), 2)
})

test_that("chemdose_chlordecay works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- chemdose_chlordecay(water1, cl2_dose = 3, time = 8)
  water3 <- chemdose_chlordecay(water1, cl2_dose = 4, time = 3, treatment = "coag")
  water4 <- chemdose_chlordecay(water1, cl_type = "chloramine", cl2_dose = 4, time = 5, treatment = "coag")
  water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 1, uv254 = 0.04, br = 50))
  water6 <- chemdose_chlordecay(water5, cl_type = "chloramine", cl2_dose = 6, time = 10)

  expect_equal(signif(water2@free_chlorine, 3), 8.14E-6)
  expect_equal(signif(water3@free_chlorine, 3), 2.87E-5)
  expect_equal(signif(water4@combined_chlorine, 3), 5.24E-5)
  expect_equal(signif(water6@combined_chlorine, 3), 8.0E-5)
})

################################################################################*
################################################################################*
# chemdose_chlordecay helpers ----

test_that("chemdose_chlordecay_chain outputs are the same as base function, chemdose_chlordecay", {
  testthat::skip_on_cran()
  water0 <- define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13, mg = 4,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50,
    free_chlorine = 2, combined_chlorine = 1
  )

  water1 <- suppressWarnings(water0 %>%
    chemdose_chlordecay(cl2_dose = 10, time = 8))

  water2 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_chlordecay_chain(cl2_dose = 10, time = 8, output_water = "chlor") %>%
    pluck_water("chlor", "free_chlorine")


  water3 <- suppressWarnings(water0 %>%
    chemdose_chlordecay(cl2_dose = 10, time = 8, use_chlorine_slot = TRUE))

  water4 <- suppressWarnings(water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    mutate(free_chlorine = 2, combined_chlorine = 1) %>%
    define_water_chain() %>%
    chemdose_chlordecay_chain(cl2_dose = 10, time = 8, output_water = "chlor", use_chlorine_slot = TRUE)) %>%
    pluck_water("chlor", "free_chlorine")


  expect_equal(water1@free_chlorine, water2$chlor_free_chlorine)
  expect_equal(water3@free_chlorine, water4$chlor_free_chlorine)


  cldoses <- tibble(cl2_dose = seq(2, 8, 2))
  cltypes <- tibble(free_mono = c("chlorine", "chloramine"))
  water5 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    cross_join(cldoses) %>%
    cross_join(cltypes) %>%
    chemdose_chlordecay_chain(time = 4, cl_type = free_mono, output_water = "chlor") %>%
    pluck_water("chlor", "combined_chlorine")

  water6 <- suppressWarnings(chemdose_chlordecay(water0, cl2_dose = 4, time = 4, cl_type = "chloramine"))
  expect_equal(water6@combined_chlorine, water5$chlor_combined_chlorine[4])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_chlordecay_chain output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    slice(1) %>%
    mutate(br = 60) %>%
    define_water_chain() %>%
    chemdose_chlordecay_chain(time = 8, cl2_dose = 4)

  water2 <- purrr::pluck(water1, "disinfected_water", 1)

  water3 <- suppressWarnings(water_df %>%
    mutate(br = 60) %>%
    define_water_chain() %>%
    mutate(
      cl2_dose = 4,
      time = 8
    ) %>%
    chemdose_chlordecay_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check chemdose_chlordecay_chain can use a column or function argument for chemical dose

test_that("chemdose_chlordecay_chain can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(
      br = 80,
      free_chlorine = 2
    ) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_chlordecay_chain(input_water = "balanced_water", time = 120, cl2_dose = 10, use_chlorine_slot = TRUE) %>%
    pluck_water("disinfected_water", c("free_chlorine")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(
      br = 80,
      free_chlorine = 2
    ) %>%
    define_water_chain() %>%
    mutate(
      time = 120,
      cl2_dose = 10,
      use_chlorine_slot = TRUE
    ) %>%
    balance_ions_chain() %>%
    chemdose_chlordecay_chain(input_water = "balanced_water") %>%
    pluck_water("disinfected_water", c("free_chlorine")))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(
      br = 80,
      free_chlorine = 2
    ) %>%
    define_water_chain() %>%
    mutate(time = 120) %>%
    balance_ions_chain() %>%
    chemdose_chlordecay_chain(input_water = "balanced_water", cl2_dose = 10, use_chlorine_slot = TRUE) %>%
    pluck_water("disinfected_water", c("free_chlorine")))

  expect_equal(water1$disinfected_water_free_chlorine, water2$disinfected_water_free_chlorine) # test different ways to input args
  # Test that inputting time/cl2_dose separately (in column and as an argument) gives same results
  expect_equal(water1$disinfected_water_free_chlorine, water3$disinfected_water_free_chlorine)
})

test_that("chemdose_chlordecay_chain errors with argument + column for same param", {
  testthat::skip_on_cran()
  water <- water_df %>%
    mutate(free_chlorine = 2) %>%
    define_water_chain("water")
  expect_error(water %>%
    mutate(
      cl2_dose = 5,
      use_chlorine_slot = TRUE
    ) %>%
    chemdose_chlordecay_chain(
      input_water = "water", time = 120, cl2_dose = 10,
      use_chlorine_slot = TRUE
    ))
  expect_error(water %>%
    mutate(time = 5) %>%
    chemdose_chlordecay_chain(input_water = "water", time = 120, cl2_dose = 10))
})

test_that("chemdose_chlordecay_chain correctly handles arguments with multiple numbers", {
  testthat::skip_on_cran()
  water <- water_df %>%
    define_water_chain("water")

  water1 <- suppressWarnings(water %>%
    chemdose_chlordecay_chain("water", time = c(60, 120), cl2_dose = 5))
  water2 <- suppressWarnings(water %>%
    chemdose_chlordecay_chain("water", time = 120, cl2_dose = seq(2, 4, 1)))

  expect_equal(nrow(water) * 2, nrow(water1))
  expect_equal(nrow(water) * 3, nrow(water2))
})
