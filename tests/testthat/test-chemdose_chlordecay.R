# Chemdose chlorine/chloramine ----

test_that("chemdose_chlordecay returns modeled chlorine/chloramine residual = 0 when chlorine dose is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2))
  Ct <- suppressWarnings(chemdose_chlordecay(water1, cl2_dose = 0, time = 8))

  expect_equal(water1@ocl, 0)
})

test_that("chemdose_chlordecay does not run when treatment_type isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1))

  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, treatment = "rw"))
  expect_error(chemdose_chlordecay(water1, cl2_dose = 1, time = 1, treatment = treated))
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

test_that("chemdose_chlordecay works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- chemdose_chlordecay(water1, cl2_dose = 3, time = 8)
  water3 <- chemdose_chlordecay(water1, cl2_dose = 4, time = 3, treatment = "coag")
  water4 <- chemdose_chlordecay(water1, cl_type = "chloramine", cl2_dose = 4, time = 5, treatment = "coag")
  water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 1, uv254 = 0.04, br = 50))
  water6 <- chemdose_chlordecay(water5, cl_type = "chloramine", cl2_dose = 6, time = 10)

  expect_equal(signif(water2@free_chlorine, 3), 1.33E-5)
  expect_equal(signif(water3@free_chlorine, 3), 3.28E-5)
  expect_equal(signif(water4@combined_chlorine, 3), 5.24E-5)
  expect_equal(signif(water6@combined_chlorine, 3), 8.0E-5)
})

################################################################################*
################################################################################*
# chemdose_chlordecay helpers ----
test_that("chemdose_chlordecay_once outputs are the same as base function, chemdose_chlordecay", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )) %>%
    chemdose_chlordecay(cl2_dose = 10, time = 8)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_chlordecay_once(cl2_dose = 10, time = 8))

  expect_equal(water1@free_chlorine, water2$free_chlorine)
})

# Check that output is a data frame

test_that("chemdose_chlordecay_once is a data frame", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_chlordecay_once(
      input_water = "balanced_water",
      cl2_dose = 5, time = 100
    ))

  expect_true(is.data.frame(water1))
})

# Check chemdose_chlordecay_once can use a column or function argument for chemical dose

test_that("chemdose_chlordecay_once can use a column or function argument for chemical dose", {
  water1 <- water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_chlordecay_once(
      cl2_dose = 5, time = 100
    )
  water2 <- water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    mutate(
      cl2_dose = 5,
      time = 100
    ) %>%
    chemdose_chlordecay_once()

  water3 <- water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    mutate(cl2_dose = 5) %>%
    chemdose_chlordecay_once(time = 100)

  expect_equal(water1$free_chlorine, water2$free_chlorine) # test different ways to input args
  # Test that inputting cl2_dose and time separately (in column and as an argument)  gives save results
  expect_equal(water1$free_chlorine, water3$free_chlorine)
})

test_that("chemdose_chlordecay_chain outputs are the same as base function, chemdose_chlordecay", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )) %>%
    chemdose_chlordecay(cl2_dose = 10, time = 8)

  water2 <- suppressWarnings(water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_chlordecay_chain(cl2_dose = 10, time = 8, output_water = "chlor") %>%
    pluck_water("chlor", c(
      "free_chlorine"
    )))

  expect_equal(water1@free_chlorine, water2$chlor_free_chlorine)
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_chlordecay_chain output is list of water class objects, and can handle an ouput_water arg", {
  water1 <- water_df %>%
    slice(1) %>%
    mutate(br = 60) %>%
    define_water_chain() %>%
    chemdose_chlordecay_chain(time = 8, cl2_dose = 4)

  water2 <- purrr::pluck(water1, 6, 1)

  water3 <- suppressWarnings(water_df %>%
    mutate(br = 60) %>%
    define_water_chain() %>%
    mutate(
      cl2_dose = 4,
      time = 8
    ) %>%
    chemdose_chlordecay_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[6]), "diff_name") # check if output_water arg works
})

# Check chemdose_chlordecay_chain can use a column or function argument for chemical dose

test_that("chemdose_chlordecay_chain can use a column or function argument for chemical dose", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_chlordecay_chain(input_water = "balanced_water", time = 120, cl2_dose = 10) %>%
    pluck_water("disinfected_water", c("free_chlorine")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    mutate(
      time = 120,
      cl2_dose = 10,
    ) %>%
    balance_ions_chain() %>%
    chemdose_chlordecay_chain(input_water = "balanced_water") %>%
    pluck_water("disinfected_water", c("free_chlorine")))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    mutate(time = 120) %>%
    balance_ions_chain() %>%
    chemdose_chlordecay_chain(input_water = "balanced_water", cl2_dose = 10) %>%
    pluck_water("disinfected_water", c("free_chlorine")))

  expect_equal(water1$disinfected_water_free_chlorine, water2$disinfected_water_free_chlorine) # test different ways to input args
  # Test that inputting time/cl2_dose separately (in column and as an argument)  gives save results
  expect_equal(water1$disinfected_water_free_chlorine, water3$disinfected_water_free_chlorine)
})

test_that("chemdose_chlordecay_chain errors with argument + column for same param", {
  water <- water_df %>%
    define_water_chain("water")
  expect_error(water %>%
    mutate(cl2_dose = 5) %>%
    chemdose_chlordecay_chain(input_water = "water", time = 120, cl2_dose = 10))
  expect_error(water %>%
    mutate(time = 5) %>%
    chemdose_chlordecay_chain(input_water = "water", time = 120, cl2_dose = 10))
})

test_that("chemdose_chlordecay_chain correctly handles arguments with multiple numbers", {
  water <- water_df %>%
    define_water_chain("water")

  water1 <- water %>%
    chemdose_chlordecay_chain("water", time = c(60, 120), cl2_dose = 5)
  water2 <- water %>%
    chemdose_chlordecay_chain("water", time = 120, cl2_dose = seq(2, 4, 1))

  expect_equal(nrow(water) * 2, nrow(water1))
  expect_equal(nrow(water) * 3, nrow(water2))
})
