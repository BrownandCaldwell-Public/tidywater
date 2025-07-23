# Bromate formation ----

test_that("ozonate_bromate returns no modeled bromate when ozone dose is 0 or time is 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))
  bromate1 <- suppressWarnings(ozonate_bromate(water1, dose = 0, time = 10, model = "Sohn"))
  bromate2 <- suppressWarnings(ozonate_bromate(water1, dose = 2, time = 0, model = "Sohn"))

  expect_equal(bromate1@bro3, 0)
  expect_equal(bromate2@bro3, 0)
})

test_that("ozonate_bromate does not run when model isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, toc = 3.5, uv254 = 0.1, br = 30))

  expect_error(ozonate_bromate(water1, model = "oops"))
  expect_error(ozonate_bromate(water1, water_type = 5))
})

test_that("ozonate_bromate stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- suppressWarnings(define_water(ph = 7.5, uv254 = 0.1, br = 5))
  water3 <- suppressWarnings(define_water(ph = 8, toc = 3, br = 50))
  water4 <- suppressWarnings(define_water(ph = 8, toc = 3, uv = 0.2, br = NA_real_))
  water5 <- suppressWarnings(define_water(ph = 8, temp = 25, toc = 3, uv = 0.2, br = 50))

  expect_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Ozekin")) # missing ph
  expect_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Ozekin")) # missing toc
  expect_error(ozonate_bromate(water4, dose = 4, time = 8, model = "Ozekin")) # missing br
  expect_error(ozonate_bromate(water5, time = 8, model = "Ozekin")) # missing dose
  expect_error(ozonate_bromate(water5, dose = 4, model = "Ozekin")) # missing time
})

test_that("ozonate_bromate stops working when models don't line up with ammonia inputs", {
  water1 <- suppressWarnings(define_water(ph = 8, alk = 50, toc = 3.5, uv254 = 0.1, br = 50))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 50, toc = 3.5, uv254 = 0.1, br = 50, tot_nh3 = 2))

  expect_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Song")) # Song model requires ammonia
  expect_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Galey")) # Galey model does not use ammonia
  expect_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Siddiqui")) # Siddiqui model does not use ammonia

  expect_no_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Ozekin"))
  expect_no_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Ozekin"))

  expect_no_error(ozonate_bromate(water1, dose = 4, time = 8, model = "Sohn"))
  expect_no_error(ozonate_bromate(water2, dose = 4, time = 8, model = "Sohn"))
})


test_that("ozonate_bromate works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100, doc = 3.5, uv254 = 0.1, br = 50))
  water2 <- ozonate_bromate(water1, dose = 1, time = 10, model = "Ozekin")
  water3 <- ozonate_bromate(water1, dose = 1, time = 10, model = "Sohn")
  water4 <- ozonate_bromate(water1, dose = 1, time = 10, model = "Galey")

  water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100, doc = 3.5, uv254 = 0.1, br = 50, tot_nh3 = 1))
  water6 <- ozonate_bromate(water5, dose = 1, time = 10, model = "Song")
  water7 <- ozonate_bromate(water5, dose = 1, time = 10, model = "Ozekin")

  expect_equal(round(water2@bro3, 1), 1.3)
  expect_equal(round(water3@bro3, 1), 1.7)
  expect_equal(round(water4@bro3, 1), 2.4)
  expect_equal(round(water6@bro3, 1), 0.3)
  expect_equal(round(water7@bro3, 1), 1.2)
})

################################################################################*
################################################################################*
# ozonate_bromate helpers ----

test_that("ozonate_bromate_df outputs are the same as base function, ozonate_bromate", {
  testthat::skip_on_cran()
  water0 <- define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13, mg = 4,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )
  water1 <- water0 %>%
    ozonate_bromate(dose = 3, time = 5)

  water2 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_df() %>%
    ozonate_bromate_df(dose = 3, time = 5, output_water = "ozone", pluck_cols = TRUE)

  models <- tibble(bromate_model = c("Ozekin", "Sohn", "Galey", "Siddiqui"))
  doses <- tibble(dose = seq(1, 3, .5))
  water3 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_df() %>%
    cross_join(models) %>%
    cross_join(doses) %>%
    ozonate_bromate_df(time = 5, model = bromate_model, output_water = "ozone", pluck_cols = TRUE)

  water4 <- ozonate_bromate(water0, dose = 2.5, time = 5, model = "Galey")

  badmodels <- tibble(bromate_model = c("Ozekin", "Song", "Galey"))
  water5 <- water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_df() %>%
    cross_join(badmodels) %>%
    cross_join(doses)

  expect_error(
    ozonate_bromate_df(water5, time = 5, model = bromate_model, output_water = "ozone")
  )

  expect_equal(water1@bro3, water2$ozone_bro3)
  expect_equal(water4@bro3, water3$ozone_bro3[14])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("ozonate_bromate_df output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 60) %>%
    define_water_df() %>%
    ozonate_bromate_df(time = 5, dose = 3))

  water2 <- purrr::pluck(water1, "ozonated", 1)

  water3 <- suppressWarnings(water_df %>%
    mutate(br = 60) %>%
    define_water_df() %>%
    mutate(
      dose = 3,
      time = 5
    ) %>%
    ozonate_bromate_df(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check ozonate_bromate_df can use a column or function argument for chemical dose

test_that("ozonate_bromate_df can use a column or function argument for chemical dose, time", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_df("watta") %>%
    ozonate_bromate_df(input_water = "watta", time = 5, dose = 3, pluck_cols = TRUE))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_df() %>%
    mutate(
      time = 5,
      dose = 3,
    ) %>%
    ozonate_bromate_df(pluck_cols = TRUE))

  # pluck_cols does the same thing as pluck_water
  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_df() %>%
    mutate(time = 5) %>%
    ozonate_bromate_df(dose = 3) %>%
    pluck_water("ozonated", c("bro3")))

  expect_equal(water1$ozonated_bro3, water2$ozonated_bro3) # test different ways to input args
  # Test that inputting time/dose separately (in column and as an argument) gives same results
  expect_equal(water1$ozonated_bro3, water3$ozonated_bro3)
})

test_that("ozonate_bromate_df multiple models", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_df() %>%
    cross_join(tibble(model = c("Sohn", "Galey"))) %>%
    ozonate_bromate_df(time = 5, dose = 3) %>%
    pluck_water("ozonated", c("bro3")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_df() %>%
    ozonate_bromate_df(time = 5, dose = 3, model = "Sohn") %>%
    pluck_water("ozonated", c("bro3")))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_df() %>%
    ozonate_bromate_df(time = 5, dose = 3, model = c("Sohn", "Galey")) %>%
    pluck_water("ozonated", c("bro3")))

  expect_equal(water1$ozonated_bro3[1], water2$ozonated_bro3) # test different ways to input args
  expect_equal(water1$ozonated_bro3, water3$ozonated_bro3)
})

test_that("ozonate_bromate_df errors with argument + column for same param", {
  testthat::skip_on_cran()
  water <- water_df %>%
    define_water_df("water")
  expect_error(water %>%
    mutate(dose = 5) %>%
    ozonate_bromate_df(input_water = "water", time = 5, dose = 5))
  expect_error(water %>%
    mutate(time = 5) %>%
    ozonate_bromate_df(input_water = "water", time = 5, dose = 5))
})

test_that("ozonate_bromate_df correctly handles arguments with multiple values", {
  testthat::skip_on_cran()
  water <- water_df %>%
    mutate(br = 10) %>%
    slice(1:2) %>%
    define_water_df()

  water1 <- water %>%
    ozonate_bromate_df(time = c(5, 10), dose = c(1, 2, 5))
  water2 <- water %>%
    ozonate_bromate_df(time = 5, dose = c(2, 5), model = c("Sohn", "Galey"))

  expect_equal(nrow(water) * 6, nrow(water1))
  expect_equal(nrow(water) * 4, nrow(water2))
})
