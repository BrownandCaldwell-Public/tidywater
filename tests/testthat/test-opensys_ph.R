# opensys_ph ----

test_that("opensys_ph errors without correct inputs", {
  water0 <- suppressWarnings(define_water(ph = 7, alk = 10))
  water1 <- suppressWarnings(define_water(alk = 10))
  
  expect_error(opensys_ph(partialpressure = 10^-4)) # no water input
  expect_error(opensys_ph(partpressure = 10^-4))
  expect_no_error(opensys_ph(water0))
  expect_error(opensys_ph(water1)) # missing ph in the water
  
})

test_that("opensys_ph preserves carbonate balance", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10))
  water2 <- opensys_ph(water1)
  
  expect_equal(water2@tot_co3, sum(water2@h2co3, water2@hco3, water2@co3))
  expect_equal(signif(water2@h2co3, 2), 1.3*10^-5)
})

test_that("opensys_ph works", {
  water0 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 10))
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 0))
  
  water2 <- opensys_ph(water0)
  water3 <- opensys_ph(water0, partialpressure = 10^-4)
  water4 <- opensys_ph(water1)
  
  expect_s4_class(water2, "water")
  expect_false(identical(water2@ph, water0@ph))
  expect_false(identical(water2@ph, water3@ph))
  expect_true(water2@alk > water0@alk) #carbonate leaving or entering water based on equilibrium
  expect_true(water2@alk > water1@alk)
  expect_true(water2@ph > water0@ph)
  expect_true(water2@ph < water3@ph)
  expect_true(water4@alk > water1@alk) # if h2co3 starts less than equilibrium, alkalinity will increase
  
  expect_equal(round(water2@ph, 1), 7.5)
  expect_equal(round(water2@alk, 4), 10.0074)
  expect_equal(signif(water2@alk_eq, 3), 0.00020)
})

test_that("opensys_ph_df outputs are the same as base function, opensys_ph.", {
  testthat::skip_on_cran()
  water0 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ))
  
  water1 <- opensys_ph(water0)
  
  water2 <- suppressWarnings(water_df %>%
                               dplyr::slice(1) %>%
                               define_water_df() %>%
                               opensys_ph_df(pluck_cols = TRUE))
  
  # test that pluck_cols does the same thing as pluck_water
  water3 <- suppressWarnings(water_df %>%
                               dplyr::slice(1) %>%
                               define_water_df() %>%
                               opensys_ph_df() %>%
                               pluck_water(c("opensys"), c("ph", "alk")))
  
  expect_equal(water1@ph, water2$opensys_ph)
  expect_equal(water1@alk, water2$opensys_alk)
  expect_equal(water2$opensys_ph, water3$opensys_ph)
  expect_equal(ncol(water2), ncol(water3))
})

test_that("opensys df takes and returns correct argument types and classes.", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df("test")
  
  water1 <- opensys_ph_df(water0, "test", "opensys", partialpressure = 10^-4)
  water2 <- water0 %>%
    mutate(partialp = 10^-4) %>%
    opensys_ph_df("test", "opensys", partialpressure = partialp)
  
  expect_error(opensys_ph_df(water_df, partialpressure = .9))
  expect_error(opensys_ph_df(water0))
  expect_s4_class(water1$opensys[[1]], "water")
  expect_equal(water1$opensys, water2$opensys)
})

test_that("opensys_ph_df can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    define_water_df()
  
  water1 <- opensys_ph_df(water0, partialpressure = 10^-4, pluck_cols = TRUE)
  water2 <- water0 %>%
    mutate(partialp = 10^-4) %>%
    opensys_ph_df(partialpressure = partialp, pluck_cols = TRUE)
  water3 <- water0 %>%
    opensys_ph_df(output_water = "opensys_water", partialpressure = 10^-4, pluck_cols = TRUE)
  
  expect_equal(water1$opensys_ph, water2$opensys_ph)
  expect_equal(water1$opensys_ph, water3$opensys_water_ph)
  expect_equal(water1$opensys_alk, water2$opensys_alk)
})
