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

################################################################################*
################################################################################*
# chemdose_dbp helpers ----
test_that("chemdose_dbp_once outputs are the same as base function, chemdose_dbp", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )) %>%
    chemdose_dbp(cl2 = 10, time = 8)

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    chemdose_dbp_once(cl2 = 10, time = 8))

  expect_equal(water1@chcl3, water2$chcl3)
  expect_equal(water1@chcl2br, water2$chcl2br)
  expect_equal(water1@chbr2cl, water2$chbr2cl)
  expect_equal(water1@chbr3, water2$chbr3)
  expect_equal(water1@tthm, water2$tthm)
  expect_equal(water1@mcaa, water2$mcaa)
  expect_equal(water1@dcaa, water2$dcaa)
  expect_equal(water1@tcaa, water2$tcaa)
  expect_equal(water1@mbaa, water2$mbaa)
  expect_equal(water1@dbaa, water2$dbaa)
  expect_equal(water1@haa5, water2$haa5)
})

# Check that output is a data frame

test_that("chemdose_dbp_once is a data frame", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_dbp_once(
      input_water = "balanced_water",
      cl2 = 5, time = 100
    ))

  expect_true(is.data.frame(water1))
})

# Check chemdose_dbp_once can use a column or function argument for chemical dose

test_that("chemdose_dbp_once can use a column or function argument for chemical dose", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_dbp_once(
      input_water = "balanced_water",
      cl2 = 5, time = 100
    ))
  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    mutate(
      cl2 = 5,
      time = 100
    ) %>%
    balance_ions_chain() %>%
    chemdose_dbp_once(input_water = "balanced_water"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 50) %>%
    define_water_chain() %>%
    mutate(cl2 = 5) %>%
    balance_ions_chain() %>%
    chemdose_dbp_once(input_water = "balanced_water", time = 100))

  expect_equal(water1$tthm, water2$tthm) # test different ways to input args
  expect_equal(water1$haa5, water2$haa5)
  expect_equal(water1$chcl2br, water2$chcl2br)

  # Test that inputting cl2 and time separately (in column and as an argument)  gives save results
  expect_equal(water1$tthm, water3$tthm)
  expect_equal(water2$haa5, water3$haa5)
  expect_equal(water2$chcl2br, water3$chcl2br)
})

test_that("chemdose_dbp_chain outputs are the same as base function, chemdose_dbp", {
  water1 <- suppressWarnings(define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05, br = 50
  )) %>%
    chemdose_dbp(cl2 = 10, time = 8)

  water2 <- suppressWarnings(water_df %>%
    mutate(br = 50) %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_dbp_chain(cl2 = 10, time = 8, output_water = "chlor") %>%
    pluck_water("chlor", c(
      "chcl3", "chcl2br", "chbr2cl", "chbr3", "tthm",
      "mcaa", "dcaa", "tcaa", "mbaa", "dbaa", "haa5"
    )))

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
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_dbp_chain output is list of water class objects, and can handle an ouput_water arg", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 60) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water", time = 8, cl2 = 4))

  water2 <- purrr::pluck(water1, 8, 1)

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
  expect_equal(names(water3[8]), "diff_name") # check if output_water arg works
})

# Check chemdose_dbp_chain can use a column or function argument for chemical dose

test_that("chemdose_dbp_chain can use a column or function argument for chemical dose", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water", time = 120, cl2 = 10) %>%
    pluck_water("disinfected_water", c("tthm", "haa5", "mbaa")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    mutate(
      time = 120,
      cl2 = 10,
    ) %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water") %>%
    pluck_water("disinfected_water", c("tthm", "haa5", "mbaa")))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    mutate(br = 80) %>%
    define_water_chain() %>%
    mutate(time = 120) %>%
    balance_ions_chain() %>%
    chemdose_dbp_chain(input_water = "balanced_water", cl2 = 10) %>%
    pluck_water("disinfected_water", c("tthm", "haa5", "mbaa")))

  expect_equal(water1$disinfected_water_tthm, water2$disinfected_water_tthm) # test different ways to input args
  expect_equal(water1$disinfected_water_haa5, water2$disinfected_water_haa5)
  expect_equal(water1$disinfected_water_mbaa, water2$disinfected_water_mbaa)

  # Test that inputting time/cl2 separately (in column and as an argument)  gives save results
  expect_equal(water1$disinfected_water_tthm, water3$disinfected_water_tthm)
  expect_equal(water2$disinfected_water_haa5, water3$disinfected_water_haa5)
  expect_equal(water2$disinfected_water_mbaa, water3$disinfected_water_mbaa)
})

test_that("chemdose_dbp_chain errors with argument + column for same param", {
  water <- water_df %>%
    mutate(br = 80) %>%
    define_water_chain("water")
  expect_error(water %>%
                 mutate(cl2 = 5)  %>%
                 chemdose_dbp_chain(input_water = "water", time = 120, cl2 = 10))
  expect_error(water %>%
                 mutate(time = 5)  %>%
                 chemdose_dbp_chain(input_water = "water", time = 120, cl2 = 10))
})

test_that("chemdose_dbp_chain correctly handles arguments with multiple numbers", {
  water <- water_df %>%
    mutate(br = 80) %>%
    define_water_chain("water")

  water1 <- water %>%
    chemdose_dbp_chain("water", time = c(60,120), cl2 = 5)
  water2 <- water %>%
    chemdose_dbp_chain("water", time = 120, cl2 = seq(2, 4, 1))

  expect_equal(nrow(water)*2, nrow(water1))
  expect_equal(nrow(water)*3, nrow(water2))

})
