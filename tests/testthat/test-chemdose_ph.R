# chemdose_ph ----

test_that("chemdose ph returns the same pH/alkalinity when no chemical is added.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- chemdose_ph(water1, h2so4 = 0, h3po4 = 0)

  water3 <- define_water(ph = 7, temp = 20, alk = 100, tot_po4 = 2, tds = 200)
  water4 <- chemdose_ph(water3, naocl = 0, alum = 0, naoh = 0)

  expect_equal(water1@ph, water2@ph)
  expect_equal(water1@alk, water2@alk)
  expect_equal(water3@ph, water4@ph)
  expect_equal(water3@alk, water4@alk)
})

test_that("chemdose ph corrects ph when softening", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 350))
  water2 <- chemdose_ph(water1, caco3 = -100)
  water3 <- chemdose_ph(water1, caco3 = -100, softening_correction = TRUE)
  water4 <- chemdose_ph(water1, caco3 = 10, softening_correction = TRUE)
  water5 <- chemdose_ph(water1, caco3 = 10)

  expect_equal(round(water3@ph, 2), 3.86) # softening correction works
  expect_error(expect_equal(water2@ph, water3@ph)) # ph with/without softening correction are different
  expect_equal(water4@ph, water5@ph) # softening_correction should not affect pH without caco3 <0
})


# To do: subdivide for each chemical?
test_that("chemdose ph works", {
  water1 <- define_water(6.7, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- define_water(7.5, 20, 100, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water3 <- define_water(7.5, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  test1 <- suppressWarnings(chemdose_ph(water1, alum = 30))
  test2 <- suppressWarnings(chemdose_ph(water2, alum = 30))
  test3 <- suppressWarnings(chemdose_ph(water2, alum = 50, h2so4 = 20))
  test4 <- suppressWarnings(chemdose_ph(water3, alum = 50, naoh = 10))
  test5 <- suppressWarnings(chemdose_ph(water4, alum = 50))
  test6 <- suppressWarnings(chemdose_ph(water4, naoh = 80))
  test7 <- suppressWarnings(chemdose_ph(water1, nh42so4 = 5))
  test8 <- suppressWarnings(chemdose_ph(water4, nh4oh = 5))

  # Rounded values from waterpro and WTP spot check
  expect_equal(round(test1@ph, 1), 5.7)
  expect_equal(round(test1@alk, 0), 5)
  expect_equal(round(test2@ph, 1), 6.9)
  expect_equal(round(test2@alk, 0), 85)
  expect_equal(round(test3@ph, 1), 6.4)
  expect_equal(round(test3@alk, 0), 55)
  expect_equal(round(test4@ph, 1), 6.1)
  expect_equal(round(test4@alk, 0), 7)
  expect_equal(round(test5@ph, 1), 4.0)
  expect_equal(round(test5@alk, 0), -5)
  expect_equal(round(test6@ph, 1), 11.4)
  expect_equal(round(test6@alk, 0), 119)
  expect_equal(round(test7@ph, 1), 6.7)
  expect_equal(round(test7@alk, 0), 20)
  expect_equal(round(test8@ph, 1), 9.7)
  expect_equal(round(test8@alk, 0), 26)
})

test_that("Starting phosphate residual does not affect starting pH.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, tot_po4 = 5) %>%
    chemdose_ph())

  water2 <- water1 %>%
    chemdose_ph()

  water3 <- water2 %>%
    chemdose_ph()

  expect_equal(water1@ph, 7)
  expect_equal(water2@ph, 7)
  expect_equal(water3@ph, 7)
})

test_that("Phosphate dose works as expected.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 50, tot_po4 = 0, temp = 25))

  water2 <- chemdose_ph(water1, h3po4 = 1) # 0.969 as PO4
  water3 <- chemdose_ph(water1, h3po4 = 5) # 4.84 as PO4
  water4 <- chemdose_ph(water1, h3po4 = 10) # 9.69 as PO4

  expect_equal(round(water2@ph, 1), 7.0)
  expect_equal(round(water3@ph, 1), 6.9)
  expect_equal(round(water4@ph, 1), 6.7)
})

test_that("Starting chlorine residual does not affect starting pH.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, free_chlorine = 1) %>%
    chemdose_ph())

  water2 <- water1 %>%
    chemdose_ph()

  water3 <- water2 %>%
    chemdose_ph()

  expect_equal(water1@ph, 7)
  expect_equal(water2@ph, 7)
  expect_equal(water3@ph, 7)
})

test_that("Starting ammonia does not affect starting pH.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, tot_nh3 = 1) %>%
    chemdose_ph())

  water2 <- water1 %>%
    chemdose_ph()

  water3 <- water2 %>%
    chemdose_ph()

  expect_equal(water1@ph, 7)
  expect_equal(water2@ph, 7)
  expect_equal(water3@ph, 7)
})

test_that("Warning when both chlorine- and ammonia-based chemical are dosed.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, na = 2, cl = 2, so4 = 2))

  expect_warning(chemdose_ph(water1, cl2 = 30, nh42so4 = 20))
  expect_warning(chemdose_ph(water1, naocl = 30, nh42so4 = 20))
  expect_warning(chemdose_ph(water1, cl2 = 30, nh4oh = 20))
  expect_no_warning(chemdose_ph(water1, cl2 = 30, naocl = 20))
  expect_no_warning(chemdose_ph(water1, nh4oh = 10, nh42so4 = 12))
  expect_no_warning(chemdose_ph(water1, hcl = 20))
})

test_that("Warning when chlorine-based chemical is dosed into water containing ammonia", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, tot_nh3 = 3, na = 2, cl = 2, so4 = 2))
  water2 <- suppressWarnings(define_water(ph = 7, alk = 10, na = 2, cl = 2, so4 = 2)) %>%
    chemdose_ph(nh4oh = 4)

  expect_warning(chemdose_ph(water1, cl2 = 30))
  expect_warning(chemdose_ph(water2, naocl = 30))
  expect_no_warning(chemdose_ph(water1, nh4oh = 20))
  expect_no_warning(chemdose_ph(water1, hcl = 20))
})

test_that("Warning when ammonia-based chemical is dosed into water containing chlorine", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, free_chlorine = 3, na = 2, cl = 2, so4 = 2))
  water2 <- suppressWarnings(define_water(ph = 7, alk = 10, na = 2, cl = 2, so4 = 2)) %>%
    chemdose_ph(naocl = 4)

  expect_warning(chemdose_ph(water1, nh42so4 = 30))
  expect_warning(chemdose_ph(water2, nh4oh = 30))
  expect_no_warning(chemdose_ph(water1, cl2 = 20))
  expect_no_warning(chemdose_ph(water1, hcl = 20))
})

test_that("Warning when water slot is NA", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10))
  water2 <- suppressWarnings(chemdose_ph(water1, naoh = 30))

  expect_warning(chemdose_ph(water1, naoh = 30), "Sodium")
  expect_equal(water2@na, NA_real_)

  expect_warning(chemdose_ph(water1, caoh = 30), "Calcium")
  expect_warning(chemdose_ph(water1, mgoh = 30), "Magne")
  expect_warning(chemdose_ph(water1, cl2 = 30), "Chloride")
  expect_warning(chemdose_ph(water1, h2so4 = 30), "Sulf")
  # expect_warning(chemdose_ph(water1, kmno4 = 30), "Potas")
})


################################################################################*
################################################################################*
# chemdose_ph helpers ----

# Test that chemdose_ph_chain outputs are the same as base function, chemdose_ph.
test_that("chemdose_ph_chain outputs the same as base, chemdose_ph", {
  testthat::skip_on_cran()
  water0 <- define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )

  water1 <- chemdose_ph(water0, naoh = 10)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_ph_chain(input_water = "defined", naoh = 10, pluck_cols = TRUE)
  
  # check that pluck_cols does the same thing as pluck_Water
  water3 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_ph_chain(input_water = "defined", naoh = 10) %>%
    pluck_water("defined", c("ph", "alk"))

  coag_doses <- tibble(alum = seq(0, 100, 10))
  softening <- tibble(softening_correction = c(T, F))
  water4 <- water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    cross_join(coag_doses) %>%
    cross_join(softening) %>%
    chemdose_ph_chain("raw", "dose", pluck_cols = TRUE)

  water5 <- chemdose_ph(water0, alum = 20)
  water6 <- chemdose_ph(water0, alum = 100, softening_correction = FALSE)

  water7 <- water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    mutate(naoh = 10) %>%
    cross_join(coag_doses) %>%
    rename(NewName = alum) %>%
    chemdose_ph_chain("raw", "dose", alum = NewName, naocl = c(0, 2), pluck_cols = TRUE)

  water8 <- chemdose_ph(water0, alum = 20, naocl = 2, naoh = 10)

  expect_equal(water2$dosed_chem_water_alk[1], water1@alk)
  expect_equal(water4$dose_ph[6], water5@ph)
  expect_equal(water4$dose_ph[22], water6@ph)
  expect_equal(water7$dose_ph[14], water8@ph)
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_ph_chain output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10))

  water2 <- purrr::pluck(water1, 4, 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[4]), "diff_name") # check if output_water arg works
})

# Check that this function can be piped to the next one
test_that("chemdose_ph_chain works", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water"))

  expect_equal(ncol(water1), 4) # check if pipe worked
})

# Check that variety of ways to input chemicals work
test_that("chemdose_ph_chain can handle different ways to input chem doses", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = 10, output_water = "dosed_chem"))

  water2 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = 10) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water"))

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(naoh = seq(0, 11, 1)) %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(hcl = c(5, 8)))

  water4 <- water3 %>%
    slice(21) # same starting wq as water 5

  water5 <- water1 %>%
    slice(11) # same starting wq as water 4

  expect_equal(
    pluck_water(water1, "dosed_chem", "toc")$dosed_chem_toc,
    pluck_water(water2, "dosed_chem_water", "toc")$dosed_chem_water_toc
  ) # test different ways to input chemical
  expect_equal(ncol(water3), 5) # both naoh and hcl dosed
  expect_equal(nrow(water3), 24) # joined correctly
  expect_error(expect_equal(
    pluck_water(water4, "dosed_chem_water", "ph")$dosed_chem_water_ph,
    pluck_water(water5, "dosed_chem", "ph")$dosed_chem_ph
  )) # since HCl added to water3, pH should be different
})

# Check that na_to_zero implementation works
test_that("chemdose_ph_chain na_to_zero argument works", {
  testthat::skip_on_cran()
  water <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_ph_chain(input_water = "balanced_water", naoh = c(1, 2, 3, 4, NA, 6, 7, 8, NA, 10, 11, 12)))
  expect_equal(water$naoh[50], 0)
  expect_equal(water$naoh[100], 0)
})
