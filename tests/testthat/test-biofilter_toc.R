# biofilter_toc ----

test_that("biofilter_toc returns an error when water is absent or input incorrectly.", {
  expect_error(biofilter_toc(ebct = 10, ozonated = TRUE))
  expect_error(biofilter_toc(water = list(ph = 7, toc = 5), ebct = 10, ozonated = TRUE))
})

test_that("biofilter_toc returns an error or warning when arguments are input improperly or missing.", {
  water <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))

  expect_error(biofilter_toc(water, ozonated = TRUE))
  expect_error(biofilter_toc(water, ebct = "4", ozonated = FALSE))

  expect_error(biofilter_toc(water, ebct = 4, ozonated = 2))
  expect_error(biofilter_toc(water, ebct = 4, ozonated = "TRUE"))
})

test_that("biofilter_toc returns an error when TOC is missing.", {
  water_no_toc <- suppressWarnings(define_water(ph = 7, temp = 15, alk = 100))
  expect_error(biofilter_toc(water_no_toc, ebct = 10, ozonated = TRUE))
})

test_that("biofilter_toc calculates different TOC removal for ozonated vs non-ozonated water.", {
  water <- suppressWarnings(define_water(ph = 7, temp = 15, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))
  no_ozone <- biofilter_toc(water, ebct = 10, ozonated = FALSE)
  ozone <- biofilter_toc(water, ebct = 10, ozonated = TRUE)

  # Expect that TOC is reduced correctly using non-ozonated parameters
  expect_equal(round(no_ozone@toc, 2), 3.53) # Expected TOC after treatment
  expect_equal(round(no_ozone@doc, 2), 3.53) # Expected DOC (BDOC fraction of TOC)
  expect_equal(round(ozone@doc, 2), 3.46) # Expected DOC (BDOC fraction of TOC)
})

test_that("biofilter_toc correctly handles temperatures and non-ozonated water.", {
  water <- suppressWarnings(define_water(ph = 7, temp = 45, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))

  # the Bad Place temperature, non-ozonated
  dosed_water_high <- suppressWarnings(biofilter_toc(water, ebct = 10, ozonated = FALSE))
  expect_equal(round(dosed_water_high@doc, 2), 3.47)

  # the Medium Place temperature, non-ozonated
  water@temp <- 19
  dosed_water_med <- suppressWarnings(biofilter_toc(water, ebct = 10, ozonated = FALSE))
  expect_equal(round(dosed_water_med@doc, 2), 3.53)

  # the Good Place temperature, non-ozonated
  water@temp <- 5
  dosed_water_low <- suppressWarnings(biofilter_toc(water, ebct = 10, ozonated = FALSE))
  expect_equal(round(dosed_water_low@doc, 2), 3.79)
})

test_that("biofilter_toc correctly handles temperatures and ozonated water.", {
  water <- suppressWarnings(define_water(ph = 7, temp = 45, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1))

  dosed_water_high <- suppressWarnings(biofilter_toc(water, ebct = 10, ozonated = TRUE))
  expect_equal(round(dosed_water_high@toc, 2), 3.07)

  water@temp <- 19
  dosed_water_med <- suppressWarnings(biofilter_toc(water, ebct = 10, ozonated = TRUE))
  expect_equal(round(dosed_water_med@toc, 2), 3.46)

  water@temp <- 5
  dosed_water_low <- suppressWarnings(biofilter_toc(water, ebct = 10, ozonated = TRUE))
  expect_equal(round(dosed_water_low@toc, 2), 3.69)
})

################################################################################*
################################################################################*
# biofilter_toc helpers ----

test_that("biofilter_toc_chain outputs are the same as base function, biofilter_toc", {
  water0 <- define_water(7.9, 20, 50,
    tot_hard = 50, ca = 13, mg = 4,
    na = 20, k = 20, cl = 30, so4 = 20,
    tds = 200, cond = 100,
    toc = 2, doc = 1.8, uv254 = 0.05
  )

  water1 <- biofilter_toc(water0, ebct = 10)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    biofilter_toc_chain(ebct = 10, output_water = "biof") %>%
    pluck_water("biof", c("doc"))

  ebcts <- tibble(EBCT = c(10, 20, 30))
  ozone <- tibble(ozonated = c(T, F))
  water3 <- water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    cross_join(ebcts) %>%
    cross_join(ozone) %>%
    biofilter_toc_chain("raw", "biof", ebct = EBCT) %>%
    pluck_water("biof", c("doc"))

  water4 <- biofilter_toc(water0, ebct = 20, ozonated = F)

  expect_equal(water1@doc, water2$biof_doc)
  expect_equal(water4@doc, water3$biof_doc[4])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("biofilter_toc_chain output is list of water class objects, and can handle an ouput_water arg", {
  water1 <- water_df %>%
    slice(1) %>%
    define_water_chain("water") %>%
    biofilter_toc_chain(input_water = "water", ebct = 8)

  water2 <- purrr::pluck(water1, "biofiltered_water", 1)

  water3 <- water_df %>%
    define_water_chain() %>%
    mutate(
      ebct = 4
    ) %>%
    biofilter_toc_chain(output_water = "diff_name")

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check biofilter_toc_chain can use a column or function argument for chemical dose

test_that("biofilter_toc_chain can use a column or function argument for chemical dose", {
  water1 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    biofilter_toc_chain(ebct = 10, ozonated = TRUE) %>%
    pluck_water("biofiltered_water", c("doc"))

  water2 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(
      ebct = 10,
    ) %>%
    biofilter_toc_chain() %>%
    pluck_water("biofiltered_water", c("doc"))

  water3 <- water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(ozonated = TRUE) %>%
    biofilter_toc_chain(ebct = 10) %>%
    pluck_water("biofiltered_water", c("doc"))

  expect_equal(water1$biofiltered_water_doc, water2$biofiltered_water_doc) # test different ways to input args
  # Test that inputting ozonated/ebct separately (in column and as an argument) gives same results
  expect_equal(water1$biofiltered_water_doc, water3$biofiltered_water_doc)
})

test_that("biofilter_toc_chain errors with argument + column for same param", {
  water <- water_df %>%
    define_water_chain("water")
  expect_error(water %>%
    mutate(ebct = 5) %>%
    biofilter_toc_chain(input_water = "water", ebct = 10, ozonated = FALSE))

  # This doesn't work because the function can't see the difference between an argument the user enters and the default ozonated = TRUE
  # Eventually remove helper defaults? Not sure.
  # expect_error(water %>%
  #   mutate(ozonated = FALSE) %>%
  #   biofilter_toc_chain(input_water = "water", ebct = 10, ozonated = TRUE))
})

test_that("biofilter_toc_chain correctly handles arguments with multiple numbers", {
  water <- water_df %>%
    define_water_chain("water")

  water1 <- water %>%
    biofilter_toc_chain("water", ebct = seq(10, 30, 5), ozonated = c(TRUE, FALSE))

  expect_equal(nrow(water) * 10, nrow(water1))
})
