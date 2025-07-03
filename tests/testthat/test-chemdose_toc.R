# Chemdose TOC ----

test_that("chemdose_toc returns the same water when coagulant dose is 0.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  toc_rem1 <- suppressWarnings(chemdose_toc(water1))

  water2 <- suppressWarnings(define_water(ph = 7, toc = 3.5, doc = 3.2, uv254 = 0.1))
  toc_rem2 <- suppressWarnings(chemdose_toc(water2))

  expect_equal(water1, toc_rem1)
  expect_equal(water2, toc_rem2)
})

test_that("chemdose_toc does not run when coeff isn't supplied correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))

  expect_error(chemdose_toc(water1, coeff = "k1"))
  expect_error(chemdose_toc(water1, coeff = c(1, 1, 1, 1, 1, 1)))
  expect_error(chemdose_toc(water1, coeff = edwardscoeff[1]))
})

test_that("chemdose_toc handles inputs correctly.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(define_water(ph = 7, uv254 = 0.1))

  expect_warning(chemdose_toc(water1, alum = 20, ferricchloride = 20))
  expect_error(chemdose_toc(water2, alum = 15))
})

test_that("chemdose_toc works.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  water2 <- suppressWarnings(chemdose_toc(water1, alum = 30))
  water3 <- suppressWarnings(chemdose_toc(water1, ferricchloride = 50, coeff = "Ferric"))
  water4 <- suppressWarnings(chemdose_toc(water1,
    ferricchloride = 50,
    coeff = data.frame(x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068)
  ))

  # Used to generate expected outputs cross check with edwards97 package
  # data = data.frame(DOC = 3.5, dose = convert_units(50, "ferricchloride", endunit = "mM"), pH = 7, UV254 = .1)
  # coagulate(data, coefs = edwards_coefs("Fe"))

  expect_equal(round(water2@doc, 1), 2.8)
  expect_equal(round(water3@doc, 1), 2.2)
  expect_equal(round(water4@doc, 1), 2.2)
})

################################################################################*
################################################################################*
# chemdose_toc helpers ----

# Check chemdose_toc_once outputs are the same as base function, chemdose_toc
# Check that output is a data frame

test_that("chemdose_toc_once outputs are the same as base function, chemdose_toc", {
  water1 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  ) %>%
    balance_ions() %>%
    chemdose_toc())

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water"))

  expect_equal(water1@toc, water2$toc)
  expect_equal(water1@doc, water2$doc)
  expect_equal(water1@uv254, water2$uv254)
})

# Check that output is a data frame

test_that("chemdose_toc_once is a data frame", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water"))


  expect_true(is.data.frame(water1))
  expect_equal(colnames(water1), c("defined_water", "balanced_water", "toc", "doc", "uv254"))
})

# Check chemdose_toc_once can use a column or function argument for chemical dose

test_that("chemdose_toc_once can use a column and/or function argument for chemical dose", {
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water", ferricchloride = 40, coeff = "Ferric"))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(
      ferricchloride = 40,
      coeff = "Ferric"
    ) %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water"))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(ferricchloride = 40) %>%
    balance_ions_chain() %>%
    chemdose_toc_once(input_water = "balanced_water", coeff = "Ferric"))

  expect_equal(water1$toc, water2$toc) # test different ways to input chemical
  expect_equal(water1$doc, water2$doc)
  expect_equal(water1$uv254, water2$uv254)
  expect_equal(water2$toc, water3$toc)
  expect_equal(water2$doc, water3$doc)
  expect_equal(water2$uv254, water3$uv254)
  expect_equal(ncol(water3), ncol(water1))
})

test_that("chemdose_toc_chain outputs are the same as base function, chemdose_toc", {
  testthat::skip_on_cran()
  water0 <- define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )

  water1 <- chemdose_toc(water0, ferricchloride = 40, coeff = "Ferric")

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    chemdose_toc_chain(ferricchloride = 40, coeff = "Ferric", output_water = "coag") %>%
    pluck_water("coag", c("toc", "doc", "uv254")))

  coag_doses <- tibble(ferricchloride = seq(0, 100, 10))
  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    cross_join(coag_doses) %>%
    chemdose_toc_chain("raw", "coag", coeff = "Ferric") %>%
    pluck_water(c("coag"), c("doc")))

  water4 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain("raw") %>%
    cross_join(coag_doses) %>%
    rename(Coagulant = ferricchloride) %>%
    chemdose_toc_chain("raw", "coag", coeff = "Ferric", ferricchloride = Coagulant) %>%
    pluck_water(c("coag"), c("doc")))

  expect_equal(water1@toc, water2$coag_toc)
  expect_equal(water1@doc, water2$coag_doc)
  expect_equal(water1@uv254, water2$coag_uv254)
  expect_equal(water1@doc, water3$coag_doc[5])
  expect_equal(water1@doc, water4$coag_doc[5])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_toc_chain output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_chain(input_water = "balanced_water", ferricsulfate = 30, coeff = "Ferric"))

  water2 <- purrr::pluck(water1, "coagulated_water", 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_chain() %>%
    mutate(alum = 10) %>%
    balance_ions_chain() %>%
    chemdose_toc_chain(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check chemdose_toc_chain can use a column or function argument for chemical dose

test_that("chemdose_toc_chain can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    balance_ions_chain() %>%
    chemdose_toc_chain(input_water = "balanced_water", ferricchloride = 40, coeff = "Ferric") %>%
    pluck_water(input_waters = "coagulated_water", c("toc", "doc", "uv254")))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(
      ferricchloride = 40,
      coeff = "Ferric"
    ) %>%
    balance_ions_chain() %>%
    chemdose_toc_chain(input_water = "balanced_water") %>%
    pluck_water(input_waters = "coagulated_water", c("toc", "doc", "uv254")))

  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_chain() %>%
    mutate(ferricchloride = 40) %>%
    balance_ions_chain() %>%
    chemdose_toc_chain(input_water = "balanced_water", coeff = "Ferric") %>%
    pluck_water(input_waters = "coagulated_water", c("toc", "doc", "uv254")))

  expect_equal(water1$coagulated_water_toc, water2$coagulated_water_toc) # test different ways to input chemical
  expect_equal(water1$coagulated_water_doc, water2$coagulated_water_doc)
  expect_equal(water1$coagulated_water_uv254, water2$coagulated_water_uv254)

  # Test that inputting chemical and coeff separately (in column and as an argument)  gives save results
  expect_equal(water1$coagulated_water_toc, water3$coagulated_water_toc)
  expect_equal(water2$coagulated_water_doc, water3$coagulated_water_doc)
  expect_equal(water2$coagulated_water_uv254, water3$coagulated_water_uv254)
})
