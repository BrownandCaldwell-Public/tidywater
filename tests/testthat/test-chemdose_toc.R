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
  water2 <- chemdose_toc(water1, alum = 30)
  water3 <- chemdose_toc(water1, ferricchloride = 50, coeff = "Ferric")
  water4 <- chemdose_toc(water1,
    ferricchloride = 50,
    coeff = data.frame(x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068)
  )

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

test_that("chemdose_toc_df is a data frame and has correct classes in output columns", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    define_water_df() %>%
    chemdose_toc_df(
      alum = 20, output_water = "coag",
      pluck_cols = T, water_prefix = FALSE
    )


  expect_true(is.data.frame(water1))
  expect_equal(colnames(water1), c("defined", "alum", "coag", "toc", "doc", "uv254"))
  expect_s4_class(water1$coag[[1]], "water")
  expect_true(is.numeric(water1$toc[1]))
})



test_that("chemdose_toc_df outputs are the same as base function, chemdose_toc", {
  testthat::skip_on_cran()
  water0 <- define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )

  water1 <- chemdose_toc(water0, ferricchloride = 40, coeff = "Ferric")

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(ferricchloride = 40, coeff = "Ferric", output_water = "coag") %>%
    pluck_water("coag", c("toc", "doc", "uv254"))

  coag_doses <- tibble(ferricchloride = seq(10, 100, 10))
  water3 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_df("raw") %>%
    cross_join(coag_doses) %>%
    chemdose_toc_df("raw", "coag", coeff = "Ferric") %>%
    pluck_water(c("coag"), c("doc")))

  water4 <- water_df %>%
    slice(1) %>%
    define_water_df("raw") %>%
    cross_join(coag_doses) %>%
    rename(Coagulant = ferricchloride) %>%
    chemdose_toc_df("raw", "coag", coeff = "Ferric", ferricchloride = Coagulant) %>%
    pluck_water(c("coag"), c("doc"))

  expect_equal(water1@toc, water2$coag_toc)
  expect_equal(water1@doc, water2$coag_doc)
  expect_equal(water1@uv254, water2$coag_uv254)
  expect_equal(water1@doc, water3$coag_doc[4])
  expect_equal(water1@doc, water4$coag_doc[4])
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_toc_df output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(input_water = "defined", ferricsulfate = 30, coeff = "Ferric"))

  water2 <- purrr::pluck(water1, "coagulated", 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    mutate(alum = 10) %>%
    chemdose_toc_df(output_water = "diff_name"))

  expect_s4_class(water2, "water") # check class
  expect_true(exists("diff_name", water3)) # check if output_water arg works
})

# Check chemdose_toc_df can use a column or function argument for chemical dose

test_that("chemdose_toc_df can use a column or function argument for chemical dose", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(
      input_water = "defined", ferricchloride = 40, coeff = "Ferric",
      pluck_cols = TRUE
    )

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    mutate(
      ferricchloride = 40,
      coeff = "Ferric"
    ) %>%
    chemdose_toc_df(pluck_cols = T)

  # Test that pluck_cols does the same thing as pluck_water function
  water3 <- water_df %>%
    slice(1) %>%
    define_water_df("raw") %>%
    mutate(ferricchloride = 40) %>%
    chemdose_toc_df(input_water = "raw", output_water = "final", coeff = "Ferric") %>%
    pluck_water(input_waters = "final", c("toc", "doc", "uv254"))

  expect_equal(water1$coagulated_toc, water2$coagulated_toc) # test different ways to input chemical
  expect_equal(water1$coagulated_doc, water2$coagulated_doc)
  expect_equal(water1$coagulated_uv254, water2$coagulated_uv254)

  # Test that inputting chemical and coeff separately (in column and as an argument)  gives same results
  expect_equal(water1$coagulated_toc, water3$final_toc)
  expect_equal(water1$coagulated_doc, water3$final_doc)
  expect_equal(water1$coagulated_uv254, water3$final_uv254)
})

test_that("chemdose_toc_df works when water_prefix is false", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(ferricsulfate = 23, pluck_cols = TRUE))

  water2 <- suppressWarnings(water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_toc_df(ferricsulfate = 23, water_prefix = FALSE))

  expect_equal(water1$coagulated_water_toc, water2$toc)
  expect_equal(water1$coagulated_water_uv254, water2$uv254)
})
