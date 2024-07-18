# Chemdose TOC ----

test_that("chemdose_toc returns the same water when coagulant dose is 0.", {
  water1 <- suppressWarnings(define_water(ph = 7, doc = 3.5, uv254 = 0.1))
  toc_rem1 <- suppressWarnings(chemdose_toc(water1))
  toc_rem1@treatment <- "defined" # add this to prevent error when comparing numbers

  water2 <- suppressWarnings(define_water(ph = 7, toc = 3.5, doc = 3.2, uv254 = 0.1))
  toc_rem2 <- suppressWarnings(chemdose_toc(water2))
  toc_rem2@treatment <- "defined" # add this to prevent error when comparing numbers

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
  water4 <- suppressWarnings(chemdose_toc(water1, ferricchloride = 50,
    coeff = c("x1" = 280, "x2" = -73.9, "x3" = 4.96, "k1" = -0.028, "k2" = 0.23, "b" = 0.068)))

  # Used to generate expected outputs cross check with edwards97 package
  # data = data.frame(DOC = 3.5, dose = convert_units(50, "ferricchloride", endunit = "mM"), pH = 7, UV254 = .1)
  # coagulate(data, coefs = edwards_coefs("Fe"))

  expect_equal(round(water2@doc, 1), 2.8)
  expect_equal(round(water3@doc, 1), 2.2)
  expect_equal(round(water4@doc, 1), 2.2)

})
