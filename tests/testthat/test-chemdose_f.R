# chemdose f tests here

test_that("chemdose_f returns raw fluoride or breaks when alum is 0 or missing.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, f = 2))
  fluor <- chemdose_f(water1, alum = 0)

  expect_equal(signif(convert_units(2, "f"), 2), 1.1E-4)
  expect_error(chemdose_f(water1))
})

test_that("chemdose_f returns errors when the coefficients are input incorrectly.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 30))

  expect_error(chemdose_f(water1, alum = 20, coeff = c(2, 3, -3)))
  expect_error(chemdose_f(water1, alum = 20, coeff = 2))
  expect_error(chemdose_f(water1, alum = 20, coeff = c("2", 1, "1", "5")))
})

test_that("chemdose_f fails without ph and f.", {
  water_f <- suppressWarnings(define_water(ph =7.5))
  water_ph <- suppressWarnings(define_water(f = 4))

  expect_error(chemdose_f(water_f, alum =30))
  expect_error(chemdose_f(water_ph, alum = 30))
})

test_that("chemdose_f works.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, f = 3))
  fluor <- chemdose_f(water1, alum = 30)

  expect_equal(signif(fluor@f,2), 1.4E-4)
})
