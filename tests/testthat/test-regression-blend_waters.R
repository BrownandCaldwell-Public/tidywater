# Regression pins: blend_waters ----
# Golden files pin every slot of the blended water. See tests/testthat/README.md.

test_that("regression: blend_waters two-way blends of dissimilar waters", {
  testthat::skip_on_cran()
  w <- regression_waters()
  ratios <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  pairs <- list(
    typical_low_alk = c("typical", "low_alk"),
    hard_high_toc = c("hard_high_alk", "high_toc"),
    cold_warm = c("cold", "warm"),
    typical_nutrients = c("typical", "nutrients")
  )
  cases <- list()
  for (p in names(pairs)) {
    for (r in ratios) {
      nm <- paste0(p, "_", r)
      cases[[nm]] <- suppressWarnings(blend_waters(w[pairs[[p]]], c(r, 1 - r)))
    }
  }
  expect_golden(cases, "blend_two_way")
})

test_that("regression: blend_waters three-way blends", {
  testthat::skip_on_cran()
  w <- regression_waters()
  expect_golden(
    suppressWarnings(list(
      typical_lowalk_hard_equal = blend_waters(w[c("typical", "low_alk", "hard_high_alk")], c(1 / 3, 1 / 3, 1 / 3)),
      typical_lowalk_hard_skew = blend_waters(w[c("typical", "low_alk", "hard_high_alk")], c(0.6, 0.3, 0.1)),
      cold_warm_nutrients = blend_waters(w[c("cold", "warm", "nutrients")], c(0.2, 0.5, 0.3)),
      hightoc_nutrients_typical = blend_waters(w[c("high_toc", "nutrients", "typical")], c(0.45, 0.45, 0.1))
    )),
    "blend_three_way"
  )
})

test_that("regression: blend_waters with po4/nh3/chlorine/borate/silicate species", {
  testthat::skip_on_cran()
  mk <- function(...) suppressWarnings(define_water(temp = 20, tds = 200, ...))
  po4 <- mk(ph = 7.8, alk = 60, tot_po4 = 2)
  nh3 <- mk(ph = 7.2, alk = 90, tot_nh3 = 1.5)
  cl <- mk(ph = 7.5, alk = 50, free_chlorine = 2)
  bo4 <- mk(ph = 8.5, alk = 100, tot_bo4 = 1, tot_sio4 = 20)
  combo <- mk(ph = 6.8, alk = 40, tot_po4 = 1, tot_nh3 = 0.5, tot_bo4 = 0.2, tot_sio4 = 5, free_chlorine = 1)
  expect_golden(
    suppressWarnings(list(
      po4_nh3 = blend_waters(list(po4, nh3), c(0.5, 0.5)),
      po4_chlorine = blend_waters(list(po4, cl), c(0.3, 0.7)),
      nh3_chlorine = blend_waters(list(nh3, cl), c(0.6, 0.4)),
      borate_silicate_chlorine = blend_waters(list(bo4, cl), c(0.5, 0.5)),
      combo_po4_borate = blend_waters(list(combo, po4, bo4), c(0.4, 0.3, 0.3)),
      single_water_identity = blend_waters(list(combo), 1)
    )),
    "blend_species"
  )
})

test_that("regression: blend_waters with an IS = NA water", {
  testthat::skip_on_cran()
  w <- regression_waters()
  is_na <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 50))
  is_na2 <- suppressWarnings(define_water(ph = 8, temp = 15, alk = 120))
  expect_golden(
    suppressWarnings(list(
      is_na_with_typical = blend_waters(list(is_na, w$typical), c(0.5, 0.5)),
      is_na_with_ideal = blend_waters(list(is_na, w$ideal), c(0.7, 0.3)),
      both_is_na = blend_waters(list(is_na, is_na2), c(0.4, 0.6))
    )),
    "blend_is_na"
  )
})
