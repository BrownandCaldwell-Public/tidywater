# Regression pins: convert_water, convert_watermg, pluck_water ----
# Golden files pin every non-NA column of the converted data frames. See tests/testthat/README.md.

# Guard for #534 (columns mislabeled from ks onward, trailing columns named NA): any NA name
# is replaced with its column position so the golden would record it as a changed row.
name_na_cols <- function(df) {
  nm <- names(df)
  nm[is.na(nm)] <- paste0("NA_col", which(is.na(nm)))
  names(df) <- nm
  df
}

regression_convert_waters <- function() {
  w <- regression_waters()
  chloraminated <- suppressWarnings(chemdose_chloramine(
    define_water(ph = 7.5, temp = 20, alk = 60, tds = 200, tot_nh3 = 1),
    time = 30, cl2 = 3, nh3 = 0
  ))
  dbps <- suppressWarnings(modify_water(
    w$high_toc,
    slot = c("tthm", "haa5", "chcl3", "chcl2br", "dcaa", "tcaa", "bro3"),
    value = c(55, 40, 30, 15, 18, 12, 5),
    units = c("ug/L", "ug/L", "ug/L", "ug/L", "ug/L", "ug/L", "ug/L")
  ))
  minor <- suppressWarnings(define_water(
    ph = 7.2, temp = 18, alk = 60, tds = 200, br = 75, f = 0.8, fe = 0.3, al = 0.05,
    mn = 40, no3 = 2, mno4 = 0.5, combined_chlorine = 1.5, tot_ch3coo = 2
  ))
  list(
    typical = w$typical,
    ideal = w$ideal,
    nutrients = w$nutrients,
    chloraminated = chloraminated,
    dbps = dbps,
    minor = minor
  )
}

test_that("regression: convert_water on several waters", {
  testthat::skip_on_cran()
  expect_golden(lapply(regression_convert_waters(), function(x) name_na_cols(convert_water(x))), "convert_water")
})

test_that("regression: convert_watermg on several waters", {
  testthat::skip_on_cran()
  expect_golden(
    lapply(regression_convert_waters(), function(x) name_na_cols(suppressWarnings(convert_watermg(x)))),
    "convert_watermg"
  )
})

test_that("regression: pluck_water numeric columns", {
  testthat::skip_on_cran()
  df <- suppressWarnings(define_water_df(water_df[1:4, ]))
  df$blended <- lapply(df$defined, function(x) {
    suppressWarnings(blend_waters(list(x, regression_waters()$typical), c(0.5, 0.5)))
  })
  out <- suppressWarnings(pluck_water(
    df,
    input_waters = c("defined", "blended"),
    parameter = c("ph", "alk", "hco3", "ca", "tot_co3", "is", "toc")
  ))
  plucked <- out[, grepl("^(defined|blended)_", names(out))]
  expect_golden(list(plucked = plucked), "pluck_water")
})
