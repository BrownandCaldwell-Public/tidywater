# Regression pins: define_water ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.

test_that("regression: standard scenario waters", {
  testthat::skip_on_cran()
  expect_golden(regression_waters(), "scenario_waters")
})

test_that("regression: define_water hardness estimation paths", {
  testthat::skip_on_cran()
  base <- function(...) {
    suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 80, na = 20, cl = 30, so4 = 40, ...))
  }
  expect_golden(
    list(
      hard_ca_mg = base(tot_hard = 150, ca = 40, mg = 12),
      hard_mg_only = base(tot_hard = 150, mg = 12),
      hard_ca_only = base(tot_hard = 150, ca = 40),
      hard_only = base(tot_hard = 150),
      ca_only = base(ca = 40),
      ca_mg_only = base(ca = 40, mg = 12),
      none = base()
    ),
    "define_water_hardness"
  )
})

test_that("regression: define_water organics and ionic strength paths", {
  testthat::skip_on_cran()
  expect_golden(
    suppressWarnings(list(
      toc_only = define_water(ph = 7, alk = 50, tds = 200, toc = 4),
      doc_only = define_water(ph = 7, alk = 50, tds = 200, doc = 4),
      uv_only = define_water(ph = 7, alk = 50, tds = 200, uv254 = 0.1),
      is_from_tds = define_water(ph = 7, alk = 50, tds = 300),
      is_from_cond = define_water(ph = 7, alk = 50, cond = 450),
      is_from_ions = define_water(ph = 7, alk = 50, ca = 20, mg = 5, na = 15, k = 3, cl = 20, so4 = 25),
      is_na = define_water(ph = 7, alk = 50),
      zero_alk = define_water(ph = 5, alk = 0, tds = 100)
    )),
    "define_water_organics_is"
  )
})

test_that("regression: define_water temperature and pH sweep", {
  testthat::skip_on_cran()
  grid <- expand.grid(temp = c(1, 10, 25, 35), ph = c(5.5, 7, 8.5, 10))
  waters <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(define_water(
      ph = grid$ph[i], temp = grid$temp[i], alk = 100, tds = 250,
      tot_po4 = 1, tot_nh3 = 1, tot_bo4 = 0.5, tot_sio4 = 10, tot_ch3coo = 2, tot_citric = 1,
      free_chlorine = 2
    ))
  })
  names(waters) <- paste0("t", grid$temp, "_ph", grid$ph)
  expect_golden(waters, "define_water_temp_ph")
})

test_that("regression: define_water minor ions and unit conversions", {
  testthat::skip_on_cran()
  expect_golden(
    suppressWarnings(list(
      minor = define_water(
        ph = 7.2, temp = 18, alk = 60, tds = 200, br = 75, f = 0.8, fe = 0.3, al = 0.05,
        mn = 40, no3 = 2, mno4 = 0.5, combined_chlorine = 1.5
      )
    )),
    "define_water_minor_ions"
  )
})

test_that("regression: define_water_df matches water_df dataset", {
  testthat::skip_on_cran()
  out <- suppressWarnings(define_water_df(water_df))
  expect_golden(list(water_df = out), "define_water_df")
})
