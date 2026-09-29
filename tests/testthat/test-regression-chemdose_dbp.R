# Regression pins: chemdose_dbp ----
# Golden files pin every slot of the output water (all 11 DBP slots plus the rest).
# See tests/testthat/README.md.

dbp_waters <- function() {
  w <- regression_waters()
  suppressWarnings(list(
    typical = w$typical, # br 50, toc 3.5, doc 3.2, uv 0.1, pH 7.5, 20 C
    high_toc = w$high_toc, # br 120, toc 8, doc 7.5, uv 0.35, pH 6.5, 25 C
    warm = w$warm, # br 80, toc 3, doc 2.8, uv 0.07, pH 8.2, 30 C
    gac_like = define_water(
      ph = 8.8, temp = 12, alk = 90, tds = 200, toc = 1.5, doc = 1.2, uv254 = 0.025, br = 300
    )
  ))
}

dbp_grid <- function(water, cl2, time) {
  grid <- expand.grid(
    treatment = c("raw", "coag", "gac"),
    cl_type = c("chlorine", "chloramine"),
    location = c("plant", "ds"),
    correction = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(chemdose_dbp(
      water,
      cl2 = cl2, time = time,
      treatment = grid$treatment[i], cl_type = grid$cl_type[i],
      location = grid$location[i], correction = grid$correction[i]
    ))
  })
  names(out) <- with(grid, paste(treatment, cl_type, location, ifelse(correction, "corr", "nocorr"), sep = "_"))
  out
}

test_that("regression: chemdose_dbp full option grid, typical water", {
  expect_golden(dbp_grid(dbp_waters()$typical, cl2 = 4, time = 24), "chemdose_dbp_grid_typical")
})

test_that("regression: chemdose_dbp full option grid, high TOC / high bromide water", {
  expect_golden(dbp_grid(dbp_waters()$high_toc, cl2 = 8, time = 48), "chemdose_dbp_grid_high_toc")
})

test_that("regression: chemdose_dbp full option grid, warm high pH water", {
  expect_golden(dbp_grid(dbp_waters()$warm, cl2 = 2.5, time = 12), "chemdose_dbp_grid_warm")
})

test_that("regression: chemdose_dbp full option grid, low DOC high bromide (GAC-range) water", {
  expect_golden(dbp_grid(dbp_waters()$gac_like, cl2 = 1.5, time = 72), "chemdose_dbp_grid_gac_like")
})

test_that("regression: chemdose_dbp dose and time sweep", {
  ws <- dbp_waters()
  grid <- expand.grid(
    water = c("typical", "high_toc"), treatment = c("raw", "coag", "gac"),
    cl2 = c(1, 3, 10), time = c(2, 8, 168),
    stringsAsFactors = FALSE
  )
  out <- lapply(seq_len(nrow(grid)), function(i) {
    w <- suppressWarnings(chemdose_dbp(
      ws[[grid$water[i]]],
      cl2 = grid$cl2[i], time = grid$time[i], treatment = grid$treatment[i], cl_type = "chlorine"
    ))
    # Only DBP slots here; the full water is pinned in the grid tests above.
    c(
      tthm = w@tthm, chcl3 = w@chcl3, chcl2br = w@chcl2br, chbr2cl = w@chbr2cl, chbr3 = w@chbr3,
      haa5 = w@haa5, mcaa = w@mcaa, dcaa = w@dcaa, tcaa = w@tcaa, mbaa = w@mbaa, dbaa = w@dbaa
    )
  })
  names(out) <- with(grid, paste0(water, "_", treatment, "_cl", cl2, "_t", time))
  expect_golden(out, "chemdose_dbp_dose_time")
})

test_that("regression: chemdose_dbp default cl_type and custom single-row coeff", {
  ws <- dbp_waters()
  # Default cl_type is the misspelled "chorine", which behaves as chlorine.
  default_cl <- suppressWarnings(chemdose_dbp(ws$typical, cl2 = 4, time = 24))
  coeff_raw <- data.frame(ID = "tthm", A = 0.05, a = 1.1, b = 0.15, c = 0.07, d = 0.6, e = 1.6, f = 0.26, ph_const = NA_real_)
  coeff_coag <- data.frame(ID = "chcl3", A = 300, a = 0.3, b = 0.4, c = -0.2, d = 1.1, e = 1.03, f = 0.3, ph_const = 7.5)
  coeff_gac <- data.frame(ID = "haa5", A = 20, a = 0.5, b = 0.3, c = -0.05, d = 0.9, e = 1.02, f = 0.2, ph_const = 8)
  expect_golden(
    suppressWarnings(list(
      default_cl_type = default_cl,
      coeff_raw_tthm = chemdose_dbp(ws$typical, cl2 = 4, time = 24, treatment = "raw", coeff = coeff_raw),
      coeff_coag_chcl3 = chemdose_dbp(ws$typical, cl2 = 4, time = 24, treatment = "coag", coeff = coeff_coag),
      coeff_gac_haa5_chloramine = chemdose_dbp(
        ws$gac_like,
        cl2 = 1.5, time = 72, treatment = "gac", cl_type = "chloramine", coeff = coeff_gac
      )
    )),
    "chemdose_dbp_default_coeff"
  )
})
