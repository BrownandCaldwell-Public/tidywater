# Regression pins: solvect_o3 ----
# Golden files pin every column of the output data frame. See tests/testthat/README.md.

o3_waters <- function() {
  ws <- regression_waters()
  list(
    typical = ws$typical, high_toc = ws$high_toc, warm = ws$warm,
    cold = suppressWarnings(define_water(
      ph = 7, temp = 3, alk = 50, tds = 120, toc = 2, doc = 1.8, uv254 = 0.03, br = 30
    ))
  )
}

test_that("regression: solvect_o3 with kd (first-order decay)", {
  ws <- o3_waters()
  grid <- expand.grid(
    water = c("typical", "cold"), dose = c(0, 1, 3), time = c(1, 2.5, 10),
    kd = c(-0.05, -0.3, -1.5), stringsAsFactors = FALSE
  )
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(solvect_o3(
      ws[[grid$water[i]]],
      time = grid$time[i], dose = grid$dose[i], kd = grid$kd[i], baffle = 0.6
    ))
  })
  names(out) <- with(grid, paste0(water, "_d", dose, "_t", time, "_kd", kd))
  expect_golden(out, "solvect_o3_kd")
})

test_that("regression: solvect_o3 without kd (solveresid_o3 decay curve)", {
  ws <- o3_waters()
  grid <- expand.grid(
    water = names(ws), dose = c(0, 1, 3, 6), time = c(0.5, 3, 7.75, 20),
    stringsAsFactors = FALSE
  )
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(solvect_o3(ws[[grid$water[i]]], time = grid$time[i], dose = grid$dose[i], baffle = 0.5))
  })
  names(out) <- with(grid, paste0(water, "_d", dose, "_t", time, "_nokd"))
  # kd = NA takes the same path as a missing kd.
  out$typical_d3_t7.75_kdNA <- suppressWarnings(solvect_o3(ws$typical, time = 7.75, dose = 3, kd = NA, baffle = 0.5))
  expect_golden(out, "solvect_o3_nokd")
})

test_that("regression: solvect_o3 negative residual cases", {
  ws <- o3_waters()
  # Low doses into high-UV water give negative modeled residuals, so the summed CT is negative.
  expect_golden(
    suppressWarnings(list(
      high_toc_d0.5_t10_nokd = solvect_o3(ws$high_toc, time = 10, dose = 0.5, baffle = 1),
      high_toc_d1_t30_nokd = solvect_o3(ws$high_toc, time = 30, dose = 1, baffle = 1),
      high_toc_resid_d0.5_t10 = solveresid_o3(ws$high_toc, dose = 0.5, time = 10),
      high_toc_resid_d1_t30 = solveresid_o3(ws$high_toc, dose = 1, time = 30),
      temps = lapply(c(t5 = 5, t15 = 15, t25 = 25, t35 = 35), function(tt) {
        w <- suppressWarnings(define_water(
          ph = 7.5, temp = tt, alk = 80, tds = 200, toc = 3, doc = 2.8, uv254 = 0.07, br = 50
        ))
        solvect_o3(w, time = 5, dose = 2, baffle = 0.7)
      })
    )),
    "solvect_o3_negative_temps"
  )
})
