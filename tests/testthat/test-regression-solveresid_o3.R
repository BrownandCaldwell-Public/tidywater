# Regression pins: solveresid_o3 ----
# Closed-form model, default rel_tol = 1e-8. See tests/testthat/README.md.

test_that("regression: solveresid_o3 waters x doses x times", {
  testthat::skip_on_cran()
  ws <- regression_waters()
  ws <- list(
    typical = ws$typical, high_toc = ws$high_toc, warm = ws$warm,
    cold_low_uv = suppressWarnings(define_water(
      ph = 6.8, temp = 4, alk = 30, tds = 100, toc = 1.2, doc = 1, uv254 = 0.015, br = 20
    ))
  )
  grid <- expand.grid(
    water = names(ws), dose = c(0, 0.5, 1, 2.5, 5, 10), time = c(0.5, 1, 2.75, 10, 30, 120),
    stringsAsFactors = FALSE
  )
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(solveresid_o3(ws[[grid$water[i]]], dose = grid$dose[i], time = grid$time[i]))
  })
  names(out) <- with(grid, paste0(water, "_d", dose, "_t", time))
  expect_golden(out, "solveresid_o3_grid")
})

test_that("regression: solveresid_o3 temperature and pH sweep", {
  testthat::skip_on_cran()
  grid <- expand.grid(temp = c(5, 15, 25, 35), ph = c(6, 7, 8, 9))
  out <- lapply(seq_len(nrow(grid)), function(i) {
    w <- suppressWarnings(define_water(
      ph = grid$ph[i], temp = grid$temp[i], alk = 100, tds = 200, toc = 3, doc = 2.8, uv254 = 0.06, br = 60
    ))
    suppressWarnings(solveresid_o3(w, dose = 3, time = 8))
  })
  names(out) <- with(grid, paste0("t", temp, "_ph", ph))
  expect_golden(out, "solveresid_o3_temp_ph")
})
