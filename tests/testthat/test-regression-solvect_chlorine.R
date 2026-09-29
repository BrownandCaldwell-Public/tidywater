# Regression pins: solvect_chlorine ----
# Golden files pin every column of the output data frame. See tests/testthat/README.md.

ct_water <- function(ph, temp, free_chlorine = 0) {
  suppressWarnings(define_water(ph = ph, temp = temp, alk = 60, tds = 150, free_chlorine = free_chlorine))
}

test_that("regression: solvect_chlorine temperature and pH sweep (Giardia branches)", {
  # temp < 12.5 and >= 12.5 use different Giardia CT equations; 12.5 itself is the edge.
  grid <- expand.grid(
    temp = c(0.5, 5, 10, 12.4, 12.5, 15, 20, 25, 30),
    ph = c(5.5, 6, 7, 8, 9, 9.4, 9.5, 9.6, 10, 10.5)
  )
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(solvect_chlorine(ct_water(grid$ph[i], grid$temp[i]), time = 30, residual = 1.2, baffle = 0.5))
  })
  names(out) <- with(grid, paste0("t", temp, "_ph", ph))
  expect_golden(out, "solvect_chlorine_temp_ph")
})

test_that("regression: solvect_chlorine virus table bin edges", {
  # residual = baffle = 1 so ct_actual == time; CTs sit on and around each table break.
  cts_69 <- list(
    `0.5` = c(5.9, 6, 9, 12, 20),
    `5` = c(3.9, 4, 6, 8, 20),
    `10` = c(2.9, 3, 4, 6, 20),
    `15` = c(1.9, 2, 3, 4, 20),
    `20` = c(0.9, 1, 2, 3, 20),
    `25` = c(0.9, 1, 2, 20)
  )
  cts_10 <- list(
    `0.5` = c(44, 45, 66, 90, 200),
    `5` = c(29, 30, 44, 60, 200),
    `10` = c(21, 22, 33, 45, 200),
    `15` = c(14, 15, 22, 30, 200),
    `20` = c(10, 11, 16, 22, 200),
    `25` = c(6, 7, 11, 15, 200)
  )
  mk <- function(cts, ph) {
    res <- list()
    for (tt in names(cts)) {
      for (ct in cts[[tt]]) {
        res[[paste0("ph", ph, "_t", tt, "_ct", ct)]] <- suppressWarnings(
          solvect_chlorine(ct_water(ph, as.numeric(tt)), time = ct, residual = 1, baffle = 1)
        )
      }
    }
    res
  }
  # Temperatures between table rows snap to the nearest row.
  snapped <- list(
    ph7_t7.6_ct4 = suppressWarnings(solvect_chlorine(ct_water(7, 7.6), time = 4, residual = 1, baffle = 1)),
    ph7_t2.7_ct6 = suppressWarnings(solvect_chlorine(ct_water(7, 2.7), time = 6, residual = 1, baffle = 1)),
    ph10_t35_ct10 = suppressWarnings(solvect_chlorine(ct_water(10, 35), time = 10, residual = 1, baffle = 1))
  )
  expect_golden(c(mk(cts_69, 7), mk(cts_10, 10), snapped), "solvect_chlorine_virus_bins")
})

test_that("regression: solvect_chlorine free_cl_slot options", {
  w <- ct_water(7.5, 15, free_chlorine = 1.5)
  expect_golden(
    suppressWarnings(list(
      residual_only_default = solvect_chlorine(w, time = 20, residual = 1, baffle = 0.7),
      residual_only_explicit = solvect_chlorine(w, time = 20, residual = 1, baffle = 0.7, free_cl_slot = "residual_only"),
      # KNOWN-BUG #523
      slot_only_knownbug523 = solvect_chlorine(w, time = 20, residual = 1, baffle = 0.7, free_cl_slot = "slot_only"),
      # KNOWN-BUG #523
      sum_with_residual_knownbug523 = solvect_chlorine(
        w,
        time = 20, residual = 1, baffle = 0.7, free_cl_slot = "sum_with_residual"
      ),
      # KNOWN-BUG #523
      slot_only_cold_ph10_knownbug523 = solvect_chlorine(
        ct_water(10, 5, free_chlorine = 2),
        time = 60, residual = 1, baffle = 1, free_cl_slot = "slot_only"
      )
    )),
    "solvect_chlorine_slot"
  )
})
