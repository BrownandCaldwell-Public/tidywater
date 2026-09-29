# Regression pins: dissolve_cu ----
# Golden files pin the returned dissolved copper (mg/L). See tests/testthat/README.md.
# KNOWN-BUG #508: the model uses tot_po4 in mol/L (water@tot_po4) instead of mg/L, so every
# pinned value reflects the buggy phosphate term. All cases carry the _knownbug508 suffix.

test_that("regression: dissolve_cu pH x DIC x PO4 sweep", {
  # KNOWN-BUG #508
  grid <- expand.grid(ph = c(6.5, 7, 7.5, 8, 8.5, 9.5), alk = c(10, 50, 150, 300), po4 = c(0, 0.5, 1, 3))
  out <- lapply(seq_len(nrow(grid)), function(i) {
    w <- suppressWarnings(define_water(
      ph = grid$ph[i], temp = 20, alk = grid$alk[i], tot_hard = 100, na = 20, cl = 20, so4 = 20,
      tds = 250, tot_po4 = grid$po4[i]
    ))
    suppressWarnings(dissolve_cu(w))
  })
  names(out) <- paste0("ph", grid$ph, "_alk", grid$alk, "_po4", grid$po4, "_knownbug508")
  expect_golden(out, "dissolve_cu_sweep")
})

test_that("regression: dissolve_cu scenario waters and dissolve_cu_df", {
  # KNOWN-BUG #508
  waters <- regression_waters()
  out <- lapply(waters, function(w) suppressWarnings(dissolve_cu(w)))
  names(out) <- paste0(names(waters), "_knownbug508")
  df <- suppressWarnings(dissolve_cu_df(define_water_df(water_df)))
  out$water_df_knownbug508 <- df[, !vapply(df, is.list, logical(1)), drop = FALSE]
  expect_golden(out, "dissolve_cu_scenarios")
})
