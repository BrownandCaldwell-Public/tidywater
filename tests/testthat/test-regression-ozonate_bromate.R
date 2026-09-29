# Regression pins: ozonate_bromate ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.
# Siddiqui without `time` errors (#528), so `time` is always passed.
# KNOWN-BUG #536: Siddiqui coefficients give bromate about 100x the other models.

bromate_water <- function(temp, nh3 = 0, ph = 7.5, br = 80, doc = 3, uv254 = 0.07, alk = 90) {
  suppressWarnings(define_water(
    ph = ph, temp = temp, alk = alk, tds = 250, toc = doc + 0.2, doc = doc, uv254 = uv254,
    br = br, tot_nh3 = nh3
  ))
}

# All seven rows of bromatecoeffs: model x ammonia.
bromate_rows <- data.frame(
  model = c("Ozekin", "Ozekin", "Sohn", "Sohn", "Song", "Galey", "Siddiqui"),
  ammonia = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
)

test_that("regression: ozonate_bromate all model rows x temperature", {
  grid <- merge(bromate_rows, data.frame(temp = c(10, 20, 30)))
  out <- lapply(seq_len(nrow(grid)), function(i) {
    w <- bromate_water(temp = grid$temp[i], nh3 = if (grid$ammonia[i]) 0.5 else 0)
    suppressWarnings(ozonate_bromate(w, dose = 2.5, time = 10, model = grid$model[i]))
  })
  names(out) <- with(grid, paste0(model, ifelse(ammonia, "_nh3", "_nonh3"), "_t", temp))
  expect_golden(out, "ozonate_bromate_models_temps")
})

test_that("regression: ozonate_bromate waters, doses and times", {
  ws <- list(
    typical = regression_waters()$typical,
    high_toc = regression_waters()$high_toc,
    warm = regression_waters()$warm,
    high_br_low_ph = bromate_water(temp = 15, ph = 6.5, br = 400, doc = 1.5, uv254 = 0.03, alk = 30),
    high_br_ammonia = bromate_water(temp = 22, ph = 8, br = 250, nh3 = 1.2)
  )
  grid <- expand.grid(
    water = names(ws), row = seq_len(nrow(bromate_rows)), dose = c(1, 4), time = c(2, 15),
    stringsAsFactors = FALSE
  )
  has_nh3 <- vapply(ws, function(w) !is.na(w@nh4) && w@nh4 > 0, logical(1))
  grid <- grid[has_nh3[grid$water] == bromate_rows$ammonia[grid$row], ]
  out <- lapply(seq_len(nrow(grid)), function(i) {
    w <- suppressWarnings(ozonate_bromate(
      ws[[grid$water[i]]],
      dose = grid$dose[i], time = grid$time[i], model = bromate_rows$model[grid$row[i]]
    ))
    w@bro3
  })
  names(out) <- with(grid, paste0(water, "_", bromate_rows$model[row], "_d", dose, "_t", time))
  expect_golden(out, "ozonate_bromate_sweep")
})
