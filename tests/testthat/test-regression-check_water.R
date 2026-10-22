# Regression pins: check_water ----
# Golden files pin the returned message lists. See tests/testthat/README.md.

# check_water can return an empty list; record the count so every case has at least one row.
# Embedded newlines are escaped so each message stays on one golden line.
pin_messages <- function(x) {
  msgs <- if (length(x) > 0) gsub("\n", "\\n", unlist(x), fixed = TRUE) else "<none>"
  list(n_messages = length(x), messages = msgs)
}

# Sum of the ions check_water uses for its TDS comparison (mg/L)
ion_sum_mgl <- function(w) {
  sum(
    convert_units(w@na, "na", "M", "mg/L"), convert_units(w@cl, "cl", "M", "mg/L"),
    convert_units(w@ca, "ca", "M", "mg/L"), convert_units(w@mg, "mg", "M", "mg/L"),
    convert_units(w@k, "k", "M", "mg/L"), convert_units(w@so4, "so4", "M", "mg/L"),
    na.rm = TRUE
  )
}

# KNOWN-BUG #541: TDS check leaves out bicarbonate. (The "No issues found" part was fixed in #553.)
test_that("regression: check_water message lists", {
  testthat::skip_on_cran()
  dw <- function(...) suppressWarnings(define_water(...))
  clean_args <- list(ph = 7.5, temp = 20, alk = 100, ca = 40, mg = 10, na = 20, k = 3, cl = 30, so4 = 40)
  clean_tds <- ion_sum_mgl(do.call(dw, clean_args))
  clean <- do.call(dw, c(clean_args, list(
    tds = clean_tds, tot_hard = calculate_hardness(40, 10), doc = 3, uv254 = 0.08, toc = 3.2, br = 40
  )))

  waters <- list(
    no_issues = clean,
    hardness_mismatch = dw(ph = 7.5, temp = 20, alk = 100, tot_hard = 300, ca = 40, mg = 10, na = 20, cl = 30, so4 = 40),
    hardness_missing_mg = dw(ph = 7.5, temp = 20, alk = 100, ca = 40, na = 20, cl = 30, so4 = 40),
    ion_imbalance = dw(ph = 7.5, temp = 20, alk = 50, ca = 10, na = 300, cl = 5, so4 = 5, tds = 800),
    tds_mismatch = dw(ph = 7.5, temp = 20, alk = 100, ca = 40, mg = 10, na = 20, k = 3, cl = 30, so4 = 40, tds = 1500),
    chloramine = do.call(dw, c(clean_args, list(tds = clean_tds, tot_nh3 = 1, free_chlorine = 2))),
    chloramine_combined = do.call(dw, c(clean_args, list(tds = clean_tds, tot_nh3 = 0.5, combined_chlorine = 1))),
    missing_is = dw(ph = 7, alk = 70),
    minimal_toc = dw(ph = 7, temp = 20, alk = 50, toc = 3, uv254 = 0.08),
    no_ph_models = dw(temp = 20, alk = 50, doc = 3),
    scenario_typical = regression_waters()$typical,
    scenario_nutrients = regression_waters()$nutrients
  )
  cases <- lapply(waters, function(w) pin_messages(suppressWarnings(check_water(w))))
  skipped <- lapply(waters[c("no_issues", "tds_mismatch", "missing_is", "chloramine")], function(w) {
    pin_messages(suppressWarnings(check_water(w, skip_models = TRUE)))
  })
  names(skipped) <- paste0(names(skipped), "_skip_models")
  expect_golden(c(cases, skipped), "check_water_messages")
})
