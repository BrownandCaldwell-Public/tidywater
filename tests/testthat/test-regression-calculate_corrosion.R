# Regression pins: calculate_corrosion ----
# Golden files pin the returned index data frames. See tests/testthat/README.md.
# Note: CCPP always uses calcite solubility and ignores `form` (open question #531); the
# ccpp golden pins that behavior for aragonite/vaterite too.
# CCPP is solved with stats::uniroot at default tolerance, so it gets a looser golden.

corrosion_waters <- function() {
  dw <- function(...) suppressWarnings(define_water(...))
  rw <- regression_waters()
  list(
    typical = rw$typical,
    low_alk = rw$low_alk,
    hard_high_alk = rw$hard_high_alk,
    cold = rw$cold,
    warm_est_ca = rw$warm,
    # ph_s > 9.3 -> bicarbonate-only branch; CCPP hits the big-scan fallback
    phs_gt_9.3 = dw(ph = 9.6, temp = 20, alk = 4, ca = 1.5, mg = 0.5, na = 5, cl = 5, so4 = 3, tds = 30),
    # strongly aggressive -> big-scan fallback
    aggressive = dw(ph = 5.8, temp = 10, alk = 8, ca = 4, mg = 1, na = 5, cl = 10, so4 = 8, tds = 50),
    very_aggressive = dw(ph = 4.5, temp = 5, alk = 1, ca = 1, na = 5, cl = 10, so4 = 8, tds = 50),
    # strongly aggressive beyond -500 -> extendInt "upX" fallback
    extreme_aggressive = dw(ph = 4, temp = 5, alk = 0.5, ca = 0.5, na = 5, cl = 10, so4 = 8, tds = 50),
    # strongly supersaturated -> big-scan fallback
    supersaturated = dw(ph = 9.2, temp = 25, alk = 350, ca = 250, mg = 30, na = 80, cl = 120, so4 = 150, tds = 1500),
    very_supersaturated = dw(ph = 10.5, temp = 30, alk = 500, ca = 400, na = 50, cl = 100, so4 = 100, tds = 2500),
    # supersaturated beyond 500 -> extendInt "downX" fallback
    extreme_supersaturated = dw(ph = 9.5, temp = 25, alk = 1000, ca = 400, na = 50, cl = 100, so4 = 100, tds = 3000)
  )
}

# IS = NA water (no tds/cond, no cl/so4); only calcium-based indices apply.
corrosion_is_na <- function() {
  suppressWarnings(define_water(ph = 7.8, temp = 15, alk = 90, tot_hard = 120))
}

non_ccpp <- c("aggressive", "ryznar", "langelier", "larsonskold", "csmr")

test_that("regression: calculate_corrosion non-CCPP indices x form", {
  waters <- corrosion_waters()
  cases <- list()
  for (w in names(waters)) {
    for (form in c("calcite", "aragonite", "vaterite")) {
      cases[[paste0(w, "_", form)]] <- suppressWarnings(calculate_corrosion(waters[[w]], index = non_ccpp, form = form))
    }
  }
  for (form in c("calcite", "aragonite", "vaterite")) {
    cases[[paste0("is_na_", form)]] <- suppressWarnings(
      calculate_corrosion(corrosion_is_na(), index = c("aggressive", "ryznar", "langelier"), form = form)
    )
  }
  expect_golden(cases, "calculate_corrosion_indices")
})

test_that("regression: calculate_corrosion CCPP", {
  # CCPP is slow (each root evaluation calls chemdose_ph), so pin a subset that covers each
  # uniroot fallback stage: restricted interval, big scan (aggressive and supersaturated),
  # extendInt "downX" (extreme_supersaturated) and extendInt "upX" (extreme_aggressive).
  waters <- corrosion_waters()[c(
    "typical", "low_alk", "hard_high_alk", "aggressive", "supersaturated",
    "extreme_aggressive", "extreme_supersaturated"
  )]
  cases <- lapply(waters, function(w) suppressWarnings(calculate_corrosion(w, index = "ccpp")))
  names(cases) <- paste0(names(waters), "_calcite")
  # form is ignored by CCPP (#531)
  for (form in c("aragonite", "vaterite")) {
    cases[[paste0("typical_", form)]] <- suppressWarnings(calculate_corrosion(waters$typical, index = "ccpp", form = form))
  }
  cases$is_na_calcite <- suppressWarnings(calculate_corrosion(corrosion_is_na(), index = "ccpp"))
  expect_golden(cases, "calculate_corrosion_ccpp", rel_tol = 1e-4, abs_tol = 1e-3)
})
