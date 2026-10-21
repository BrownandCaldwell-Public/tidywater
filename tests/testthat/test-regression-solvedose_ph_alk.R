# Regression pins: solvedose_ph and solvedose_alk ----
# Doses come from uniroot (default tol) and are rounded to 0.1 mg/L, so they are pinned
# at the standard tolerance. NA means the target is unreachable with that chemical.
# Each reachable case runs a uniroot over chemdose_ph, so the case list is kept lean.

bases <- c("naoh", "na2co3", "nahco3", "caoh2", "mgoh2")
acids <- c("hcl", "h2so4", "h3po4", "co2")

test_that("regression: solvedose_ph for every chemical", {
  w <- regression_waters()
  cases <- list()
  for (nm in c("typical", "low_alk", "hard_high_alk")) {
    water <- w[[nm]]
    for (ch in bases) {
      # nahco3 can only raise pH toward ~8.3, so give it a modest target.
      target <- if (ch == "nahco3") water@ph + 0.2 else water@ph + 0.8
      cases[[paste(nm, ch, "up", sep = "_")]] <- suppressWarnings(solvedose_ph(water, target_ph = target, chemical = ch))
      cases[[paste(nm, ch, "unreachable_down", sep = "_")]] <- suppressWarnings(solvedose_ph(water, target_ph = water@ph - 0.5, chemical = ch))
    }
    for (ch in acids) {
      cases[[paste(nm, ch, "down_6.6", sep = "_")]] <- suppressWarnings(solvedose_ph(water, target_ph = 6.6, chemical = ch))
      cases[[paste(nm, ch, "unreachable_up", sep = "_")]] <- suppressWarnings(solvedose_ph(water, target_ph = water@ph + 0.5, chemical = ch))
    }
    cases[[paste0(nm, "_co2_below_6.5")]] <- suppressWarnings(solvedose_ph(water, target_ph = 6, chemical = "co2"))
    cases[[paste0(nm, "_naoh_target_na")]] <- suppressWarnings(solvedose_ph(water, target_ph = NA, chemical = "naoh"))
  }
  # Extra targets on one water. (NaHCO3 to pH 11 is not pinned: the pH curve is so flat there
  # that the ~454,000 mg/L answer moves with any solver change; see #532.)
  cases$typical_naoh_ph10 <- suppressWarnings(solvedose_ph(w$typical, target_ph = 10, chemical = "naoh"))
  cases$typical_caoh2_ph9.5 <- suppressWarnings(solvedose_ph(w$typical, target_ph = 9.5, chemical = "caoh2"))
  cases$typical_hcl_ph5 <- suppressWarnings(solvedose_ph(w$typical, target_ph = 5, chemical = "hcl"))
  cases$cold_h2so4_ph6.8 <- suppressWarnings(solvedose_ph(w$cold, target_ph = 6.8, chemical = "h2so4"))
  cases$low_alk_hcl_ph4 <- suppressWarnings(solvedose_ph(w$low_alk, target_ph = 4, chemical = "hcl"))
  expect_golden(cases, "solvedose_ph")
})

test_that("regression: solvedose_alk for every chemical", {
  w <- regression_waters()
  cases <- list()
  for (nm in c("typical", "low_alk")) {
    water <- w[[nm]]
    for (ch in bases) {
      cases[[paste(nm, ch, "up_20", sep = "_")]] <- suppressWarnings(solvedose_alk(water, target_alk = water@alk + 20, chemical = ch))
    }
    # CO2 does not change alkalinity, so solvedose_alk does not accept it (#539).
    for (ch in setdiff(acids, "co2")) {
      cases[[paste(nm, ch, "down_half", sep = "_")]] <- suppressWarnings(solvedose_alk(water, target_alk = water@alk / 2, chemical = ch))
    }
  }
  for (nm in names(w)) {
    water <- w[[nm]]
    for (ch in bases) {
      cases[[paste(nm, ch, "unreachable_down", sep = "_")]] <- suppressWarnings(solvedose_alk(water, target_alk = water@alk - 5, chemical = ch))
    }
    for (ch in setdiff(acids, "co2")) {
      cases[[paste(nm, ch, "unreachable_up", sep = "_")]] <- suppressWarnings(solvedose_alk(water, target_alk = water@alk + 5, chemical = ch))
    }
    cases[[paste0(nm, "_naoh_target_na")]] <- suppressWarnings(solvedose_alk(water, target_alk = NA, chemical = "naoh"))
  }
  cases$hard_high_alk_naoh_up_100 <- suppressWarnings(solvedose_alk(w$hard_high_alk, target_alk = 350, chemical = "naoh"))
  cases$hard_high_alk_h2so4_down_100 <- suppressWarnings(solvedose_alk(w$hard_high_alk, target_alk = 150, chemical = "h2so4"))
  expect_golden(cases, "solvedose_alk")
})
