# Regression pins: modify_water ----
# Unit sweeps pin just the modified slot; whole-water cases pin every slot to show
# nothing else changes. See tests/testthat/README.md.

test_that("regression: modify_water ion slots across convert_units units", {
  testthat::skip_on_cran()
  base <- regression_waters()$typical
  ions <- c("na", "ca", "mg", "k", "cl", "so4", "no3", "br", "bro3", "f", "fe", "al", "mn")
  units <- c("mg/L", "ug/L", "M", "mM", "meq/L")
  cases <- list()
  for (ion in ions) {
    ion_units <- units
    if (ion %in% c("ca", "mg")) ion_units <- c(ion_units, "mg/L CaCO3")
    if (ion == "no3") ion_units <- c(ion_units, "mg/L N")
    # Any combination that errors is skipped (none currently do); a change shows up as added/removed rows.
    for (u in ion_units) {
      out <- tryCatch(
        suppressWarnings(modify_water(base, slot = ion, value = 2.5, units = u)),
        error = function(e) NULL
      )
      if (!is.null(out)) cases[[paste0(ion, "_", u)]] <- methods::slot(out, ion)
    }
  }
  expect_golden(cases, "modify_ion_units")
})

test_that("regression: modify_water organics, uv254, and DBP unit branches", {
  testthat::skip_on_cran()
  base <- regression_waters()$typical
  slot_of <- function(slot, value, units) {
    methods::slot(suppressWarnings(modify_water(base, slot = slot, value = value, units = units)), slot)
  }
  cases <- list()
  for (s in c("toc", "doc", "bdoc")) {
    cases[[paste0(s, "_mg/L")]] <- slot_of(s, 4.2, "mg/L")
    cases[[paste0(s, "_ug/L")]] <- slot_of(s, 4200, "ug/L")
  }
  cases[["uv254_cm-1"]] <- slot_of("uv254", 0.12, "cm-1")
  cases[["uv254_m-1"]] <- slot_of("uv254", 12, "m-1")
  dbps <- c(
    "chcl3", "chcl2br", "chbr2cl", "chbr3", "tthm",
    "mcaa", "dcaa", "tcaa", "mbaa", "dbaa", "haa5",
    "bcaa", "cdbaa", "dcbaa"
  )
  # "baa" is accepted by modify_water but is not a water slot (errors), so it is not pinned.
  for (d in dbps) {
    cases[[paste0(d, "_ug/L")]] <- slot_of(d, 15, "ug/L")
    cases[[paste0(d, "_mg/L")]] <- slot_of(d, 0.015, "mg/L")
  }
  expect_golden(cases, "modify_organics_dbps")
})

test_that("regression: modify_water whole-water outputs", {
  testthat::skip_on_cran()
  w <- regression_waters()
  expect_golden(
    suppressWarnings(list(
      typical_multi_ions = modify_water(
        w$typical,
        slot = c("ca", "mg", "na", "cl", "br"), value = c(40, 50, 25, 1.2, 80),
        units = c("mg/L", "mg/L CaCO3", "mg/L", "meq/L", "ug/L")
      ),
      nutrients_organics_uv = modify_water(
        w$nutrients,
        slot = c("toc", "doc", "bdoc", "uv254"), value = c(3.5, 3.1, 0.8, 9),
        units = c("mg/L", "mg/L", "mg/L", "m-1")
      ),
      high_toc_dbps = modify_water(
        w$high_toc,
        slot = c("tthm", "haa5", "chcl3", "dcaa"), value = c(60, 45, 30, 20),
        units = c("ug/L", "ug/L", "ug/L", "ug/L")
      ),
      ideal_mixed = modify_water(
        w$ideal,
        slot = c("toc", "tthm", "fe", "uv254"), value = c(2500, 0.05, 0.3, 0.1),
        units = c("ug/L", "mg/L", "mg/L", "cm-1")
      )
    )),
    "modify_whole_water"
  )
})
