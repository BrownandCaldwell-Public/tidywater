# Regression pins: chemdose_chloramine ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.
#
# Tolerance choice: chemdose_chloramine integrates an ODE with deSolve (lsoda,
# atol = rtol = 1e-12), so results can drift slightly across platforms / deSolve
# versions. CI showed relative differences up to ~6e-6 between Windows, macOS and Ubuntu,
# so we pin the whole water (every slot, including tot_nh3 and combined_chlorine) with
# rel_tol = 1e-4. Chlorine species are stored in mol/L
# (~1e-5 M for a few mg/L), and species that decay to ~0 (clipped at 0 or left at
# ODE noise ~1e-15 M) would fail a pure relative test, so abs_tol = 1e-12 mol/L
# (~7e-8 mg/L Cl2, far below anything meaningful) absorbs that noise.
chloramine_tol <- list(rel_tol = 1e-4, abs_tol = 1e-12)

expect_golden_chloramine <- function(cases, name) {
  expect_golden(cases, name, rel_tol = chloramine_tol$rel_tol, abs_tol = chloramine_tol$abs_tol)
}

chloramine_water <- function(ph = 7.5, temp = 20, alk = 80) {
  suppressWarnings(define_water(ph = ph, temp = temp, alk = alk, tds = 200))
}

test_that("regression: chemdose_chloramine breakpoint regimes (Cl2:N mass ratio)", {
  w <- chloramine_water()
  ratios <- c(3, 5, 7.6, 10, 15)
  out <- lapply(ratios, function(r) suppressWarnings(chemdose_chloramine(w, time = 30, cl2 = r, nh3 = 1)))
  names(out) <- paste0("ratio_", ratios)
  expect_golden_chloramine(out, "chemdose_chloramine_ratios")
})

test_that("regression: chemdose_chloramine pH and temperature sweep", {
  grid <- expand.grid(ph = c(6, 7, 8, 9), temp = c(5, 20, 30), ratio = c(4, 10))
  out <- lapply(seq_len(nrow(grid)), function(i) {
    w <- chloramine_water(ph = grid$ph[i], temp = grid$temp[i])
    wo <- suppressWarnings(chemdose_chloramine(w, time = 20, cl2 = grid$ratio[i] * 0.8, nh3 = 0.8))
    # Chlorine/ammonia slots only; the full water is pinned in the other tests.
    c(
      free_chlorine = wo@free_chlorine, nh2cl = wo@nh2cl, nhcl2 = wo@nhcl2, ncl3 = wo@ncl3,
      combined_chlorine = wo@combined_chlorine, tot_nh3 = wo@tot_nh3
    )
  })
  names(out) <- with(grid, paste0("ph", ph, "_t", temp, "_ratio", ratio))
  expect_golden_chloramine(out, "chemdose_chloramine_ph_temp")
})

test_that("regression: chemdose_chloramine alkalinity and time", {
  out <- suppressWarnings(list(
    alk_10 = chemdose_chloramine(chloramine_water(alk = 10), time = 60, cl2 = 5, nh3 = 1),
    alk_250 = chemdose_chloramine(chloramine_water(alk = 250), time = 60, cl2 = 5, nh3 = 1),
    alk_250_breakpoint = chemdose_chloramine(chloramine_water(alk = 250), time = 60, cl2 = 12, nh3 = 1),
    time_1 = chemdose_chloramine(chloramine_water(), time = 1, cl2 = 8, nh3 = 1),
    time_5 = chemdose_chloramine(chloramine_water(), time = 5, cl2 = 8, nh3 = 1),
    # Non-integer minutes: the ODE output grid is seq(0, time * 60, by = 60), so 12.5 min
    # ends at 12 min. Pinned as-is.
    time_12.5 = chemdose_chloramine(chloramine_water(), time = 12.5, cl2 = 8, nh3 = 1),
    time_12 = chemdose_chloramine(chloramine_water(), time = 12, cl2 = 8, nh3 = 1),
    time_240 = chemdose_chloramine(chloramine_water(), time = 240, cl2 = 8, nh3 = 1),
    cl2_only = chemdose_chloramine(chloramine_water(), time = 30, cl2 = 3),
    nh3_only = chemdose_chloramine(chloramine_water(), time = 30, nh3 = 1)
  ))
  expect_golden_chloramine(out, "chemdose_chloramine_alk_time")
})

test_that("regression: chemdose_chloramine starting chloramine species", {
  w <- chloramine_water()
  w_nh2cl <- w
  w_nh2cl@nh2cl <- convert_units(2, "cl2")
  w_mix <- w
  w_mix@nh2cl <- convert_units(1, "cl2")
  w_mix@nhcl2 <- convert_units(0.5, "cl2")
  w_mix@ncl3 <- convert_units(0.1, "cl2")
  w_comb <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 80, tds = 200, combined_chlorine = 1.5))
  out <- suppressWarnings(list(
    nh2cl_start_no_dose = chemdose_chloramine(w_nh2cl, time = 60, cl2 = 0, nh3 = 0),
    nh2cl_start_cl2_dose = chemdose_chloramine(w_nh2cl, time = 30, cl2 = 3, nh3 = 0),
    mixed_start_nh3_dose = chemdose_chloramine(w_mix, time = 30, cl2 = 2, nh3 = 0.5),
    combined_slot_overridden = chemdose_chloramine(w_comb, time = 30, cl2 = 4, nh3 = 1)
  ))
  expect_golden_chloramine(out, "chemdose_chloramine_start_species")
})

test_that("regression: chemdose_chloramine free chlorine / ammonia slot paths", {
  w <- suppressWarnings(define_water(
    ph = 7.5, temp = 20, alk = 80, tds = 200, free_chlorine = 4, tot_nh3 = 1
  ))
  out <- suppressWarnings(list(
    dose_args_ignore_slots = chemdose_chloramine(w, time = 30, cl2 = 2, nh3 = 0.5),
    missing_cl2_uses_slot = chemdose_chloramine(w, time = 30, nh3 = 0.5),
    missing_nh3_uses_slot = chemdose_chloramine(w, time = 30, cl2 = 2),
    missing_both_uses_slots = chemdose_chloramine(w, time = 30),
    free_cl_slot_only = chemdose_chloramine(w, time = 30, nh3 = 0.5, use_free_cl_slot = TRUE),
    free_cl_slot_plus_dose = chemdose_chloramine(w, time = 30, cl2 = 2, nh3 = 0.5, use_free_cl_slot = TRUE),
    tot_nh3_slot_only = chemdose_chloramine(w, time = 30, cl2 = 2, use_tot_nh3_slot = TRUE),
    tot_nh3_slot_plus_dose = chemdose_chloramine(w, time = 30, cl2 = 2, nh3 = 0.5, use_tot_nh3_slot = TRUE),
    both_slots_plus_doses = chemdose_chloramine(
      w,
      time = 30, cl2 = 2, nh3 = 0.5, use_free_cl_slot = TRUE, use_tot_nh3_slot = TRUE
    ),
    nutrients_water_slots = chemdose_chloramine(
      regression_waters()$nutrients,
      time = 45, use_free_cl_slot = TRUE, use_tot_nh3_slot = TRUE
    )
  ))
  expect_golden_chloramine(out, "chemdose_chloramine_slots")
})
