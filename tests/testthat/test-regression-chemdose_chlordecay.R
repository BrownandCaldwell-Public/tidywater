# Regression pins: chemdose_chlordecay ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.
# uniroot is run with tol = 1e-14, so the default rel_tol = 1e-8 is used.

test_that("regression: chemdose_chlordecay chlorine and chloramine grid", {
  testthat::skip_on_cran()
  ws <- regression_waters()
  grid <- expand.grid(
    water = c("typical", "high_toc", "cold"),
    model = c("chlorine_raw", "chlorine_coag", "chloramine"),
    dose = c(1.5, 4, 10),
    time = c(0.5, 8, 72),
    stringsAsFactors = FALSE
  )
  out <- lapply(seq_len(nrow(grid)), function(i) {
    cl_type <- if (grid$model[i] == "chloramine") "chloramine" else "chlorine"
    treatment <- if (grid$model[i] == "chlorine_coag") "coag" else "raw"
    w <- suppressWarnings(chemdose_chlordecay(
      ws[[grid$water[i]]],
      cl2_dose = grid$dose[i], time = grid$time[i], treatment = treatment, cl_type = cl_type
    ))
    c(free_chlorine = w@free_chlorine, combined_chlorine = w@combined_chlorine)
  })
  names(out) <- with(grid, paste0(water, "_", model, "_d", dose, "_t", time))
  expect_golden(out, "chemdose_chlordecay_grid")
})

test_that("regression: chemdose_chlordecay full waters", {
  testthat::skip_on_cran()
  ws <- regression_waters()
  expect_golden(
    suppressWarnings(list(
      typical_chlorine_raw = chemdose_chlordecay(ws$typical, cl2_dose = 3, time = 24),
      typical_chlorine_coag = chemdose_chlordecay(ws$typical, cl2_dose = 3, time = 24, treatment = "coag"),
      typical_chloramine = chemdose_chlordecay(ws$typical, cl2_dose = 3, time = 24, cl_type = "chloramine"),
      warm_chlorine_raw = chemdose_chlordecay(ws$warm, cl2_dose = 2, time = 4),
      # nutrients has tot_nh3 and an existing free_chlorine slot that gets overridden
      nutrients_chlorine_override = chemdose_chlordecay(ws$nutrients, cl2_dose = 2.5, time = 12),
      dose_0_chlorine = chemdose_chlordecay(ws$typical, cl2_dose = 0, time = 10),
      dose_0_chloramine = chemdose_chlordecay(ws$typical, cl2_dose = 0, time = 10, cl_type = "chloramine")
    )),
    "chemdose_chlordecay_waters"
  )
})

test_that("regression: chemdose_chlordecay use_chlorine_slot", {
  testthat::skip_on_cran()
  base <- suppressWarnings(define_water(
    ph = 7.5, temp = 20, alk = 80, tds = 200, toc = 3, uv254 = 0.08,
    free_chlorine = 2, combined_chlorine = 2
  ))
  expect_golden(
    suppressWarnings(list(
      chlorine_slot_only = chemdose_chlordecay(base, time = 8, use_chlorine_slot = TRUE),
      chlorine_slot_plus_dose = chemdose_chlordecay(base, cl2_dose = 1, time = 8, use_chlorine_slot = TRUE),
      chlorine_slot_coag = chemdose_chlordecay(
        base,
        time = 8, treatment = "coag", use_chlorine_slot = TRUE
      ),
      chloramine_slot_only = chemdose_chlordecay(
        base,
        time = 8, cl_type = "chloramine", use_chlorine_slot = TRUE
      ),
      chloramine_slot_plus_dose = chemdose_chlordecay(
        base,
        cl2_dose = 1, time = 8, cl_type = "chloramine", use_chlorine_slot = TRUE
      )
    )),
    "chemdose_chlordecay_slot"
  )
})

test_that("regression: chemdose_chlordecay long time floors residual at zero", {
  testthat::skip_on_cran()
  # The 0.85 residual correction (Eq. 5-118) would make ct_corrected < 0 once most
  # chlorine has decayed; the residual is floored at 0 (#524).
  ws <- regression_waters()
  expect_golden(
    suppressWarnings(list(
      high_toc_chlorine_raw_t500 = chemdose_chlordecay(ws$high_toc, cl2_dose = 2, time = 500),
      high_toc_chlorine_coag_t500 = chemdose_chlordecay(
        ws$high_toc,
        cl2_dose = 2, time = 500, treatment = "coag"
      ),
      typical_chloramine_t2000 = chemdose_chlordecay(ws$typical, cl2_dose = 2, time = 2000, cl_type = "chloramine")
    )),
    "chemdose_chlordecay_long_time"
  )
})
