# Regression pins: end-to-end treatment trains ----
# Chains several models the way users do, pinning the water after every step. A refactor that
# changes how one model's output feeds the next (units, slots, re-speciation) shows up here even
# when each function's own golden still passes.

test_that("regression: conventional coagulation, chlorination, and corrosion", {
  testthat::skip_on_cran()
  steps <- suppressWarnings({
    raw <- regression_waters()$typical
    coag_ph <- chemdose_ph(raw, alum = 35)
    coag <- chemdose_toc(coag_ph, alum = 35, coeff = "Alum")
    adjusted <- chemdose_ph(coag, naoh = 8)
    chlorinated <- chemdose_ph(adjusted, naocl = 3)
    decayed <- chemdose_chlordecay(chlorinated, cl2_dose = 3, time = 24, treatment = "coag")
    dbps <- chemdose_dbp(chlorinated, cl2 = 3, time = 24, treatment = "coag", location = "ds")
    list(
      raw = raw, coag_ph = coag_ph, coag = coag, adjusted = adjusted, chlorinated = chlorinated,
      decayed = decayed, dbps = dbps,
      ct = solvect_chlorine(chlorinated, time = 60, residual = 2, baffle = 0.5),
      corrosion = calculate_corrosion(adjusted, index = c("aggressive", "ryznar", "langelier", "larsonskold", "csmr")),
      lead = dissolve_pb(adjusted),
      toc_rule = regulate_toc(raw_water = raw, treated_water = coag)
    )
  })
  expect_golden(steps, "conventional")

  # CCPP uses uniroot with its default tolerance; pin it separately with a looser tolerance.
  ccpp <- suppressWarnings(calculate_corrosion(steps$adjusted, index = "ccpp"))
  expect_golden(list(ccpp = ccpp), "conventional_ccpp", rel_tol = 1e-4, abs_tol = 1e-3)
})

test_that("regression: lime softening and recarbonation", {
  testthat::skip_on_cran()
  steps <- suppressWarnings({
    raw <- regression_waters()$hard_high_alk
    limed <- chemdose_ph(raw, caoh2 = 200)
    softened <- chemdose_ph(limed, caco3 = -180, softening_correction = TRUE)
    softened_toc <- chemdose_toc(softened, caoh2 = 200)
    recarbonated <- chemdose_ph(softened_toc, co2 = 25)
    list(
      raw = raw, limed = limed, softened = softened, softened_toc = softened_toc,
      recarbonated = recarbonated,
      co2_to_ph8 = solvedose_ph(softened_toc, target_ph = 8, chemical = "co2"),
      balanced = balance_ions(recarbonated, anion = "cl", cation = "na"),
      solids = solvemass_solids(flow = 5, turb = 3, ca_removed = 72, toc_removed = raw@toc - softened_toc@toc, details = TRUE)
    )
  })
  expect_golden(steps, "softening")
})

test_that("regression: ozone, biofiltration, GAC, and chloramination", {
  testthat::skip_on_cran()
  steps <- suppressWarnings({
    raw <- regression_waters()$high_toc
    coag_ph <- chemdose_ph(raw, ferricchloride = 40)
    coag <- chemdose_toc(coag_ph, ferricchloride = 40, coeff = "Ferric")
    ozonated <- ozonate_bromate(coag, dose = 2.5, time = 8, model = "Ozekin")
    biofiltered <- biofilter_toc(ozonated, ebct = 10, ozonated = TRUE)
    gac <- gac_toc(biofiltered, ebct = 15, model = "WTP", bed_vol = 12000, pretreat = "o3biof")
    chloraminated <- chemdose_chloramine(gac, time = 30, cl2 = 4, nh3 = 0.9)
    decayed <- chemdose_chlordecay(gac, cl2_dose = 3.5, time = 48, cl_type = "chloramine")
    dbps <- chemdose_dbp(gac, cl2 = 3.5, time = 48, treatment = "gac", cl_type = "chloramine")
    list(
      raw = raw, coag = coag, ozonated = ozonated, biofiltered = biofiltered, gac = gac,
      chloraminated = chloraminated,
      decayed = decayed, dbps = dbps,
      ozone_resid = solveresid_o3(coag, dose = 2.5, time = 8),
      ozone_ct = solvect_o3(coag, time = 8, dose = 2.5, baffle = 0.7)
    )
  })
  chloraminated <- steps$chloraminated
  steps$chloraminated <- NULL
  expect_golden(steps, "advanced")

  # The chloramine ODE varies ~1e-6 relative across platforms; abs_tol covers near-zero species (mol/L).
  expect_golden(list(chloraminated = chloraminated), "advanced_chloramine", rel_tol = 1e-4, abs_tol = 1e-12)
})

test_that("regression: blended sources with corrosion control", {
  testthat::skip_on_cran()
  steps <- suppressWarnings({
    surface <- regression_waters()$low_alk
    ground <- regression_waters()$hard_high_alk
    blended <- blend_waters(list(surface, ground), c(0.65, 0.35))
    opensys <- opensys_ph(blended)
    decarb <- decarbonate_ph(blended, co2_removed = 0.7)
    phosphate <- chemdose_ph(decarb, h3po4 = 3, naoh = 5)
    list(
      surface = surface, ground = ground, blended = blended, decarb = decarb, phosphate = phosphate,
      lead = dissolve_pb(phosphate),
      copper = dissolve_cu(phosphate),
      corrosion = calculate_corrosion(phosphate, index = c("aggressive", "ryznar", "langelier", "larsonskold", "csmr")),
      mg = convert_watermg(phosphate)
    )
  })
  expect_golden(steps, "blended")

  # opensys_ph uses uniroot with its default tolerance.
  expect_golden(list(opensys = opensys), "blended_opensys", rel_tol = 1e-4, abs_tol = 1e-10)
})

test_that("regression: data frame workflow matches the single-water chain", {
  testthat::skip_on_cran()
  df <- suppressWarnings(
    transform(water_df[1:6, ], br = 60) |>
      define_water_df("raw") |>
      chemdose_ph_df("raw", "coag_ph", alum = 30) |>
      chemdose_toc_df("coag_ph", "coag", alum = 30) |>
      chemdose_ph_df("coag", "chlorinated", naocl = 2) |>
      chemdose_dbp_df("chlorinated", "dbps", cl2 = 2, time = 24, treatment = "coag", location = "plant") |>
      calculate_corrosion_df("chlorinated", index = c("langelier", "larsonskold", "csmr"))
  )
  expect_golden(list(train = df), "water_df_train")
})

test_that("regression: reuse coagulation, ozone, and biofiltration", {
  testthat::skip_on_cran()
  steps <- suppressWarnings({
    raw <- define_water(
      ph = 7.2, temp = 22, alk = 180, tot_hard = 200, ca = 50, mg = 18, na = 90, k = 15, cl = 110, so4 = 80,
      tds = 650, toc = 7.5, doc = 7, uv254 = 0.11, br = 150, tot_nh3 = 0.5
    )
    coag_ph <- chemdose_ph(raw, alum = 60, h2so4 = 20)
    coag <- chemdose_toc(coag_ph, alum = 60, coeff = "Reuse Site Langmuir", fns = 0.55)
    ozonated <- ozonate_bromate(coag, dose = 4, time = 10, model = "Ozekin")
    biofiltered <- biofilter_toc(ozonated, ebct = 12, reuse = TRUE, ozone_dose = 4, media_type = "gac")
    biofiltered_atp <- biofilter_toc(ozonated, ebct = 12, reuse = TRUE, ozonated = TRUE, atp = 40)
    gac <- gac_toc(biofiltered, ebct = 15, model = "WTP", bed_vol = 8000, pretreat = "o3biof")
    list(
      raw = raw, coag = coag, ozonated = ozonated, biofiltered = biofiltered,
      biofiltered_atp = biofiltered_atp, gac = gac
    )
  })
  expect_golden(steps, "reuse")
})
