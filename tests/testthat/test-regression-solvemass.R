# Regression pins: solvemass_* ----
# Golden files pin the returned masses (lb/day). See tests/testthat/README.md.

test_that("regression: solvemass_chem", {
  testthat::skip_on_cran()
  expect_golden(
    list(
      alum_full_strength = solvemass_chem(dose = 30, flow = 10),
      naoh_50pct = solvemass_chem(dose = 12, flow = 4.5, strength = 50),
      zero_dose = solvemass_chem(dose = 0, flow = 4.5),
      vector_doses = solvemass_chem(dose = c(0, 5, 17.5, 40), flow = 2.2, strength = 37),
      vector_flows = solvemass_chem(dose = 8, flow = c(0.5, 1, 25))
    ),
    "solvemass_chem"
  )
})

test_that("regression: solvemass_solids totals and details", {
  testthat::skip_on_cran()
  cases <- list(
    alum = solvemass_solids(alum = 30, flow = 10, turb = 5),
    ferricchloride = solvemass_solids(ferricchloride = 20, flow = 10, turb = 5),
    ferricsulfate = solvemass_solids(ferricsulfate = 25, flow = 10, turb = 5),
    toc_removed = solvemass_solids(flow = 10, toc_removed = 1.5, turb = 5),
    ca_removed = solvemass_solids(flow = 10, ca_removed = 40, turb = 5),
    mg_removed = solvemass_solids(flow = 10, mg_removed = 8, turb = 5),
    polymer = solvemass_solids(flow = 10, polymer = 0.5, turb = 5),
    b_custom = solvemass_solids(flow = 10, turb = 5, b = 2.2),
    turb_zero = solvemass_solids(alum = 30, flow = 10, turb = 0),
    vector_alum = solvemass_solids(alum = c(0, 10, 50), flow = 4, turb = 2),
    details_alum = solvemass_solids(alum = 30, flow = 10, turb = 5, details = TRUE),
    details_ferricchloride = solvemass_solids(ferricchloride = 20, flow = 10, turb = 5, details = TRUE),
    details_ferricsulfate = solvemass_solids(ferricsulfate = 25, flow = 10, turb = 5, details = TRUE),
    details_all_terms = solvemass_solids(
      alum = 10, ferricchloride = 5, ferricsulfate = 5, flow = 3.5, toc_removed = 1.2, ca_removed = 40,
      mg_removed = 8, polymer = 0.5, turb = 12, b = 2, details = TRUE
    ),
    details_vector = solvemass_solids(alum = c(0, 10, 50), flow = 4, turb = 2, ca_removed = c(0, 20, 40), details = TRUE)
  )
  expect_golden(cases, "solvemass_solids")
})
