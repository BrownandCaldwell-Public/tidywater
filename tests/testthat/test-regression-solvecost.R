# Regression pins: solvecost_* ----
# Golden files pin the returned costs. See tests/testthat/README.md.

times <- c("day", "month", "year")

test_that("regression: solvecost_chem", {
  testthat::skip_on_cran()
  cases <- list()
  for (t in times) {
    cases[[paste0("alum_30mgL_10mgd_", t)]] <- solvecost_chem(dose = 30, flow = 10, cost = 0.3, time = t)
    cases[[paste0("naoh_50pct_", t)]] <- solvecost_chem(dose = 12, flow = 4.5, strength = 50, cost = 0.45, time = t)
  }
  cases$vector_doses <- solvecost_chem(dose = c(0, 5, 17.5, 40), flow = 2.2, strength = 37, cost = 0.12)
  cases$vector_flows_month <- solvecost_chem(dose = 8, flow = c(0.5, 1, 25), cost = 1.1, time = "month")
  expect_golden(cases, "solvecost_chem")
})

test_that("regression: solvecost_power", {
  testthat::skip_on_cran()
  cases <- list()
  for (t in times) {
    cases[[paste0("full_util_", t)]] <- solvecost_power(power = 50, cost = 0.08, time = t)
    cases[[paste0("util35_", t)]] <- solvecost_power(power = 220, utilization = 35, cost = 0.12, time = t)
  }
  cases$vector_power <- solvecost_power(power = c(0, 10, 75.5), utilization = 80, cost = 0.1)
  expect_golden(cases, "solvecost_power")
})

test_that("regression: solvecost_labor", {
  testthat::skip_on_cran()
  cases <- list()
  for (t in times) {
    cases[[paste0("fte1.5_", t)]] <- solvecost_labor(fte = 1.5, cost = 50000, time = t)
  }
  cases$vector_fte_year <- solvecost_labor(fte = c(0.25, 1, 4), cost = 85000, time = "year")
  expect_golden(cases, "solvecost_labor")
})

test_that("regression: solvecost_solids", {
  testthat::skip_on_cran()
  cases <- list()
  for (t in times) {
    cases[[paste0("alum_", t)]] <- solvecost_solids(alum = 30, flow = 10, turb = 5, cost = 0.05, time = t)
    cases[[paste0("ferricchloride_", t)]] <- solvecost_solids(ferricchloride = 20, flow = 10, turb = 5, cost = 0.05, time = t)
    cases[[paste0("ferricsulfate_", t)]] <- solvecost_solids(ferricsulfate = 25, flow = 10, turb = 5, cost = 0.05, time = t)
  }
  cases$all_terms <- solvecost_solids(
    alum = 10, ferricchloride = 5, ferricsulfate = 5, flow = 3.5, toc_removed = 1.2, ca_removed = 40,
    mg_removed = 8, polymer = 0.5, turb = 12, b = 2, cost = 0.07, time = "year"
  )
  cases$turb_only <- solvecost_solids(flow = 1, turb = 20, cost = 0.1)
  cases$vector_alum <- solvecost_solids(alum = c(0, 10, 50), flow = 4, turb = 2, cost = 0.05, time = "month")
  expect_golden(cases, "solvecost_solids")
})
