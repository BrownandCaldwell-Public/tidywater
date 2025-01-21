## solvemass_chem -------------
test_that("solvemass_chem returns 0 when inputs are 0, missing, or the wrong format.", {
  # no dose
  expect_equal(solvemass_chem(dose = 0, flow = 30), 0)
  expect_error(solvemass_chem(flow = 30))

  # no flow
  expect_equal(solvemass_chem(flow = 0, dose = 30), 0)
  expect_error(solvemass_chem(dose = 30))

  # format
  expect_error(solvemass_chem(dose = "alum", flow = 30, strength = 10))
    expect_error(solvemass_chem(dose = 30, flow = "20gpm", strength = 10))
     expect_error(solvemass_chem(dose = 30, flow =20, strength = "10%"))
})

test_that("solvemass_chem works.", {
  expect_equal(round(solvemass_chem(dose = 20, flow = 30, strength = 50)), 10008)

  test_df <- water_df %>%
    mutate(
      dose = 20,
      flow = seq(2, 24, 2),
      mass = solvemass_chem(dose = dose, flow = flow, strength = 50)
    )

  expect_equal(test_df$mass[1], 20 * 2 * 8.34 / (50 / 100))
})


# solvemass_solids -------------
test_that("solvemass_solids returns 0 when inputs are 0, missing, or the wrong format.", {
  # no turb
  expect_error(solvemass_solids(alum = 10, flow = 30))

  # no flow
  expect_equal(solvemass_solids(ferricchloride = 10, flow = 0, turb = 5), 0)
  expect_error(solvemass_solids(ferricchloride = 10, turb = 5))

  # format
  expect_error(solvemass_solids(alum = "20", flow = 30, cost = 0.2, turb = 5))
  expect_error(solvemass_solids(alum = 20, flow = "30", cost = 0.2, turb = 5))
  expect_error(solvemass_solids(alum = 20, flow = 30, cost = "$2.00", turb = 5))
  expect_error(solvemass_solids(alum = 20, flow = 30, cost = 0.2, turb = "5NTU"))
})

test_that("solvemass_solids works.", {
  expect_equal(round(solvemass_solids(alum = 20, flow = 30, turb = 2)), 2952)

  test_df <- water_df %>%
    mutate(
      alum = 20,
      flow = seq(2, 24, 2),
      mass = solvemass_solids(alum = alum, flow = flow, turb = 50)
    )

  expect_equal(test_df$mass[1], 8.34 * 2 * (0.44 * 20 + (50 * 1.5)))
})
