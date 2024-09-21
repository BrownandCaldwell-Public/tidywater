# Cost calc tests here

## solvecost_chem -------------
test_that("solvecost_chem returns 0 when inputs are 0, missing, or the wrong format.", {

  #no dose
  expect_equal(solvecost_chem(dose = 0, flow = 30, cost = 0.2), 0)
  expect_error(solvecost_chem(flow = 30, cost = 0.2))

  #no flow
  expect_equal(solvecost_chem(flow = 0, dose = 30, cost = 0.2), 0)
  expect_error(solvecost_chem(dose = 30, cost = 0.2))

  #no cost
  expect_equal(solvecost_chem(flow = 10, dose = 30, cost = 0), 0)
  expect_error(solvecost_chem(dose = 30, flow = 12))

  #time format
  expect_error(solvecost_chem(dose = 20, flow = 30, cost = 0.2, time = day))
  expect_error(solvecost_chem(dose = 20, flow = 30, cost = 0.2, time = "minutes"))
})

test_that("solvecost_chem works.", {

  expect_equal(round(solvecost_chem(dose = 20, flow = 30, cost = 0.2, time = "year")), 365542)
  expect_equal(round(solvecost_chem(dose = 20, flow = 30, cost = 0.2, time = "month")), 30441)
  expect_equal(round(solvecost_chem(dose = 20, flow = 30, cost = 0.2, time = "day")), 1001)
  expect_equal(round(solvecost_chem(dose = 20, flow = 30, cost = 0.2, strength = 20)), 5004)
})


## solvecost_power ---------------
test_that("solvecost_power returns 0 when inputs are 0, missing, or the wrong format.", {

  #no power
  expect_equal(solvecost_power(power = 0, cost = 0.2), 0)
  expect_error(solvecost_power(cost = 0.2))

  #no cost
  expect_equal(solvecost_power(power = 50, cost = 0), 0)
  expect_error(solvecost_power(power = 30))

  #time format
  expect_error(solvecost_power(dose = 20, flow = 30, cost = 0.2, time = day))
  expect_error(solvecost_power(dose = 20, flow = 30, cost = 0.2, time = "minutes"))
})

test_that("solvecost_power works.", {

  expect_equal(round(solvecost_power(power = 20, cost = 0.2, time = "year")), 35064)
  expect_equal(round(solvecost_power(power = 20, cost = 0.2, time = "month")), 2920)
  expect_equal(round(solvecost_power(power = 20, cost = 0.2, time = "day")), 96)
  expect_equal(round(solvecost_power(power = 20, cost = 0.2, utilization = 20)), 19)
})

# solvecost_solids -------------

test_that("solvecost_solids returns 0 when inputs are 0, missing, or the wrong format.", {

  #no turb
  expect_error(solvecost_solids(alum = 10, flow = 30, cost = 0.2))

  #no flow
  expect_equal(solvecost_solids(ferricchloride = 10, flow = 0, turb = 5, cost = 0.2), 0)
  expect_error(solvecost_solids(ferricchloride = 10, turb = 5, cost = 0.2))

  #no cost
  expect_equal(solvecost_solids(ferricsulfate = 10, flow = 10, turb = 5, cost = 0), 0)
  expect_error(solvecost_solids(ferricsulfate = 10, turb = 5, flow = 10))

  #time format
  expect_error(solvecost_solids(alum = 20, flow = 30, cost = 0.2, turb = 5, time = day))
  expect_error(solvecost_solids(alum = 20, flow = 30, cost = 0.2, turb = 5, time = "minutes"))
})

test_that("solvecost_solids works.", {

  expect_equal(round(solvecost_solids(alum = 20, flow = 30, cost = 0.2, turb = 2, time = "year")), 215670)
  expect_equal(round(solvecost_solids(ferricchloride = 20, flow = 30, cost = 0.2, turb =.1, time = "month")), 30669)
  expect_equal(round(solvecost_solids(ferricsulfate = 20, flow = 30, cost = 0.2, turb = 3, time = "day")), 806)
  expect_equal(round(solvecost_solids(alum = 20, ferricchloride = 30, flow = 30, cost = 0.2, turb =2, b = .5)), 1992)
})

## solvecost_labor ---------------
test_that("solvecost_labor returns 0 when inputs are 0, missing, or the wrong format.", {

  #no fte
  expect_equal(solvecost_labor(fte = 0, cost = 20000), 0)
  expect_error(solvecost_labor(cost = 20000))

  #no cost
  expect_equal(solvecost_labor(fte = 1.5, cost = 0), 0)
  expect_error(solvecost_labor(fte = 1.5))

  #time format
  expect_error(solvecost_labor(fte = 1, cost = 20000, time = day))
  expect_error(solvecost_labor(fte = 1, cost = 20000, time = "minutes"))
})

test_that("solvecost_labor works.", {

  expect_equal(round(solvecost_labor(fte = 2.0, cost = 20000, time = "year")), 40000)
  expect_equal(round(solvecost_labor(fte = 2.0, cost = 20000, time = "month")), 3331)
  expect_equal(round(solvecost_labor(fte = 2.0, cost = 20000, time = "day")), 110)
})