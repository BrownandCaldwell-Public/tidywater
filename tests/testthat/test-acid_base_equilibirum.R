

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# Test define water function with all inputs provided
# To do: separate out unit conversions from CBA calcs and formatting.
test_that("Define water works", {
  water <- define_water(7, 20, 100, 50, 40, 10, 10, 10, 10)
  expect_equal(water$ph, 7)
  expect_equal(water$ca, 0.000399653)
  expect_equal(water$cba, 0.001998264)
  expect_equal(water$alk_eq, 0.001998264)
})

# Test chemical dosing
# To do: subdivide for each chemical.
test_that("Dose chemical works", {
  water1 <- define_water(6.7, 20, 20, 50, 40, 10, 10, 10, 10)
  water2 <- define_water(7.5, 20, 100, 50, 40, 10, 10, 10, 10)
  water3 <- define_water(7.5, 20, 20, 50, 40, 10, 10, 10, 10)
  water4 <- define_water(8, 20, 20, 50, 40, 10, 10, 10, 10)

  # test1 <- dose_chemical(water1, alum = 30)
  # test2 <- dose_chemical(water2, alum = 30)
  # test3 <- dose_chemical(water2, alum = 50, h2so4 = 20)
  # test4 <- dose_chemical(water3, alum = 50, naoh = 10)
  # test5 <- dose_chemical(water4, alum = 50)
  test6 <- dose_chemical(water4, naoh = 80)

  # expect_equal(round(test1$ph, 2), 5.68)
  # expect_equal(round(test1$alk, 0), 5)
  # expect_equal(round(test2$ph, 2), 6.94)
  # expect_equal(round(test2$alk, 0), 85)
  # expect_equal(round(test3$ph, 2), 6.38)
  # expect_equal(round(test3$alk, 1), 54.5)
  # expect_equal(round(test4$ph, 2), 6.09)
  # expect_equal(round(test4$alk, 1), 7.5)
  # expect_equal(round(test5$ph, 2), 4.01)
  # expect_equal(round(test5$alk, 0), -5)
  expect_equal(round(test6$ph, 2), 11.35)
  expect_equal(round(test6$alk, 0), 120)


})

test_that("pH solver works", {
  water_params <- tibble(cba = 0.001998264, kw = 10^-14, tot_co3 = 0.002444312, so4_dose = 0.000102, po4_dose = 0, tot_ocl = 0) # About 10 mg/L H2SO4
  phfinal <- solve_ph(water_params)

  expect_equal(phfinal, 6.79)
})



