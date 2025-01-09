# Solve pH ----

test_that("Solve pH returns correct pH with no chemical dosing.", {
  suppressWarnings({
    water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0)
    water2 <- define_water(ph = 5, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0)
    water3 <- define_water(ph = 10, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0)
  })

  water4 <- define_water(6.7, 20, 20, 70, 10, 10, 10, 10, 10, 10)
  water5 <- define_water(7.5, 20, 100, 70, 10, 10, 10, 10, 10)
  water6 <- define_water(7.5, 20, 20, 70, 10, 10, 10, 10, 10)
  water7 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10)

  water8 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water9 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  water10 <- define_water(ph = 7, alk = 100, temp = 20, tds = 100, tot_po4 = 3)
  water11 <- define_water(ph = 7, alk = 100, temp = 20, tds = 100, free_chlorine = 3)
  water12 <- define_water(ph = 7, alk = 100, temp = 20, tds = 100, tot_nh3 = 3)

  expect_equal(solve_ph(water1), water1@ph)
  expect_equal(solve_ph(water2), water2@ph)
  expect_equal(solve_ph(water3), water3@ph)
  expect_equal(solve_ph(water4), water4@ph)
  expect_equal(solve_ph(water5), water5@ph)
  expect_equal(solve_ph(water6), water6@ph)
  expect_equal(solve_ph(water7), water7@ph)
  expect_equal(solve_ph(water8), water8@ph)
  expect_equal(solve_ph(water9), water9@ph)
  expect_equal(solve_ph(water10), water10@ph)
  expect_equal(solve_ph(water11), water11@ph)
  expect_equal(solve_ph(water12), water12@ph)
})
