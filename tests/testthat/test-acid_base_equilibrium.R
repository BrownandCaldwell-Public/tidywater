# Solve pH ----

test_that("Solve pH returns correct pH with no chemical dosing.", {
  suppressWarnings( {
    water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
    water2 <- define_water(ph = 5, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
    water3 <- define_water(ph = 10, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, toc = 5, doc = 4.8, uv254 = .1)
  })

  water4 <- define_water(6.7, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water5 <- define_water(7.5, 20, 100, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water6 <- define_water(7.5, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water7 <- define_water(8, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  water8 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water9 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)

  expect_equal(solve_ph(water1), water1@ph)
  expect_equal(solve_ph(water2), water2@ph)
  expect_equal(solve_ph(water3), water3@ph)
  expect_equal(solve_ph(water4), water4@ph)
  expect_equal(solve_ph(water5), water5@ph)
  expect_equal(solve_ph(water6), water6@ph)
  expect_equal(solve_ph(water7), water7@ph)
  expect_equal(solve_ph(water8), water8@ph)
  expect_equal(solve_ph(water9), water9@ph)

})

# Dose chemical ----

test_that("Dose chemical returns the same pH/alkalinity when no chemical is added.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- chemdose_ph(water1, h2so4 = 0, h3po4 = 0)

  expect_equal(water1@ph, water2@ph)
  expect_equal(water1@alk, water2@alk)

})


# To do: subdivide for each chemical?
test_that("Dose chemical works", {
  water1 <- define_water(6.7, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- define_water(7.5, 20, 100, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water3 <- define_water(7.5, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water4 <- define_water(8, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  test1 <- chemdose_ph(water1, alum = 30)
  test2 <- chemdose_ph(water2, alum = 30)
  test3 <- chemdose_ph(water2, alum = 50, h2so4 = 20)
  test4 <- chemdose_ph(water3, alum = 50, naoh = 10)
  test5 <- chemdose_ph(water4, alum = 50)
  test6 <- chemdose_ph(water4, naoh = 80)
  # Rounded values from waterpro spot check (doesn't match with more decimals)
  expect_equal(round(test1@ph, 1), 5.7)
  expect_equal(round(test1@alk, 0), 5)
  expect_equal(round(test2@ph, 1), 6.9)
  expect_equal(round(test2@alk, 0), 85)
  expect_equal(round(test3@ph, 1), 6.3)
  expect_equal(round(test3@alk, 0), 54)
  expect_equal(round(test4@ph, 1), 6)
  expect_equal(round(test4@alk, 0), 7)
  expect_equal(round(test5@ph, 1), 4.0)
  expect_equal(round(test5@alk, 0), -5)
  expect_equal(round(test6@ph, 1), 11.4)
  expect_equal(round(test6@alk, 0), 120)

})

# Solve Dose pH ----

test_that("Solve dose pH produces a warning and returns NA when target pH is unreachable but runs otherwise.", {
  water4 <- define_water(8, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_warning(solvedose_ph(water4, 6, "naoh"))
  expect_warning(solvedose_ph(water4, 6, "co2"))
  expect_equal(suppressWarnings(solvedose_ph(water4, 6, "co2")), NA)
  expect_no_warning(solvedose_ph(water4, 9, "naoh"))
  expect_no_error(solvedose_ph(water4, 9, "naoh"))
})

test_that("Solve dose pH doesn't run when target pH is out of range.", {
  water4 <- define_water(8, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_error(solvedose_ph(water4, 20, "naoh"))
})

test_that("Solve dose pH returns the correct values.", {
  water4 <- define_water(8, 20, 20, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  # these are based on current tidywater outputs
  expect_equal(solvedose_ph(water4, 11, "naoh"), 39.2)
  expect_equal(solvedose_ph(water4, 7, "co2"), 3.5)
  co2dose <- solvedose_ph(water4, 7, "co2")
  expect_equal(round(chemdose_ph(water4, co2 = co2dose)@ph, 1), 7)
})


# Solve Dose Alkalinity ----

test_that("Solve dose alk produces a warning and returns NA when target alk is unreachable but runs otherwise.", {
  water5 <- define_water(8, 20, 50, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_warning(solvedose_alk(water5, 20, "naoh"))
  expect_equal(suppressWarnings(solvedose_alk(water5, 100, "h2so4")), NA)
  expect_no_warning(solvedose_alk(water5, 100, "naoh"))
  expect_no_error(solvedose_alk(water5, 100, "naoh"))
})

test_that("Solve dose alk works.", {
  water5 <- define_water(8, 20, 50, 50, 40, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  # these are based on current tidywater outputs
  expect_equal(solvedose_alk(water5, 100, "naoh"), 40.2)
  expect_equal(solvedose_alk(water5, 10, "h2so4"), 38.9)
  naohdose <- solvedose_alk(water5, 100, "naoh")
  expect_equal(round(chemdose_ph(water5, naoh = naohdose)@alk), 100)
})


# Blend waters ----

test_that("Blend waters gives error when ratios don't sum to 1 and runs otherwise.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- define_water(ph = 5, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water3 <- define_water(ph = 10, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)

  expect_error(blend_waters(c(water1, water2, water3), c(.5, .5, .5)))
  expect_no_error(blend_waters(c(water1, water2, water3), c(1 / 3, 1 / 3, 1 / 3)))
})

test_that("Blend waters outputs same water when ratio is 1 or the blending waters have the same parameters.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1) # same as water1
  water3 <- define_water(ph = 10, temp = 10, alk = 200, 0, 0, 0, 0, 0, 0, tds = 100, toc = 5, doc = 4.8, uv254 = .1)

  blend1 <- blend_waters(c(water1, water3), c(1, 0))
  blend2 <- blend_waters(c(water1, water3), c(0, 1))
  expect_equal(water1, blend1)
  expect_equal(water3, blend2)

  blend3 <- blend_waters(c(water1, water2), c(.5, .5))
  expect_equal(water1, blend3)

})

test_that("Blend waters conserves temperature and alkalinity.", {
  water2 <- define_water(ph = 7, temp = 20, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1) # same as water1
  water3 <- define_water(ph = 10, temp = 10, alk = 200, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)

  blend1 <- blend_waters(c(water2, water3), c(.5, .5))
  expect_equal(blend1@alk, 150)
  expect_equal(blend1@temp, 15)
})

test_that("Blend waters conserves DOC.", {
  water2 <- define_water(ph = 7, temp = 20, alk = 100, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 5, uv254 = .1) # same as water1
  water3 <- define_water(ph = 10, temp = 10, alk = 200, 0, 0, 0, 0, 0, 0, cond = 100, toc = 3, doc = 3, uv254 = .1)

  blend1 <- blend_waters(c(water2, water3), c(.5, .5))
  expect_equal(blend1@doc, 4)
})

