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
  water11 <- define_water(ph = 7, alk = 100, temp = 20, tds = 100, tot_ocl = 3)
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

# Dose chemical ----

test_that("Dose chemical returns the same pH/alkalinity when no chemical is added.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, 0, 0, 0, 0, 0, 0, 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- chemdose_ph(water1, h2so4 = 0, h3po4 = 0)

  water3 <- define_water(ph = 7, temp = 20, alk = 100, tot_po4 = 2, tds = 200)
  water4 <- chemdose_ph(water3, naocl = 0, alum = 0, naoh = 0)

  expect_equal(water1@ph, water2@ph)
  expect_equal(water1@alk, water2@alk)
  expect_equal(water3@ph, water4@ph)
  expect_equal(water3@alk, water4@alk)
})

test_that("Dose chemical corrects ph when softening", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 350))
  water2 <- chemdose_ph(water1, caco3 = -100)
  water3 <- chemdose_ph(water1, caco3 = -100, softening_correction = TRUE)
  water4 <- chemdose_ph(water1, caco3 = 10, softening_correction = TRUE)
  water5 <- chemdose_ph(water1, caco3 = 10)

  expect_equal(round(water3@ph, 2), 3.86) # softening correction works
  expect_error(expect_equal(water2@ph, water3@ph)) # ph with/without softening correction are different
  expect_equal(water4@ph, water5@ph) # softening_correction should not affect pH without caco3 <0
})


# To do: subdivide for each chemical?
test_that("Dose chemical works", {
  water1 <- define_water(6.7, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- define_water(7.5, 20, 100, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water3 <- define_water(7.5, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  test1 <- chemdose_ph(water1, alum = 30)
  test2 <- chemdose_ph(water2, alum = 30)
  test3 <- chemdose_ph(water2, alum = 50, h2so4 = 20)
  test4 <- chemdose_ph(water3, alum = 50, naoh = 10)
  test5 <- chemdose_ph(water4, alum = 50)
  test6 <- chemdose_ph(water4, naoh = 80)
  test7 <- chemdose_ph(water1, nh42so4 = 5)
  test8 <- chemdose_ph(water4, nh4oh = 5)

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
  expect_equal(round(test7@ph, 1), 6.7)
  # This is not passing right now. Numbers from WTP model
  # expect_equal(round(test8@ph, 1), 9.7)
  # expect_equal(round(test8@alk, 0), 25)
})

test_that("Starting phosphate residual does not affect starting pH.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, tot_po4 = 5) %>%
    chemdose_ph())

  water2 <- water1 %>%
    chemdose_ph()

  water3 <- water2 %>%
    chemdose_ph()

  expect_equal(water1@ph, 7)
  expect_equal(water2@ph, 7)
  expect_equal(water3@ph, 7)
})

test_that("Phosphate dose works as expected.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 50, tot_po4 = 0, temp = 25))

  water2 <- chemdose_ph(water1, h3po4 = 1) # 0.969 as PO4
  water3 <- chemdose_ph(water1, h3po4 = 5) # 4.84 as PO4
  water4 <- chemdose_ph(water1, h3po4 = 10) # 9.69 as PO4

  expect_equal(round(water2@ph, 1), 7.0)
  expect_equal(round(water3@ph, 1), 6.9)
  expect_equal(round(water4@ph, 1), 6.7)
})

test_that("Starting chlorine residual does not affect starting pH.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, tot_ocl = 1) %>%
    chemdose_ph())

  water2 <- water1 %>%
    chemdose_ph()

  water3 <- water2 %>%
    chemdose_ph()

  expect_equal(water1@ph, 7)
  expect_equal(water2@ph, 7)
  expect_equal(water3@ph, 7)
})

test_that("Starting ammonia does not affect starting pH.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 10, tot_nh3 = 1) %>%
    chemdose_ph())

  water2 <- water1 %>%
    chemdose_ph()

  water3 <- water2 %>%
    chemdose_ph()

  expect_equal(water1@ph, 7)
  expect_equal(water2@ph, 7)
  expect_equal(water3@ph, 7)
})

# Solve Dose pH ----

test_that("Solve dose pH produces a warning and returns NA when target pH is unreachable but runs otherwise.", {
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_warning(solvedose_ph(water4, 6, "naoh"))
  expect_warning(solvedose_ph(water4, 6, "co2"))
  expect_equal(suppressWarnings(solvedose_ph(water4, 6, "co2")), NA)
  expect_no_warning(solvedose_ph(water4, 9, "naoh"))
  expect_no_error(solvedose_ph(water4, 9, "naoh"))
})

test_that("Solve dose pH doesn't run when target pH is out of range.", {
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_error(solvedose_ph(water4, 20, "naoh"))
})

test_that("Solve dose pH returns the correct values.", {
  water4 <- define_water(8, 20, 20, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  # these are based on current tidywater outputs
  expect_equal(solvedose_ph(water4, 11, "naoh"), 39.2)
  expect_equal(solvedose_ph(water4, 7, "co2"), 3.5)
  co2dose <- solvedose_ph(water4, 7, "co2")
  expect_equal(round(chemdose_ph(water4, co2 = co2dose)@ph, 1), 7)
})

test_that("Solve dose pH doesn't error when target pH is close to starting.", {
  water1 <- define_water(
    ph = 7.01, temp = 19, alk = 100, tot_hard = 100,
    ca = 26, mg = 8, tot_po4 = 1, tds = 200
  )

  expect_no_error(solvedose_ph(water1, 7, "h2so4"))

  water2 <- define_water(
    ph = 7.99, temp = 19, alk = 150, tot_hard = 100,
    ca = 26, mg = 8, tot_ocl = 1, tds = 200
  )

  expect_no_error(solvedose_ph(water2, 8, "naoh"))
})


# Solve Dose Alkalinity ----

test_that("Solve dose alk produces a warning and returns NA when target alk is unreachable but runs otherwise.", {
  water5 <- define_water(8, 20, 50, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)

  expect_warning(solvedose_alk(water5, 20, "naoh"))
  expect_equal(suppressWarnings(solvedose_alk(water5, 100, "h2so4")), NA)
  expect_no_warning(solvedose_alk(water5, 100, "naoh"))
  expect_no_error(solvedose_alk(water5, 100, "naoh"))
})

test_that("Solve dose alk works.", {
  water5 <- define_water(8, 20, 50, 70, 10, 10, 10, 10, 10, toc = 5, doc = 4.8, uv254 = .1)
  # these are based on current tidywater outputs
  expect_equal(solvedose_alk(water5, 100, "naoh"), 39.8)
  expect_equal(solvedose_alk(water5, 10, "h2so4"), 39.2)
  naohdose <- solvedose_alk(water5, 100, "naoh")
  expect_equal(signif(chemdose_ph(water5, naoh = naohdose)@alk, 2), 100)
})


# Blend waters ----

test_that("Blend waters gives error when ratios don't sum to 1 and runs otherwise.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- define_water(ph = 5, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water3 <- define_water(ph = 10, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)

  expect_error(blend_waters(c(water1, water2, water3), c(.5, .5, .5)))
  expect_no_error(blend_waters(c(water1, water2, water3), c(1 / 3, 1 / 3, 1 / 3)))
})

test_that("Blend waters outputs same water when ratio is 1 or the blending waters have the same parameters.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)
  water2 <- water1
  water3 <- define_water(ph = 10, temp = 25, alk = 100, so4 = 0, ca = 0, mg = 0, cond = 100, toc = 5, doc = 4.8, uv254 = .1)

  blend1 <- blend_waters(c(water1, water3), c(1, 0))
  blend1@applied_treatment <- "defined" # set treatments to be the same to avoid an error
  blend2 <- blend_waters(c(water1, water3), c(0, 1))
  blend2@applied_treatment <- "defined" # set treatments to be the same to avoid an error
  expect_equal(water1, blend1)
  expect_equal(water3, blend2)

  blend3 <- blend_waters(c(water1, water2), c(.5, .5))
  blend3@applied_treatment <- "defined"
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

test_that("Blend waters correctly handles treatment and list of estimated parameters.", {
  water1 <- define_water(ph = 7, temp = 25, alk = 100, tds = 100) %>%
    chemdose_ph(naoh = 5)
  water2 <- define_water(ph = 7, temp = 25, alk = 100, cond = 100) %>%
    balance_ions()
  water3 <- suppressWarnings(define_water(ph = 10, temp = 10, alk = 200, tot_hard = 100, cl = 100, na = 100))

  blend1 <- suppressWarnings(blend_waters(c(water1, water2), c(.5, .5)))
  blend2 <- suppressWarnings(blend_waters(c(water2, water3), c(.5, .5)))
  blend3 <- blend_waters(c(water1), c(1))

  expect_equal(blend1@applied_treatment, "defined_chemdosed_balanced_blended")
  expect_equal(blend2@applied_treatment, "defined_balanced_blended")
  expect_equal(blend1@estimated, "_cond_tds_na")
  expect_equal(blend2@estimated, "_tds_na_ca_mg_cond")
  expect_equal(blend3@estimated, water1@estimated)
})

test_that("Blend waters warns when some slots are NA.", {
  water1 <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 100, tot_hard = 100))
  water2 <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 100, na = 100))

  expect_warning(blend_waters(c(water1, water2), c(.5, .5)), "ca.+na")
})
