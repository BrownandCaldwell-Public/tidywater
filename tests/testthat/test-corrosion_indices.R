test_that("most indices won't work without ca, cl, so4", {

  water <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tds = 238))



  expect_equal(suppressWarnings(calculate_corrosion(water,index = "aggressive"))@aggressive, NA_real_)
  expect_error(suppressWarnings(calculate_corrosion(water,index = "ryznar")))
  expect_error(suppressWarnings(calculate_corrosion(water,index = "langelier")))
  expect_error(suppressWarnings(calculate_corrosion(water,index = "ccpp")))
  expect_equal(suppressWarnings(calculate_corrosion(water,index = "larsonskold"))@larsonskold, NA_real_)
  expect_equal(suppressWarnings(calculate_corrosion(water,index = "csmr"))@csmr, NA_real_)

})

test_that("function catches index typos", {

  water <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tds = 238,
                                         tot_hard = 100, cl = 40, so4 = 40))

    expect_error(calculate_corrosion(water, index = "csr"))
    expect_error(calculate_corrosion(water, index = c("aggressive", "ccccp")))
    expect_error(calculate_corrosion(water, index = c("ai", "ryznar", "ccpp", "csmr", "langelier")))
    expect_no_error(calculate_corrosion(water, index = c("ryznar", "csmr", "larsonskold"))) #no error
})

test_that("aggressive index works", {

  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca_hard = 200)) %>%
    calculate_corrosion(index = "aggressive")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 15, ca_hard = 200)) %>%
    calculate_corrosion(index = "aggressive")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 15, tot_hard = 150)) %>%
    calculate_corrosion(index = "aggressive")

  expect_equal(round(water1@aggressive), 13) # high alk
  expect_equal(round(water2@aggressive), 11) # low alk
  expect_equal(round(water3@aggressive), 11) # use tot_hard instead of ca_hard

})

test_that("csmr works", {

  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, cl = 100, so4 = 1)) %>%
    calculate_corrosion(index = "csmr")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, cl = 2, so4 = 150)) %>%
    calculate_corrosion(index = "csmr")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 15, tot_hard = 150, so4 = 5)) %>%
    balance_ions() %>%
    calculate_corrosion(index = "csmr")

  expect_equal(round(water1@csmr), 100) # high cl, low so4
  expect_equal(round(water2@csmr, 2), 0.01) # low cl high so4
  expect_equal(round(water3@csmr), 18) # use balance ions to get chloride
})

test_that("larsonskold works", {

  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, cl = 100, so4 = 1)) %>%
    calculate_corrosion(index = "larsonskold")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, cl = 2, so4 = 150)) %>%
    calculate_corrosion(index = "larsonskold")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, cl = 50, so4 = 30)) %>%
    balance_ions() %>%
    calculate_corrosion(index = "larsonskold")

  water4 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, cl = 150, so4 = 150)) %>%
    calculate_corrosion(index = "larsonskold")

  expect_equal(round(water1@larsonskold, 1), 0.7) # high cl, low so4
  expect_equal(round(water2@larsonskold, 1), 0.8) # low cl high so4
  expect_equal(round(water3@larsonskold, 2), 0.51) # use balance ions to get chloride
  expect_equal(round(water4@larsonskold), 74) # low alk
})

# test answers will probably change as we figure out which ph_s to use. For now, I'm using MWH's ph_s.
# tests will stay the same though
test_that("langelier works", {

  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca_hard = 100, tds = 173)) %>%
    calculate_corrosion(index = "langelier")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca_hard = 100, tds = 56)) %>%
    calculate_corrosion(index = "langelier")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172)) %>%
    calculate_corrosion(index = "langelier")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca_hard = 50, tds = 30)) %>%
    calculate_corrosion(index = "langelier")

  expect_equal(round(water1@langelier, 1), 0.8) # high alk
  expect_equal(round(water2@langelier, 1), -0.9) # low alk
  expect_equal(round(water3@langelier, 1), 0.7) # use tot_hard to get ca
  expect_equal(round(water4@langelier), -2) # low ph, alk, and hard to simulte highly corrosive water
})

# test answers will probably change as we figure out which ph_s to use. For now, I'm using MWH's ph_s.
# tests will stay the same though
test_that("ryznar works", {

  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca_hard = 100, tds = 173)) %>%
    calculate_corrosion(index = "ryznar")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca_hard = 100, tds = 56)) %>%
    calculate_corrosion(index = "ryznar")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172)) %>%
    calculate_corrosion(index = "ryznar")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca_hard = 50, tds = 30)) %>%
    calculate_corrosion(index = "ryznar")

  expect_equal(round(water1@ryznar), 6) # high alk
  expect_equal(round(water2@ryznar), 10) # low alk
  expect_equal(round(water3@ryznar), 7) # use tot_hard to get ca
  expect_equal(round(water4@ryznar), 11) # low ph, alk, and hard to simulte highly corrosive water
})

test_that("ccpp works", {

  water1 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, ca_hard = 100, tds = 173)) %>%
    calculate_corrosion(index = "ccpp")

  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 5, ca_hard = 100, tds = 56)) %>%
    calculate_corrosion(index = "ccpp")

  water3 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 200, tot_hard = 150, tds = 172)) %>%
    calculate_corrosion(index = "ccpp")

  water4 <- suppressWarnings(define_water(ph = 6.9, temp = 25, alk = 5, ca_hard = 50, tds = 30)) %>%
    calculate_corrosion(index = "ccpp")

  water5 <- suppressWarnings(define_water(ph = 6.85, temp = 25, alk = 80, ca_hard = 80, tds = 90)) %>%
    calculate_corrosion(index = "ccpp")

  expect_equal(round(water1@ccpp), 17) # high alk
  expect_equal(round(water2@ccpp, 1), -1.2) # low alk
  expect_equal(round(water3@ccpp), 16) # use tot_hard to get ca
  expect_equal(round(water4@ccpp), -4) # low ca
  expect_equal(round(water5@ccpp), -33) # low pH
})
