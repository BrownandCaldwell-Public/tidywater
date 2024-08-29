# dissolve_pb----

test_that("dissolve_pb doesn't work without alkalinity and IS.", {
  water1 <- suppressWarnings(define_water(ph = 7, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 7, alk = 90))

  expect_error(dissolve_pb(water1))
  expect_error(dissolve_pb(water2))
})

test_that("dissolve_pb outputs total lead with various inputs for ionic strength", {
  water1 <- suppressWarnings(define_water(ph = 8, alk = 200, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 90, cond = 500))
  water3 <- suppressWarnings(define_water(ph = 8, alk = 90, tot_hard = 110, cl = 200))

  dissolved1 <- dissolve_pb(water1)
  dissolved2 <- dissolve_pb(water2)
  dissolved3 <- dissolve_pb(water3)

  expect_equal(signif(dissolved1$tot_dissolved_pb, 2), 1.1e-6)
  expect_equal(signif(dissolved2$tot_dissolved_pb, 2), 1.1e-6)
  expect_equal(signif(dissolved3$tot_dissolved_pb, 2), 1.1e-6)
})

test_that("dissolve_pb works.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 100, tds = 200, so4 = 120, cl = 50, tot_hard = 90)) %>%
    dissolve_pb()

  water2 <- suppressWarnings(define_water(ph = 7, alk = 100, temp = 25, cl = 100, tot_po4 = 2, so4 = 100, tot_hard = 50)) %>%
    dissolve_pb()

  water3 <- suppressWarnings(define_water(ph = 7, alk = 100, temp = 25, tds = 200)) %>%
    dissolve_pb(hydroxypyromorphite = "Zhu", pyromorphite = "Xie", laurionite = "Lothenbach")

  expect_equal(signif(water1$tot_dissolved_pb, 2), 1.2e-6)
  expect_equal(water1$controlling_solid, "Cerussite")

  expect_equal(signif(water2$tot_dissolved_pb, 2), 1.1e-8)
  expect_equal(water2$controlling_solid, "Pyromorphite")

  expect_equal(signif(water3$tot_dissolved_pb, 2), 1.2e-6)
  expect_equal(water3$controlling_solid, "Cerussite")
})

# calculate_dic----
test_that("calculate _dic doesn't work without ph or alkalinity.", {
  water1 <- suppressWarnings(define_water(ph = 7)) %>%
    calculate_dic()
  water2 <- suppressWarnings(define_water(alk = 70)) %>%
    calculate_dic()

  expect_equal(water1, NA_real_)
  expect_equal(water2, NA_real_)
})

test_that("calculate _dic works.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 200)) %>%
    calculate_dic()

  water2 <- suppressWarnings(define_water(ph = 6.5, alk = 5)) %>%
    calculate_dic()

  expect_equal(round(water1), 59)
  expect_equal(round(water2), 2)
})
