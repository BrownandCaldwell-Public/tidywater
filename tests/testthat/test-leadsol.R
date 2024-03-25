# dissolve_pb----

test_that("dissolve_pb doesn't output total lead without alkalinity.", {
  water1 <- suppressWarnings(define_water(ph = 7, tds = 200))
  dissolved <- dissolve_pb(water1) %>%
    drop_na(tot_dissolved_pb)
  expect_equal(0, nrow(dissolved))
})

test_that("dissolve_pb doesn't output total lead without conditions for ionic strength", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 90))
  dissolved <- dissolve_pb(water1) %>%
    drop_na(tot_dissolved_pb)
  expect_equal(0, nrow(dissolved))
})

test_that("dissolve_pb outputs total lead without various inputs for ionic strength", {
  water1 <- suppressWarnings(define_water(ph = 8, alk = 200, tds = 200))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 90, cond = 500))
  water3 <- suppressWarnings(define_water(ph = 8, alk = 90, tot_hard = 110, cl = 200))
  
  dissolved1 <- dissolve_pb(water1) %>%
    drop_na(tot_dissolved_pb)
  dissolved2 <- dissolve_pb(water2) %>%
    drop_na(tot_dissolved_pb)
  dissolved3 <- dissolve_pb(water3) %>%
    drop_na(tot_dissolved_pb)
  
  expect_equal(3, nrow(dissolved1))
  expect_equal(3, nrow(dissolved2))
  expect_equal(5, nrow(dissolved3))
})

test_that("dissolve_pb works.", {
  water1 <- suppressWarnings(define_water(ph = 7, alk = 100, tds = 200, so4 = 120, cl=50, tot_hard = 90)) %>%
    dissolve_pb()
  
  expect_equal(round(water1$Pb_2_plus[water1$species_name == "Cerussite"], 8), 2.6e-7)
  expect_equal(round(water1$tot_dissolved_pb[water1$species_name == "Anglesite"], 6), 1.38e-4)
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
  water1 <- suppressWarnings(define_water(ph = 7, alk =200)) %>%
    calculate_dic()
  
  water2 <- suppressWarnings(define_water(ph = 4, alk = 5)) %>%
    calculate_dic()
  
  expect_equal(round(water1), 28)
  # This is pretty weird, calc_dic needs some help
  expect_equal(round(water2), -206)
})
