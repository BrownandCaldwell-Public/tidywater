
test_that("regulate_toc works", {

  reg_1 <- regulate_toc(50, 5, 2)
  reg_2 <- regulate_toc(50, 5, 4)

  expect_equal(reg_1$toc_compliance_status, "In Compliance")
  expect_equal(ncol(reg_1), 2)
  expect_equal(reg_2$toc_compliance_status, "Not Compliant")
  expect_equal(ncol(reg_2), 3)
})


test_that("regulate_toc warns when finished water TOC >= raw TOC, or raw TOC <= 2 mg/L", {

  # finished > raw
  expect_warning(regulate_toc(50, 3, 4))
  water1 <- suppressWarnings(regulate_toc(50, 3, 4))
  expect_equal(water1$toc_compliance_status, "Not Calculated")
  expect_equal(water1$toc_removal_percent, "Not Calculated")

  #finished = raw
  expect_warning(regulate_toc(50, 5, 5))
  water2 <- suppressWarnings(regulate_toc(50, 5,5))
  expect_equal(water2$toc_compliance_status, "Not Calculated")
  expect_equal(water2$toc_removal_percent, "Not Calculated")

  # raw < 2
  expect_warning(regulate_toc(65, 1, .5))
  water3 <- suppressWarnings(regulate_toc(65, 1,.5))
  expect_equal(water3$toc_compliance_status, "Not Calculated")
  expect_equal(water3$toc_removal_percent, "Not Calculated")

})


test_that("regulate_toc_df is same s as base function", {
  base <- regulate_toc(100, 4, 2)

  regulated <- water_df %>%
    slice(3) %>%
    select(toc_raw = toc, alk_raw = alk) %>%
    mutate(toc_finished = 2) %>%
    regulate_toc_df()

expect_equal(base$toc_compliance_status, regulated$toc_compliance_status)
expect_equal(base$toc_removal_percent, regulated$toc_removal_percent)


base2 <- regulate_toc(50, 4, 3.9)

regulated2 <- water_df %>%
  slice(9) %>%
  select(toc_raw = toc, alk_raw = alk) %>%
  mutate(toc_finished = 3.9) %>%
  regulate_toc_df()

expect_equal(base2$toc_compliance_status, regulated2$toc_compliance_status)
expect_equal(base2$toc_removal_percent, regulated2$toc_removal_percent)
expect_equal(base2$comment, regulated2$comment)

})


test_that("regulate_toc_df warns when raw TOC <= 2 mg/L", {
testthat::skip_on_cran()

regulated <- water_df %>%
  define_water_df() %>%
  chemdose_ph_df(alum = 30, output_water = "dosed") %>%
  chemdose_toc_df("dosed") %>%
  pluck_water(c("coagulated", "defined"), c("toc", "alk")) %>%
  select(toc_finished = coagulated_toc, toc_raw = defined_toc, alk_raw = defined_alk)

expect_warning(regulated %>% slice(1) %>% regulate_toc_df(), "Raw water TOC < 2")

water1 <- suppressWarnings(regulate_toc_df(regulated))

expect_equal(slice(water1, 1)$toc_compliance_status, "Not Calculated")
expect_equal(slice(water1, 5)$toc_compliance_status, "Not Compliant")
expect_equal(slice(water1, 12)$toc_compliance_status, "In Compliance")
})


test_that("regulate_toc_df warns when finished water TOC >= raw TOC", {

  regulated <- water_df %>%
    select(toc_raw = toc, alk_raw = alk) %>%
    mutate(toc_finished = 3.9)

  expect_warning(regulated %>% slice(1) %>%  regulate_toc_df(), "Finished water TOC is greater than or equal")

  water1 <- suppressWarnings(regulate_toc_df(regulated))

  expect_equal(slice(water1, 1)$toc_compliance_status, "Not Calculated")
})

test_that("regulate_toc_df can take column and argument inputs", {

  regulated1 <-  suppressWarnings(water_df %>%
                                    select(toc_raw = toc, alk_raw = alk) %>%
                                    mutate(toc_finished = seq(0.1, 1.2, .1)) %>%
                                    regulate_toc_df())

  regulated2 <-  suppressWarnings(water_df %>%
                                    select(toc_raw = toc) %>%
                                    mutate(toc_finished = seq(0.1, 1.2, .1)) %>%
                                    regulate_toc_df(alk_raw = 80))

  regulated3 <-  suppressWarnings(water_df %>%
                                    select(alk_raw = alk) %>%
                                    regulate_toc_df(toc_raw = c(2, 4), toc_finished = .7))

  expect_equal(slice(regulated1, 2)$toc_removal_percent, slice(regulated2, 2)$toc_removal_percent)
  expect_equal(slice(regulated3, 3)$toc_removal_percent, slice(regulated1, 7)$toc_removal_percent)

  expect_equal(nrow(regulated2), 12) # no cross join
  expect_equal(nrow(regulated3), 24) # cross joined

})
