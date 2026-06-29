library(dplyr)

test_that("regulate_toc works", {
  reg_1 <- regulate_toc(alk_raw = 50, toc_raw = 5, toc_finished = 2)
  reg_2 <- regulate_toc(alk_raw = 50, toc_raw = 5, toc_finished = 4)

  expect_equal(reg_1$toc_compliance_status, TRUE)
  expect_equal(ncol(reg_1), 4)
  expect_equal(reg_2$toc_compliance_status, FALSE)
  expect_equal(ncol(reg_2), 4)
})


test_that("regulate_toc works for TOC >= raw TOC (not compliant), or raw TOC <= 2 mg/L (alternate compliance), and
TOC <= 2 mg/L without alternate compliance", {
  # finished > raw
  water1 <- (regulate_toc(alk_raw = 50, toc_raw = 3, toc_finished = 4))
  expect_equal(water1$toc_compliance_status, FALSE)
  expect_equal(water1$toc_removal_percent, -33.3)

  # finished = raw
  water2 <- (regulate_toc(alk_raw = 50, toc_raw = 5, toc_finished = 5))
  expect_equal(water2$toc_compliance_status, FALSE)
  expect_equal(water2$toc_removal_percent, 0)

  # raw < 2
  water3 <- (regulate_toc(alk_raw = 65, toc_raw = 1, toc_finished = 0.5))
  expect_equal(water3$toc_compliance_status, TRUE)
  expect_equal(water3$toc_removal_percent, 50)

   # raw < 2 (no alternative compliance)
  water4 <- (regulate_toc(alk_raw = 65, toc_raw = 1, toc_finished = 1.5, skip_alternate = TRUE))
  expect_equal(water4$toc_compliance_status, FALSE)
  expect_equal(water4$toc_removal_percent, -50)

   # raw < 2 (no alternative compliance (finished < raw))
  water5 <- (regulate_toc(alk_raw = 65, toc_raw = 1, toc_finished = 0.5, skip_alternate = TRUE))
  expect_equal(water5$toc_compliance_status, FALSE)
  expect_equal(water5$toc_removal_percent, 50)
})


test_that("regulate_toc_df is same s as base function", {
  base <- regulate_toc(alk_raw = 100, toc_raw = 4, toc_finished = 2)

  regulated <- water_df[3, ] %>%
    select(toc_raw = toc, alk_raw = alk) %>%
    mutate(toc_finished = 2) %>%
    regulate_toc_df()

  expect_equal(base$toc_compliance_status, regulated$toc_compliance_status)
  expect_equal(base$toc_removal_percent, regulated$toc_removal_percent)

  base2 <- regulate_toc(alk_raw = 50, toc_raw = 4, toc_finished = 3.9)

  regulated2 <- water_df[9, ] %>%
    select(toc_raw = toc, alk_raw = alk) %>%
    mutate(toc_finished = 3.9) %>%
    regulate_toc_df()

  expect_equal(base2$toc_compliance_status, regulated2$toc_compliance_status)
  expect_equal(base2$toc_removal_percent, regulated2$toc_removal_percent)
  expect_equal(base2$comment, regulated2$comment)
})


test_that("regulate_toc_df indicates as such when raw TOC <= 2 mg/L", {
  testthat::skip_on_cran()

  regulated <- water_df %>%
    define_water_df() %>%
    chemdose_ph_df(alum = 30, output_water = "dosed", keep_dose_cols = TRUE) %>%
    chemdose_toc_df("dosed", keep_dose_cols = TRUE) %>%
    pluck_water(c("coagulated", "defined"), c("toc", "alk")) %>%
    select(toc_finished = coagulated_toc, toc_raw = defined_toc, alk_raw = defined_alk)

  water1 <- suppressWarnings(regulate_toc_df(regulated))

  expect_equal(water1[1, ]$toc_compliance_status, TRUE)
  expect_equal(water1[5, ]$toc_compliance_status, FALSE)
  expect_equal(water1[12, ]$toc_compliance_status, TRUE)
})


test_that("regulate_toc_df indicates when finished water TOC >= raw TOC", {
  regulated <- water_df %>%
    select(toc_raw = toc, alk_raw = alk) %>%
    mutate(toc_finished = 3.9)

  water1 <- suppressWarnings(regulate_toc_df(regulated))

  expect_equal(slice(water1, 2)$toc_compliance_status, FALSE)
})

test_that("regulate_toc_df can take column and argument inputs", {
  regulated1 <- suppressWarnings(
    water_df %>%
      select(toc_raw = toc, alk_raw = alk) %>%
      mutate(toc_finished = seq(0.1, 1.2, .1)) %>%
      regulate_toc_df()
  )

  regulated2 <- suppressWarnings(
    water_df %>%
      select(toc_raw = toc) %>%
      mutate(toc_finished = seq(0.1, 1.2, .1)) %>%
      regulate_toc_df(alk_raw = 80)
  )

  regulated3 <- suppressWarnings(
    water_df %>%
      select(alk_raw = alk) %>%
      regulate_toc_df(toc_raw = c(2, 4), toc_finished = .7)
  )

  expect_equal(regulated1[2, ]$toc_removal_percent, regulated2[2, ]$toc_removal_percent)
  expect_equal(regulated3[3, ]$toc_removal_percent, regulated1[7, ]$toc_removal_percent)

  expect_equal(nrow(regulated2), 12) # no cross join
  expect_equal(nrow(regulated3), 24) # cross joined
})

test_that("regulate_toc_df can take water objects as inputs", {
  water_df2 <- water_df |> 
    define_water_df()

  result <- suppressWarnings(regulate_toc_df(water_df2))
  expect_equal(nrow(result), nrow(water_df2))
  expect_equal(ncol(result), ncol(water_df2) + 7)

  water1<-water_df |> define_water_df()
  water2<-water_df |> define_water_df() |> 
    mutate(toc <- (water_df$toc)/1.5)
  water3df<-data.frame(water1, water2)

  result2 <- suppressWarnings(regulate_toc_df(water3df,))


})


test_that("regulate_toc can take water objects as inputs", {
  water1 <- define_water(alk = 50, toc = 5)
  water2 <- define_water(toc = 2)

  # same calc as in function 
  removal <- ((5 - 2) / 5) * 100

  result <- suppressWarnings(regulate_toc(raw_water = water1, treated_water = water2))
  expect_equal(result$toc_compliance_status, TRUE)
  expect_equal(result$toc_removal_percent, removal)
  expect_equal(result$required_removal,0)
  # test errors if numerics are passed as water inputs, or water objects are put into numeric inputs
  # also check that numerics from water class slots can corretley be passed into numeric inputs
  expect_error(regulate_toc(raw_water= 1, treated_water= 1))

  expect_error(regulate_toc(alk_raw =  water1, toc_raw  = water2, toc_finished = water2))

  result2<- suppressWarnings(regulate_toc(alk_raw=water1@alk, toc_raw=water1@toc, toc_finished=water2@toc))

#expect warning that water inputs will be used over individual parameters if both are provided
  
  expect_warning(regulate_toc(raw_water=water1, treated_water=water2, alk_raw= 50, toc_raw = 5, toc_finished = 2),
  "Both water objects and individual parameters provided. Water object parameters will be used, and individual parameters will be ignored.")

  #new result is equal to old result, Reproducibility  
  expect_equal(result, result2)

  expect_equal(result2$comment, "Treated water TOC < 2 mg/L meets alternative compliance (sect 141.135 (a)(2)(ii))." )
})

test_that("regulate_toc can skip alternate compliance for water object imputs",{
  water1 <- define_water(alk = 65, toc = 1)
  water2 <- define_water(toc = 1.5)
  result <- regulate_toc(raw_water = water1, treated_water = water2, skip_alternate = TRUE)
  expect_equal(result$toc_compliance_status, FALSE)
  expect_equal(result$toc_removal_percent, -50)
})

test_that("alternate compliance  4 (dbp, alk, and toc) works",{

  water1 <- define_water(ph = 7, alk = 70, toc = 3)
  water2 <- define_water(ph = 7, alk = 70, br = 10, toc = 1.5)
  dpbwater <- chemdose_dbp(water2, cl2 = 2, time = 3)
  result <- regulate_toc(raw_water = water1, treated_water = water2, skip_alternate = TRUE)
  expect_equal(result$toc_compliance_status, TRUE)
  expect_equal(result$toc_removal_percent, 50)

  result2 <- regulate_toc(raw_water = water1, treated_water = dpbwater, skip_alternate = FALSE)
  expect_equal(result2$toc_compliance_status, TRUE)
  expect_equal(result2$toc_removal_percent, 50)

  water3 <- define_water(alk = 70, toc = 2.5)
  water4 <- define_water(ph=7, br = 10, toc = 2.1)
  dpbwater2 <- chemdose_dbp(water4, cl2 = 2, time = 3)
  result3 <- regulate_toc(raw_water = water3, treated_water = dpbwater2, skip_alternate = FALSE)
  expect_equal(result3$toc_compliance_status, TRUE)

  result4 <- regulate_toc(raw_water = water3, treated_water = dpbwater2, skip_alternate = TRUE)
  expect_equal(result4$toc_compliance_status, FALSE)

})