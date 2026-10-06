#test functionality 

test_that("check water does what it ought to", {
testthat::skip_on_cran()


# passing a nonwater object
  df<-data.frame(ph=2, alk = 70)

  expect_error(check_water(df))

# pull a water from water df 
water1<-suppressWarnings(water_df |> 
  dplyr::slice(1) |> 
  as.list() |> 
  do.call(define_water, args = _ ))
  
water2<-suppressWarnings(water_df |> 
  dplyr::slice(1) |> 
  as.list() |> 
  do.call(define_water, args = _ ))
  
  check1<-check_water(water1)
  check2<-check_water(water2)

  # both have the same applicable models
  expect_equal(check1[[2]], check2[[2]])
  
#handles NA values 
water3 <-suppressWarnings(define_water())
  
  check3<-check_water(water3)

  expect_equal(check3[[1]], "Total hardness check: tot_hard, Ca, or Mg missing.")
  expect_equal(check3[[3]], "TDS check: TDS or ions missing, cannot compare to sum of ions.")
  expect_equal(check3[[5]], "No applicable models...")


  # total hardness check works (first expect that ther is no flag, second expect flag)
  water4_noflag<-suppressWarnings(define_water(ca = 20, mg = 10, tot_hard = 100))
  check4_noflag<-check_water(water4_noflag)


  # No row for hardness issue
expect_equal(length(check4_noflag), 4)

  water4_flag<-suppressWarnings(define_water(ca = 20, mg = 10, tot_hard = 220))
  check4_flag<-check_water(water4_flag)

  #row for hardness issue 
expect_equal(length(check4_flag), 5)


  # ion bal PD check works (reasonable PD, then flag)
  water5_noFlag<-suppressWarnings(define_water(cl=15,na=10))
  check5_noFlag<-check_water(water5_noFlag)

  expect_equal(length(check5_noFlag), 4)


  water5_flag<-suppressWarnings(define_water(cl=10,na=10))
  check5_flag<-check_water(water5_flag)

  expect_equal(length(check5_flag), 5)

  # TDS test works (reasonable PD, then flag)
  water6_noflag<-suppressWarnings(define_water(cl=10,na=10, tds =22))
  check6_noflag<-check_water(water6_noflag)

expect_equal(length(check6_noflag), 3)
  
  water6_flag<-suppressWarnings(define_water(cl=15,na=10, tds = 20))
  check6_flag<-check_water(water6_flag)

expect_equal(length(check6_flag), 3)

  
  # chloramine checks 
water8_both<-suppressWarnings(define_water(ph=7))
check8_both<-check_water(water8_both)
  
  expect_equal(length(check8_both), 4)
  
  water8_complete<-suppressWarnings(define_water(ph=7))
  water8_complete@combined_chlorine<-1
  water8_complete@tot_nh3<-1
  check8_complete<-check_water(water8_complete)
  
  expect_equal(length(check8_complete), 5)
  


})

test_that("Check applicable models works",{
testthat::skip_on_cran()
water1<-suppressWarnings(define_water())
water2<-suppressWarnings(define_water(ph=7, alk = 70))
  

check1<-check_water(water1)
check2<-check_water(water2)
  
  
  expect_equal(check1[[5]],"No applicable models...")
  
  expect_equal(check2[[5]], "Applicable models: chemdose_ph, decarbonate_ph, dissolve_cu, opensys_ph, chemdose_chloramine.")
  
  #check skip works 

  check3<-check_water(water2, skip_models = TRUE)

  expect_equal(length(check2), 5)
  expect_equal(length(check3), 4)

})

test_that("check_water only reports no issues when there are none", {
  testthat::skip_on_cran()
  # a single issue is not followed by "No issues found"
  water1 <- suppressWarnings(define_water(
    ph = 7.5, temp = 20, alk = 80, ca = 30, mg = 8, na = 20, k = 3, cl = 25, so4 = 30, tds = 600
  ))
  check1 <- check_water(water1, skip_models = TRUE)

  expect_false("No issues found with water input." %in% unlist(check1))
  expect_true(any(grepl("TDS check", unlist(check1))))

  # a clean water reports no issues, with or without the models check
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 80, ca = 30, mg = 8, na = 20, cl = 25, so4 = 30))
  water2@tot_hard <- calculate_hardness(30, 8)
  water2@tds <- 113
  check2 <- check_water(water2, skip_models = TRUE)
  check3 <- check_water(water2)

  expect_equal(check2, list("No issues found with water input."))
  expect_equal(length(check3), 2)
  expect_match(check3[[1]], "Applicable models")
  expect_equal(check3[[2]], "No issues found with water input.")
})