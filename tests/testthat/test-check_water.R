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
  
  check1<-check_water(water1)

  expect_equal(nrow(check1),6)
#should be 6 total once applicable models is in 
  
#handles NA values 
water2 <-suppressWarnings(define_water())
  
  check2<-check_water(water2)

  expect_equal(check2$value[2], NA_character_)
  expect_equal(check2$value[3], NA_character_)
  expect_equal(check2$value[4], "Cations Ca and Na missing")


  # total hardness check works (first expect that ther is no flag, second expect flag)
  water3_noflag<-suppressWarnings(define_water(ca = 20, mg = 10, tot_hard = 100))
  check3_noflag<-check_water(water3_noflag)

expect_equal(check3_noflag$flag[1], "None")

  water3_flag<-suppressWarnings(define_water(ca = 20, mg = 10, tot_hard = 220))
  check3_flag<-check_water(water3_flag)

expect_equal(check3_flag$flag[1], "Total hardness does not match calculated hardness from Ca and Mg")


  # ion bal PD check works (reasonable PD, then flag)
  water4_noFlag<-suppressWarnings(define_water(cl=15,na=10))
  check4_noFlag<-check_water(water4_noFlag)

  expect_equal(check4_noFlag$flag[2], "None")


  water4_flag<-suppressWarnings(define_water(cl=10,na=10))
  check4_flag<-check_water(water4_flag)

  expect_equal(check4_flag$flag[2], "Unreasonable PD > 10%")

  # TDS test works (reasonable PD, then flag)
  water5_noflag<-suppressWarnings(define_water(cl=10,na=10, tds =22))
  check5_noflag<-check_water(water5_noflag)

expect_equal(check5_noflag$flag[3], "None")
  
  water5_flag<-suppressWarnings(define_water(cl=15,na=10, tds = 20))
  check5_flag<-check_water(water5_flag)

expect_equal(check5_flag$flag[3], "Unreasonable PD > 10%")

  # ionic strength (anions missing)

water6<-suppressWarnings(define_water(na=20,cl=20))
check6<-check_water(water6)

expect_equal(check6$flag[4], "None")
  
water6_missing<-suppressWarnings(define_water(na=20))
check6_missing<-check_water(water6_missing)
  
expect_equal(check6_missing$flag[4], "Missing major ions")
  
  # chloramine checks 
water7_both<-suppressWarnings(define_water(ph=7))
check7_both<-check_water(water7_both)
  
  expect_equal(check7_both$value[5], "Total nitrogen and Cl missing")
  
water7_cl<-suppressWarnings(define_water(ph=7))
water7_cl@combined_chlorine<-1
check7_cl<-check_water(water7_cl)
  
  expect_equal(check7_cl$value[5], "There is no total Nitrogen")
  
water7_nh3<-suppressWarnings(define_water(ph=7))
water7_nh3@tot_nh3<-1
check7_nh3<-check_water(water7_nh3)
  
  expect_equal(check7_nh3$value[5], "There is no total Cl")
  
  water7_complete<-suppressWarnings(define_water(ph=7))
  water7_complete@combined_chlorine<-1
  water7_complete@tot_nh3<-1
  check7_complete<-check_water(water7_complete)
  
  expect_equal(check7_complete$value[5], "Cl and NH3 both present")
  


})

test_that("Check applicable models works",{
testthat::skip_on_cran()
water1<-suppressWarnings(define_water())
water2<-suppressWarnings(define_water(ph=7, alk = 70))
  

check1<-check_water(water1)
check2<-check_water(water2)
  
  
  expect_equal(check1$value[6],"No applicable models...")
  
  expect_equal(check2$value[6], "chemdose_ph, decarbonate_ph, dissolve_cu, opensys_ph, chemdose_chloramine")
  
  #check skip works 

  check3<-check_water(water2, skip_models = TRUE)

  expect_equal(nrow(check3), 5)

})