#test functionality 

test_that("check water does what it ought to", {

# passing a nonwater object
  df<-data.frame(ph=2, alk = 70)

  expect_error(check_water(df))

  #pull a water from water df 
water1<-water_df |> 
  dplyr::slice(1) |> 
  as.list() |> 
  do.call(define_water, args = _ )
  
  check1<-check_water(water1)

  expect_equal(nrow(check1),5)
#should be 5 total once acplicable models is in 
  
#handles NA values 
water2 <-define_water()
  
  check2<-check_water(water2)

  expect_equal(check2$value[1], NA_character_)
  expect_equal(check2$value[2], NA_character_)
  expect_equal(check2$value[3], "Cations Ca and Na missing")
})

test_that("Check aplicable models works",{

water1<-define_water()
water2<-define_water(ph=7, alk = 70)
  

check1<-check_water(water1)
check2<-check_water(water2)
  
  
  expect_equal(check1$value[5],"No aplicable models...")
  
  expect_equal(check2$value[5], "chemdose_ph, decarbonate_ph, dissolve_cu, opensys_ph, chemdose_chloramine, calculate_corrosion")
  
  


})