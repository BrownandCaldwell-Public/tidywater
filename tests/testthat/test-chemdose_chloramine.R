# Chloramine Breakpoint/Mono- formation

# test value compare
    # breakpoint trend, nh3 dose --> high conc of nhcl2 
    # test values, compare with shiny results
# plot
# add the nitrite, bromide dependence after passing all tests
# double check on all coeffs? (what's I_ini)
# reference list add articles?
# reread function description and revise
# helper (chain) function

# Questions
# need a test to make sure use_slot = TRUE/FALSE is working? or just manual checking should be good?
# is use_slot for both chlorine and ammonia? add warning?
# is there a way to test if both warning will show up at the same time, or do we know that both will show up as long as we see one?
# test coverage error: could not find function ode?
# conceptual question: go to shiny app
# add temp warning? 
# function name?
# not-so-relevant questions on test file chlordecay Ct?

# capture warning

################
test_that("chemdose_chloramine returns free_chlorine = 0 when existing free chlorine in the system and chlorine dose are 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66))
  rewater1 <- suppressWarnings(chemdose_chloramine(water1, time = 8, cl2=0))
  
  expect_equal(rewater1@free_chlorine, 0)
})



test_that("chemdose_chloramine warns when chloramine is present in the system", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water1@nh2cl <- 1
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, tot_nh3 = 1))
  water2@nhcl2 <- 1
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water3@ncl3 <- 1
  water4 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 1,tot_nh3 = 2))
  
  warnings1 <- capture_warnings(chemdose_chloramine(water1, time = 20, cl2 = 3, nh3 = 1, cl_use_slot = TRUE, nh3_use_slot = TRUE, multi_nh3_source = 2)) 
  expect_equal(length(warnings1),3)
  warnings2 <- capture_warnings(chemdose_chloramine(water2, time = 5, cl2 = 1, nh3 = 2, nh3_use_slot = TRUE)) 
  expect_equal(length(warnings2),3)
  warnings3 <- capture_warnings(chemdose_chloramine(water3, time = 30, cl2 = 2, cl_use_slot = TRUE, nh3 = 2)) 
  expect_equal(length(warnings3),3)
  warnings4 <- capture_warnings(chemdose_chloramine(water4, time = 10, cl2 = 1, nh3 = 4, multi_cl_source = 2, multi_nh3_source = 2))
  expect_equal(length(warnings4),2)
})

test_that("chemdose_chloramine stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 65, free_chlorine = 5))

  # note suppressed warnings
  expect_no_error(suppressWarnings(chemdose_chloramine(water1, time = 20, cl2 = 1)))
  expect_error(chemdose_chloramine(water2, cl2 = 4, cl_use_slot = TRUE)) # missing time
})


test_that("chemdose_chloramine stops running when input multi_cl_source is set to an invalid value", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water2 <- suppressWarnings(define_water(ph = 9, temp = 25, alk = 75, free_chlorine = 5))
  
  expect_error(chemdose_chloramine(water1, time = 40, cl2 = 2, cl_use_slot = TRUE, multi_cl_source = 3))
  expect_error(chemdose_chloramine(water1, time = 40, cl2 = 2, nh3 = 2, cl_use_slot = TRUE, multi_cl_source = 3))
}) 

test_that("chemdose_chloramine stops running when input multi_nh3_source is set to an invalid value", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 1,tot_nh3 = 1))
  
  expect_error(chemdose_chloramine(water1, time = 40, cl2 = 2, cl_use_slot = TRUE, multi_nh3_source = -1))
  expect_error(chemdose_chloramine(water1, time = 10, nh3 = 2, nh3_use_slot = TRUE, multi_nh3_source = 10))
}) 

####################
# test_that("chemdose_chloramine works.", {
#   water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 3.5, uv254 = 0.1, br = 50))
#   water2 <- chemdose_chlordecay(water1, cl2_dose = 3, time = 8)
#   water3 <- chemdose_chlordecay(water1, cl2_dose = 4, time = 3, treatment = "coag")
#   water4 <- chemdose_chlordecay(water1, cl_type = 'chloramine', cl2_dose = 4, time = 5, treatment = "coag")
#   water5 <- suppressWarnings(define_water(ph = 7.5, temp = 20, toc = 1, uv254 = 0.04, br = 50))
#   water6 <- chemdose_chlordecay(water5, cl_type = 'chloramine', cl2_dose = 6, time = 10)
#   
#   expect_equal(signif(water2@free_chlorine, 3), 1.33E-5)
#   expect_equal(signif(water3@free_chlorine, 3), 3.28E-5)
#   expect_equal(signif(water4@combined_chlorine, 3), 5.24E-5)
#   expect_equal(signif(water6@combined_chlorine, 3), 8.0E-5)
# })


#########################

# example_breakpoint <- suppressWarnings(define_water(8, 20, 65, free_chlorine = 2,tot_nh3 = 1)) %>%
#   chemdose_chloramine(time=20,cl2=1,nh3 = 1)
# 
# example_breakpoint@free_chlorine
# 
# # # # 
# water1 <- suppressWarnings(define_water(8, 20, 65, free_chlorine = 2,tot_nh3 = 1))
# water1@nh2cl <- 1
# example_breakpoint1 <- chemdose_chloramine(water1,time=20,cl2=1)
# 
# example_breakpoint1@free_chlorine
# 
# example_breakpoint1<- suppressWarnings(define_water(8, 20, 65, free_chlorine = 10, tot_nh3 = 1)) %>%
#   chemdose_chloramine(time=37)


# library(deSolve)
# library(ggplot2)
# library(reshape2)
# library(scales)
# library(tidywater)
# library(tidyverse)
# devtools::load_all() # comment out in file, run in console pane only







