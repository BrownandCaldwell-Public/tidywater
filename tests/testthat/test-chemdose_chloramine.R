# Chloramine Breakpoint/Mono- formation

# test value compare
    # check the effect of having chloramine presence
# make sure that the last test, the values from the original function is replicable
# chemdose_chloramine_chain
# sensitivity check
# add the nitrite, bromide dependence after passing all tests
# double check on all coeffs? (what's I_ini)
# coefficients make into the main?
# reference list add articles?
# reread function description and revise
# clean up function script and test script

# Questions

# asked about \code naming the simulate_breakpoint
# conceptual question: go to shiny app

###############
test_that("chemdose_chloramine returns free_chlorine = 0 when existing free chlorine in the system and chlorine dose are 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66))
  water2 <- suppressWarnings(chemdose_chloramine(water1, time = 10))
  
  expect_equal(water2@free_chlorine, 0)
})

test_that("chemdose_chloramine warns when chloramine is present in the system in the form of combined_chlorine.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5, combined_chlorine = 1, tot_nh3 = 1))
  
  warnings <- capture_warnings(chemdose_chloramine(water1, time = 20, cl2 = 3, nh3 = 1, cl_use_slot = TRUE, nh3_use_slot = TRUE, multi_nh3_source = 2)) 
  expect_equal(length(warnings),3)
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
  expect_error(chemdose_chloramine(water1, time = 40, cl2 = 2, nh3 = 2, cl_use_slot = TRUE, multi_cl_source = 4))
}) 

test_that("chemdose_chloramine stops running when input multi_nh3_source is set to an invalid value", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 1,tot_nh3 = 1))
  
  expect_error(chemdose_chloramine(water1, time = 40, cl2 = 2, cl_use_slot = TRUE, multi_nh3_source = -1))
  expect_error(chemdose_chloramine(water1, time = 10, nh3 = 2, nh3_use_slot = TRUE, multi_nh3_source = 10))
}) 

test_that("chemdose_chloramine works.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 50, free_chlorine = 10, tot_nh3 = 1))
  water2 <- suppressWarnings(define_water(8, 25, 60, free_chlorine = 6, tot_nh3 = 2))
  water3 <- suppressWarnings(define_water(6.5, 21, 80, free_chlorine = 12, tot_nh3 = 2))
  water4 <- suppressWarnings(define_water(6, 30, 90, free_chlorine = 10, tot_nh3 = 10/13))
  
  water5 <- suppressWarnings(chemdose_chloramine(water1, time = 5))
  water6 <- suppressWarnings(chemdose_chloramine(water2, time = 10))
  water7 <- suppressWarnings(chemdose_chloramine(water3, time = 3))
  water8 <- suppressWarnings(chemdose_chloramine(water4, time = 30))
  
  # values calculated from original EPA function
  expect_lt(abs(2.164128e-05 - water5@free_chlorine), convert_units(0.2,'cl2'))
  expect_lt(abs(1.139440e-05 - water5@nh2cl), convert_units(0.2,'cl2'))
  expect_lt(abs(2.666012e-06 - water5@nhcl2), convert_units(0.2,'cl2'))
  expect_lt(abs(6.743913e-07 - water5@ncl3), convert_units(0.2,'cl2'))
  expect_lt(abs(2.856840e-10- water5@tot_nh3), convert_units(0.2,'n'))
  
  expect_lt(abs(5.700349e-10 - water6@free_chlorine), convert_units(0.2,'cl2'))
  expect_lt(abs(8.433662e-05 - water6@nh2cl), convert_units(0.2,'cl2'))
  expect_lt(abs(7.578846e-08 - water6@nhcl2), convert_units(0.2,'cl2'))
  expect_lt(abs(3.108072e-13 - water6@ncl3), convert_units(0.2,'cl2'))
  expect_lt(abs(5.843269e-05 - water6@tot_nh3), convert_units(0.2,'n'))
  
  expect_lt(abs(7.408670e-08 - water7@free_chlorine), convert_units(0.2,'cl2'))
  expect_lt(abs(1.117277e-04 - water7@nh2cl), convert_units(0.2,'cl2'))
  expect_lt(abs(2.678862e-05 - water7@nhcl2), convert_units(0.2,'cl2'))
  expect_lt(abs(8.791807e-09 - water7@ncl3), convert_units(0.2,'cl2'))
  expect_lt(abs(2.268390e-06 - water7@tot_nh3), convert_units(0.2,'n'))
  
  expect_lt(abs(4.565128e-05 - water8@free_chlorine), convert_units(0.2,'cl2'))
  expect_lt(abs(7.314484e-13 - water8@nh2cl), convert_units(0.2,'cl2'))
  expect_lt(abs(7.314484e-13 - water8@nhcl2), convert_units(0.2,'cl2'))
  expect_lt(abs(1.414666e-08 - water8@ncl3), convert_units(0.2,'cl2'))
  expect_lt(abs(2.549642e-06 - water8@tot_nh3), convert_units(0.2,'n'))

})


#########################
# example_breakpoint2 <- suppressWarnings(define_water(7.5, 20, 60, free_chlorine = 10,tot_nh3 = 10/7)) %>% 
#   chemdose_chloramine(time = 20, cl2 = 2, cl_use_slot = TRUE)
# 
# ratio <- example_breakpoint2[[4]]
# water <- example_breakpoint2[[3]]
# sim_data <- example_breakpoint2[[2]]
# out <- example_breakpoint2[[1]]
# 
# out1 <- out
# out1$time <- out1$time/60
# out1$TOTNH <- convert_units(out$TOTNH,'n','M','mg/L')
# out1$TOTCl <- convert_units(out$TOTCl,'cl2','M','mg/L')
# out1$NH2Cl <-  convert_units(out$NH2Cl,'cl2','M','mg/L')
# out1$NHCl2 <- convert_units(out$NHCl2,'cl2','M','mg/L')
# out1$NCl3 <- convert_units(out$NCl3,'cl2','M','mg/L')
# out1$TOT <- out1$TOTCl + out1$NHCl2 + out1$NH2Cl + out1$NCl3
# 
# x <- out1$time
# freeNH3 <- out1$TOTNH
# freeCl <- out1$TOTCl
# mono <-  out1$NH2Cl
# di <- out1$NHCl2 
# tri <- out1$NCl3 
# totalCl <- out1$TOT
# 
# plot(x,freeCl, type='l', ylim=c(0,10))
# lines(x,mono,col = 'green')
# lines(x, di, col = 'blue')
# lines(x, tri, col = 'red')
# lines(x,totalCl,col='orange')
# 
# legend(x = "topright",          # Position
#        legend = c('freeCl','mono','di','tri','total'),  # Legend texts
#        lty = rep(1,5),           # Line types
#        # inset=c(-1,1),
#        col = c('black','green','blue','red','orange'),           # Line colors
#        lwd = 2)                 # Line width
# 

# 
# library(deSolve)
# library(ggplot2)
# library(reshape2)
# library(scales)
# library(tidywater)
# library(tidyverse)
# devtools::load_all() # comment out in file, run in console pane only





