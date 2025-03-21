# Chloramine Breakpoint/Mono- formation

# test value compare
    # breakpoint trend, nh3 dose --> high conc of nhcl2 
    # test values, compare with shiny results
# plot
# add the nitrite, bromide dependence after passing all tests
# double check on all coeffs? (what's I_ini)

# Questions
# need a test to make sure use_slots = TRUE/FALSE is working? or just manual checking should be good?
# is use_slots for both chlorine and ammonia? add warning?
# is there a way to test if both warning will show up at the same time, or do we know that both will show up as long as we see one?
# add temp warning? 
# function name?
# not-so-relevant questions on test file chlordecay Ct?


################
test_that("chloramine_breakpoint returns free_chlorine = 0 when existing free chlorine in the system and chlorine dose are 0.", {
  water1 <- suppressWarnings(define_water(7.5, 20, 66))
  rewater1 <- suppressWarnings(simulate_breakpoint(water1, time = 8, cl2=0))
  
  expect_equal(rewater1@free_chlorine, 0)
  expect_equal(rewater1@tot_nh3, 0)
  
})

test_that("chloramine_breakpoint warns when chloramine is present in the system", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water1@nh2cl <- 1
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water2@nhcl2 <- 1
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water3@ncl3 <- 1
  
  expect_warning(simulate_breakpoint(water1, time = 20, cl2=1)) # cl2 dosage not incorporated and chloramine initial presence. 
  expect_warning(simulate_breakpoint(water2, time = 20, cl2=1))
  expect_warning(simulate_breakpoint(water3, time = 20, cl2=1))
})

test_that("chloramine_breakpoint stops working when inputs are missing", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5,tot_nh3 = 1))
  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 65, free_chlorine = 5,tot_nh3 = 1))

  expect_no_error(simulate_breakpoint(water1, time = 20, cl2 = 1))
  expect_error(simulate_breakpoint(water2, cl2 = 4, use_slots = TRUE)) # missing time
})

####################
# test_that("chloramine_breakpoint works.", {
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
#   simulate_breakpoint(time=20,cl2=1,nh3 = 1)
# 
# example_breakpoint@free_chlorine
# 
# # # # 
# water1 <- suppressWarnings(define_water(8, 20, 65, free_chlorine = 2,tot_nh3 = 1))
# water1@nh2cl <- 1
# example_breakpoint1 <- simulate_breakpoint(water1,time=20,cl2=1)
# 
# example_breakpoint1@free_chlorine
# 
# 
# example_breakpoint1<- suppressWarnings(define_water(8, 20, 65, free_chlorine = 10, tot_nh3 = 1)) %>%
#   simulate_breakpoint(time=37)
