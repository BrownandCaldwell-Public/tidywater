# Chloramine Breakpoint/Mono- formation

# notes
# 1. added in time check can't be less than 1 min, otherwise will have a solver error, possibly caused by ode
# after 1 min, the time input doesn't even need to be whole numbers
# 2. redefined calculate_alpha functions for carbonate, just so alpha1 and alpha2 build off of alpha0, like phosphate
# 3. alpha0 and alpha1 for ammonia and ammonium, alpha0TOTNH is actually unused
# did some testing for the two different expressions for alpha1TOTNH, the returned concs are close (see the bottom of this script)
# 4. forms of chloramines, mol Cl2/L <--> mg Cl2/L and mol N/L <--> mg N/L?
# 5. if combined_chloramine is present, how to break it down (potentially based on pH), currently ignored
##  set total_chlorine = combined. Jiaming test? test if combined chlor  == mono, what  are results? or what if it's totchlor and ammonia, what species do we get? Make issue
# 6. when ode keeps oscillate around 0 and returns a negative number, set it to 0
#
# 7. see at the bottom of test script and commnented out section for concs calculation using the EPA script.
#

###############
library(dplyr)

test_that("chemdose_chloramine returns free_chlorine = 0 when existing free chlorine in the system and chlorine dose are 0.", {
  water1 <- suppressWarnings(define_water(7.5, 21, 66))
  water2 <- suppressWarnings(chemdose_chloramine(water1, time = 20))

  expect_equal(water2@free_chlorine, 0)
})

test_that("chemdose_chloramine warns when chloramine is already present in water.", {
  # add one chloramine species
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65))
  water1@nh2cl <- 1

  # add 2 chloramine species
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65))
  water2@nhcl2 <- 1
  water2@ncl3 <- 1

  # combined chlorine
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, combined_chlorine = 1))

  expect_warning(chemdose_chloramine(water1, time = 20, cl2 = 3, nh3 = 1), "nh2cl")
  expect_warning(chemdose_chloramine(water2, time = 20, cl2 = 3, nh3 = 1), "nh2cl")
  expect_warning(chemdose_chloramine(water3, time = 20, cl2 = 3, nh3 = 1), "combined_")
})

test_that("chemdose_chloramine warns when existing free cl2 or nh3 is ignored.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 2))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, tot_nh3 = 2))
  # both
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 2, tot_nh3 = 2))

  expect_warning(chemdose_chloramine(water1, time = 20, cl2 = 3, nh3 = 1), "ignored")
  expect_warning(chemdose_chloramine(water2, time = 20, cl2 = 3, nh3 = 1), "ignored")

  warnings <- capture_warnings(chemdose_chloramine(water3, time = 10, cl2 = 1, nh3 = 4))
  expect_equal(length(warnings), 2)
})

test_that("chemdose_chloramine stops working when inputs are missing.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5, tot_nh3 = 1))
  water2 <- suppressWarnings(define_water(ph = 8, temp = 25, alk = 65, free_chlorine = 5))

  # note suppressed warnings
  expect_no_error(suppressWarnings(chemdose_chloramine(water1, time = 20, cl2 = 1)))
  expect_error(chemdose_chloramine(water2, cl2 = 4, use_free_cl_slot = TRUE)) # missing time
})

test_that("chemdose_chloramine stops working when time input is set to less than 1 minute.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5, tot_nh3 = 1))

  expect_error(chemdose_chloramine(water1, time = 0.5, cl2 = 4, use_free_cl_slot = TRUE)) # time < 1 min
})

test_that("chemdose_chloramine uses both slot and dose when slots are set to TRUE.", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5))
  water2 <- suppressWarnings(define_water(ph = 9, temp = 25, alk = 75, tot_nh3 = 5))
  # both
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 2, tot_nh3 = 2))


  expect_warning(chemdose_chloramine(water1, time = 40, cl2 = 2, nh3 = 2, use_free_cl_slot = TRUE))
  expect_warning(chemdose_chloramine(water2, time = 40, cl2 = 2, nh3 = 4, use_tot_nh3_slot = TRUE))
  warnings <- capture_warnings(chemdose_chloramine(water3, time = 10, cl2 = 1, nh3 = 4))
  expect_equal(length(warnings), 2)
})

test_that("chemdose_chloramine uses slot only when dose is zero or missing or uses both when specified.", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 5))
  water2 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, tot_nh3 = 2))
  # both
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 65, free_chlorine = 2, tot_nh3 = 2))

  water4 <- chemdose_chloramine(water1, time = 40, nh3 = 2, use_free_cl_slot = TRUE)
  water5 <- chemdose_chloramine(water2, time = 40, cl2 = 5, use_tot_nh3_slot = TRUE)
  expect_warning(chemdose_chloramine(water2, time = 40, cl2 = 5), "slot")
  expect_warning(chemdose_chloramine(water3, time = 10, cl2 = 1, use_tot_nh3_slot = TRUE), "ignored")
  expect_warning(chemdose_chloramine(water3, time = 10, cl2 = 1, use_tot_nh3_slot = TRUE, use_free_cl_slot = TRUE), "BOTH")
  expect_equal(water4@nh2cl, water5@nh2cl)
})

# chemdose_chloramine_df ----
# Test that chemdose_chloramine_df outputs are the same as base function, chemdose_chloramine.
test_that("chemdose_chloramine_df outputs the same as base, chemdose_chloramine", {
  testthat::skip_on_cran()
  water0 <- define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05
  )

  water1 <- chemdose_chloramine(water0, time = 20, nh3 = 1, cl2 = 1)

  water2 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_chloramine_df(nh3 = 1, cl2 = 1, time = 20) %>%
    pluck_water(c("chloraminated"), c("free_chlorine", "nh2cl", "nhcl2", "ncl3", "combined_chlorine", "tot_nh3"))

  # check that pluck_cols does the same thing as pluck_Water
  water3 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_chloramine_df(nh3 = 1, cl2 = 1, time = 20, pluck_cols = TRUE)

  expect_equal(water2$chloraminated_free_chlorine[1], water1@free_chlorine)
  expect_equal(water2$chloraminated_combined_chlorine[1], water1@combined_chlorine)
  expect_equal(ncol(water2), ncol(water3))
  expect_equal(water2$chloraminated_combined_chlorine[1], water3$chloraminated_combined_chlorine[1])

  water3 <- suppressWarnings(define_water(
    ph = 7.9, temp = 20, alk = 50, tot_hard = 50, ca = 13, mg = 4, na = 20, k = 20,
    cl = 30, so4 = 20, tds = 200, cond = 100, toc = 2, doc = 1.8, uv254 = 0.05, free_chlorine = 2, tot_nh3 = 2
  ) %>%
    chemdose_chloramine(time = 30, nh3 = 4, cl2 = 5, use_free_cl_slot = TRUE, use_tot_nh3_slot = TRUE))


  water4 <- suppressWarnings(water_df %>%
                               slice(1) %>%
                               mutate(free_chlorine = 2, tot_nh3 = 2) %>%
                               define_water_df() %>%
                               chemdose_chloramine_df(time = 30, nh3 = 4, cl2 = 5, use_free_cl_slot = TRUE, use_tot_nh3_slot = TRUE, pluck_cols = TRUE))

  expect_equal(water4$chloraminated_free_chlorine[1], water3@free_chlorine)
  expect_equal(water4$chloraminated_combined_chlorine[1], water3@combined_chlorine)
})

# Test that output is a column of water class lists, and changing the output column name works

test_that("chemdose_chloramine_df output is list of water class objects, and can handle an ouput_water arg", {
  testthat::skip_on_cran()
  water1 <- water_df %>%
    slice(1) %>%
    define_water_df() %>%
    chemdose_chloramine_df(time = 10, nh3 = 3, cl2 = 3)

  water2 <- purrr::pluck(water1, "chloraminated", 1)

  water3 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    mutate(nh3 = 3) %>%
    chemdose_chloramine_df(output_water = "diff_name", time = 10, cl2 = 3))

  expect_s4_class(water2, "water") # check class
  expect_equal(names(water3[5]), "diff_name") # check if output_water arg works
})

# Check that this function can be piped to the next one
test_that("chemdose_chloramine_df works", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    mutate(
      nh3 = 2,
      cl2 = 3,
      time = 10
    ) %>%
    chemdose_chloramine_df())

  expect_equal(ncol(water1), 5) # check if pipe worked
})

# Check that variety of ways to input chemicals work
test_that("chemdose_chloramine_df can handle different ways to input chem doses", {
  testthat::skip_on_cran()
  water1 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    chemdose_chloramine_df(nh3 = 3, cl2 = 5, time = 30, pluck_cols = TRUE))

  water2 <- suppressWarnings(water_df %>%
    mutate(tot_nh3 = 2) %>%
    define_water_df() %>%
    mutate(
      nh3 = 3,
      cl2 = 5,
      time = 30
    ) %>%
    chemdose_chloramine_df(pluck_cols = TRUE))

  # test different ways to input chemical
  expect_equal(water1$chloraminated_free_chlorine, water2$chloraminated_free_chlorine)

  water3 <- suppressWarnings(water_df %>%
    define_water_df() %>%
    mutate(nh3 = seq(0, 11, 1)) %>%
    chemdose_chloramine_df(cl2 = c(5, 8), time = 30, pluck_cols = TRUE))

  water4 <- water3 %>%
    slice(4) # same starting wq as water 5

  water5 <- water1 %>%
    slice(4) # same starting wq as water 4

  expect_equal(
    water4$chloraminated_combined_chlorine,
    water5$chloraminated_combined_chlorine
  )

  expect_equal(
    water4$chloraminated_free_chlorine,
    water5$chloraminated_free_chlorine
  )

  water6 <- suppressWarnings(water_df %>%
    mutate(tot_nh3 = 2) %>%
    define_water_df() %>%
    mutate(nh3 = 3) %>%
    chemdose_chloramine_df(use_tot_nh3_slot = TRUE, cl2 = 5, time = 30, pluck_cols = TRUE))

  water7 <- suppressWarnings(water_df %>%
    mutate(tot_nh3 = 2) %>%
    define_water_df() %>%
    mutate(
      nh3 = 3,
      use_tot_nh3_slot = TRUE
    ) %>%
    chemdose_chloramine_df(cl2 = 5, time = 30, pluck_cols = TRUE))

  # test different ways to call use_slot
  expect_equal(water6$chloraminated_combined_chlorine, water7$chloraminated_combined_chlorine)

  # both waters 2 and 7 have starting tot_nh3, but only water 7 has use_tot_nh3_slot set to TRUE
  expect_error(
    expect_equal(water2$chloraminated_combined_chlorine, water7$chloraminated_combined_chlorine)
  )
})


# note that this test only passes when chemdose_chlorine uses the original alpha0TOTNH and alpha1TOTNH
#  test_that("chemdose_chloramine works.", {
#   water1 <- suppressWarnings(define_water(7.5, 20, 50, free_chlorine = 10, tot_nh3 = 1))
#   water2 <- suppressWarnings(define_water(8, 25, 60, free_chlorine = 6, tot_nh3 = 2))
#   water3 <- suppressWarnings(define_water(6.5, 21, 80, free_chlorine = 12, tot_nh3 = 2))
#   water4 <- suppressWarnings(define_water(6, 30, 90, free_chlorine = 10, tot_nh3 = 10/13))
#
#   water5 <- suppressWarnings(chemdose_chloramine(water1, time = 5))
#   water6 <- suppressWarnings(chemdose_chloramine(water2, time = 10))
#   water7 <- suppressWarnings(chemdose_chloramine(water3, time = 3))
#   water8 <- suppressWarnings(chemdose_chloramine(water4, time = 30))
#
#   # values calculated from original EPA function, run simulate_chloramine1 below
#   # set threshold for difference between models to be 0.2 mg/L
#   TH_cl2 <- convert_units(0.2,'cl2')
#   TH_nh3 <- convert_units(0.2,'n')
#
#   expect_lt(abs(2.164128e-05 - water5@free_chlorine), TH_cl2)
#   expect_lt(abs(1.139440e-05 - water5@nh2cl), TH_cl2)
#   expect_lt(abs(2.666012e-06 - water5@nhcl2), TH_cl2)
#   expect_lt(abs(6.743913e-07 - water5@ncl3), TH_cl2)
#   expect_lt(abs(2.856840e-10- water5@tot_nh3), TH_nh3)
#
#   expect_lt(abs(5.700349e-10 - water6@free_chlorine), TH_cl2)
#   expect_lt(abs(8.433662e-05 - water6@nh2cl), TH_cl2)
#   expect_lt(abs(7.578846e-08 - water6@nhcl2), TH_cl2)
#   expect_lt(abs(3.108072e-13 - water6@ncl3), TH_cl2)
#   expect_lt(abs(5.843269e-05 - water6@tot_nh3), TH_nh3)
#
#   expect_lt(abs(7.408670e-08 - water7@free_chlorine), TH_cl2)
#   expect_lt(abs(1.117277e-04 - water7@nh2cl), TH_cl2)
#   expect_lt(abs(2.678862e-05 - water7@nhcl2), TH_cl2)
#   expect_lt(abs(8.791807e-09 - water7@ncl3), TH_cl2)
#   expect_lt(abs(2.268390e-06 - water7@tot_nh3), TH_nh3)
#
#   expect_lt(abs(4.565128e-05 - water8@free_chlorine), TH_cl2)
#   expect_lt(abs(7.314484e-13 - water8@nh2cl), TH_cl2)
#   expect_lt(abs(1.414666e-08 - water8@nhcl2), TH_cl2)
#   expect_lt(abs(2.549642e-06 - water8@ncl3), TH_cl2)
#   expect_lt(abs(0 - water8@tot_nh3), TH_nh3)
#
# })


######################
# EPA script, generate numbers for the last test

# simulate_chloramine1 <- function(water, initial_chemical, Free_mgL, input_ratio, output_time, output_chemical) { # time_m argument to be added
#
#   # Set general simulation parameters
#   length_m <- 240
#   ratio_step <- 0.2
#   ratio_min <- 0.0
#   ratio_max <- 15.0
#
#   temp <- water@temp
#   ph <- water@ph
#   alk <- water@alk
#
#   #Set time steps
#   time <- seq(from = 0, to = length_m*60, by = 60)
#   data_points <- length(time)
#
#   #Get initial conditions based on various possible input scenarios
#
#   #Calcualate initial total chlorine concentration
#   #Free Chlorine
#   if (initial_chemical == "chlorine") {
#     #Set chlorine to nitrogen mass ratio number sequence
#     CltoN_Mass <- seq(1, ratio_max, ratio_step)
#     num_cond <- length(CltoN_Mass)
#
#     #Calcualate initial total chlorine and total ammonia concentrations
#     TOTCl_ini <- rep(Free_mgL/71000, num_cond)
#     TOTNH_ini <- (Free_mgL/CltoN_Mass)/14000
#
#   }
#
#   #Free Ammonia
#   if (initial_chemical == "ammonia") {
#     #Set chlorine to nitrogen mass ratio number sequence
#     CltoN_Mass <- seq(ratio_min, ratio_max, ratio_step)
#     num_cond <- length(CltoN_Mass)
#
#     #Calcualate initial total chlorine and total ammonia concentrations
#     TOTCl_ini <- (CltoN_Mass*water@tot_nh3)/71000
#     TOTNH_ini <- rep(water@tot_nh3/14000, num_cond)
#   }
#
#   #Calcualate initial concentrations
#   NH2Cl_ini <- rep(0, num_cond)
#   NHCl2_ini <- rep(0, num_cond)
#   NCl3_ini <- rep(0, num_cond)
#   I_ini <- rep(0, num_cond)
#
#
#   # Convert temperature from Celsius to Kelvin
#   T_K <- temp + 273.15
#
#   # Calculate equilibrium constants for chloramine system adjusted for temperature
#   KHOCl <- 10^(-(1.18e-4 * T_K^2 - 7.86e-2 * T_K + 20.5))  #10^-7.6
#   KNH4 <- 10^(-(1.03e-4 * T_K^2 - 9.21e-2 * T_K + 27.6))   #10^-9.25
#   KH2CO3 <- 10^(-(1.48e-4 * T_K^2 - 9.39e-2 * T_K + 21.2)) #10^-6.35
#   KHCO3 <- 10^(-(1.19e-4 * T_K^2 - 7.99e-2 * T_K + 23.6))  #10^-10.33
#   KW <- 10^(-(1.5e-4 * T_K^2 - 1.23e-1 * T_K + 37.3))      #10^-14
#
#   # Calculate water species concentrations (moles/L)
#   H <- 10^-ph
#   OH <- KW/H
#
#   # Calculate alpha values
#   alpha0TOTCl <- 1/(1 + KHOCl/H)
#   alpha1TOTCl <- 1/(1 + H/KHOCl)
#
#   alpha0TOTNH <- 1/(1 + KNH4/H)
#   alpha1TOTNH <- 1/(1 + H/KNH4)
#
#   alpha0TOTCO <- 1/(1 + KH2CO3/H + KH2CO3*KHCO3/H^2)
#   alpha1TOTCO <- 1/(1 + H/KH2CO3 + KHCO3/H)
#   alpha2TOTCO <- 1/(1 + H/KHCO3 + H^2/(KH2CO3*KHCO3))
#
#   # Calculate total carbonate concentration (moles/L)
#   TOTCO <- (alk/50000 + H - OH)/(alpha1TOTCO + 2 * alpha2TOTCO)
#
#   # Calculate carbonate species concentrations (moles/L)
#   H2CO3 <- alpha0TOTCO*TOTCO
#   HCO3 <- alpha1TOTCO*TOTCO
#   CO3 <- alpha2TOTCO*TOTCO
#
#   # Calculated rate constants (moles/L and seconds) adjusted for temperature
#   k1 <- 6.6e8 * exp(-1510/T_K)                #4.2e6
#   k2 <- 1.38e8 * exp(-8800/T_K)               #2.1e-5
#   k3 <- 3.0e5 * exp(-2010/T_K)                #2.8e2        % -2080
#   k4 <- 6.5e-7
#   k5H <- 1.05e7 * exp(-2169/T_K)              #6.9e3        % off by a bit
#   k5HCO3 <- 4.2e31 * exp(-22144/T_K)          #2.2e-1       % off by a bit
#   k5H2CO3 <- 8.19e6 * exp(-4026/T_K)          #1.1e1
#   k5 <- k5H*H + k5HCO3*HCO3 + k5H2CO3*H2CO3
#   k6 <- 6.0e4
#   k7 <- 1.1e2
#   k8 <- 2.8e4
#   k9 <- 8.3e3
#   k10 <- 1.5e-2
#   k11p <- 3.28e9*OH + 6.0e6*CO3                             # double check this and below
#   k11OCl <- 9e4
#   k12 <- 5.56e10
#   k13 <- 1.39e9
#   k14 <- 2.31e2
#
#   # Define function for chloramine system
#   chloramine <- function(t, y, parms) { # t argument is unused
#     with(as.list(y), {
#
#       dTOTNH <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H)
#       dTOTCl <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 + k8*I*NHCl2 -
#                    (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 + 2*k12*NHCl2*NCl3*OH + k13*NH2Cl*NCl3*OH -
#                    2*k14*NHCl2*alpha1TOTCl*TOTCl)
#       dNH2Cl <- (k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH - k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 - 2*k5*NH2Cl^2 +
#                    2*k6*NHCl2*alpha1TOTNH*TOTNH*H - k9*I*NH2Cl - k10*NH2Cl*NHCl2 - k13*NH2Cl*NCl3*OH)
#       dNHCl2 <- (k3*alpha0TOTCl*TOTCl*NH2Cl - k4*NHCl2 + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H - k7*NHCl2*OH - k8*I*NHCl2 -
#                    k10*NH2Cl*NHCl2 - (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH -
#                    k14*NHCl2*alpha1TOTCl*TOTCl)
#       dNCl3 <- ((k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH - k13*NH2Cl*NCl3*OH)
#       dI <- (k7*NHCl2*OH - k8*I*NHCl2 - k9*I*NH2Cl)
#       list(c(dTOTNH, dTOTCl, dNH2Cl, dNHCl2, dNCl3, dI))
#     })
#   }
#
#   #Initialize blank data frame for simulation results
#   sim_data <- data.frame(TOTNH = numeric(),
#                          TOTCl = numeric(),
#                          NH2Cl = numeric(),
#                          NHCl2 = numeric(),
#                          NCl3 = numeric(),
#                          I = numeric(),
#                          Mass_Ratio = numeric()
#   )
#
#   for (i in 1:num_cond){
#     #Set Initial Condition Variables
#     yini <- c(TOTNH = TOTNH_ini[i],
#               TOTCl = TOTCl_ini[i],
#               NH2Cl = NH2Cl_ini[i],
#               NHCl2 = NHCl2_ini[i],
#               NCl3 = NCl3_ini[i],
#               I = I_ini[i])
#
#     #Solver of ODE System
#     out <- cbind(as.data.frame(ode(func = chloramine,
#                                    parms = NULL,
#                                    y = yini,
#                                    # y = yin,
#                                    times = time,
#                                    atol = 1e-12,
#                                    rtol = 1e-12
#                                    )
#                               ),
#                   Mass_Ratio = CltoN_Mass[i]
#                   )
#
#     sim_data <- rbind(sim_data, out)
#   }
#
#   # Extract concentrations (moles/L) and convert to typical units (e.g., mg Cl2/L or mg N/L)
#   sim_data$Total_Chlorine <- (sim_data$NH2Cl + sim_data$NHCl2*2 + sim_data$NCl3*3 + sim_data$TOTCl)*71000
#   sim_data$Monochloramine <- sim_data$NH2Cl*71000
#   sim_data$Dichloramine <- sim_data$NHCl2*71000*2
#   sim_data$Trichloramine <- sim_data$NCl3*71000*3
#   sim_data$Free_Chlorine <- sim_data$TOTCl*71000
#   sim_data$Free_Ammonia <- sim_data$TOTNH*14000
#   sim_data$Total_Ammonia_N <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*14000
#   sim_data$Total_Ammonia_NH3 <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*17000
#   sim_data$Cl2N <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_N
#   sim_data$Cl2NH3 <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_NH3
#   sim <- melt(sim_data, id.vars=c("time", "Mass_Ratio"), variable.name="chemical", value.name="concentration")
#
#   # return(sim)
#
#   # result <- sim[sim$time == output_time*60 & sim$Mass_Ratio==input_ratio & sim$chemical == output_chemical,]$concentration
#   result <- sim[sim$time == output_time*60 & sim$Mass_Ratio==input_ratio,]
#   return(result)
#
# }
#
# # example
# water4 <- suppressWarnings(define_water(6, 30, 90, free_chlorine = 10, tot_nh3 = 10/13))
# water8 <- suppressWarnings(chemdose_chloramine(water4, time = 30))
# conc <- simulate_chloramine1(water4, "chlorine", Free_mgL = 10,
#                                input_ratio = 13, output_time = 30)
#
#
#
#
#
#

##############
# # test difference depending on alpha1TOTNH equations

# # with alpha1TOTNH <- 1/(1 + H/ks$knh4)
# example_df1 <- water_df %>%
#   mutate(free_chlorine = 10, tot_nh3 = 2) %>%
#   define_water_df() %>%
#   balance_ions_df() %>%
#   mutate(
#        time = 30,
#        multi_cl_source = 1,
#        multi_nh3_source = 1
#    ) %>%
#    chemdose_chloramine_df(input_water = "balanced_water",
#                              cl2 = seq(2, 24, 2),
#                              nh3 = 2)

# # With alpha1TOTNH <- calculate_alpha1_ammonia(H, ks)
# example_df2 <- water_df %>%
#   mutate(free_chlorine = 10, tot_nh3 = 2) %>%
#   define_water_df() %>%
#   balance_ions_df() %>%
#   mutate(
#     time = 30,
#     multi_cl_source = 1,
#     multi_nh3_source = 1
#   ) %>%
#   chemdose_chloramine_df(input_water = "balanced_water",
#                             cl2 = seq(2, 24, 2),
#                             nh3 = 2)
#
# test1 <- pluck_water(example_df1,input_waters = c('chloraminated'),'tot_nh3')
# test2 <- pluck_water(example_df2,input_waters = c('chloraminated'),'tot_nh3')
#
# test1 <- convert_units(test1$chloraminated_tot_nh3, 'n','M','mg/L')
# test2 <- convert_units(test2$chloraminated_tot_nh3, 'n', 'M','mg/L')
#
# plot(test1,test2, xlim=c(0,7),ylim=c(0,7))
#
# test <- test1-test2
# plot(test)

# # difference in mg/L:
# free_chlorine(0~0.003) nhcl2, ncl3, tot_nh3
# nh2cl (0~-0.25)
# nhcl2 (0~0.1)
# ncl3 (~0, alpha1TOTNH not in dNCl3 in ode)
# tot_nh3 (0~0.025)
