# chlorine breakpoint curve simulator
# These functions predict total trihalomethane (TTHM) and haloacetic acid (HAA) formation

#' @title Calculate DBP formation
#'
#' @description \code{chemdose_dbp} calculates disinfection byproduct (DBP) formation based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001). Required arguments include an object of class "water"
#' created by \code{\link{define_water}} chlorine dose, type, reaction time, and treatment applied (if any).
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including bromide, TOC, UV254, temperature, and pH.
#'
#' @details The function will calculate haloacetic acids (HAA) as HAA5, and total trihalomethanes (TTHM).
#' Use \code{summarise_wq} to quickly tabulate the results.
#'
#' @source TTHMs, raw: U.S. EPA (2001) equation 5-131
#' @source HAAs, raw: U.S. EPA (2001) equation 5-134
#' @source TTHMs, treated: U.S. EPA (2001) equation 5-139
#' @source HAAs, treated: U.S. EPA (2001) equation 5-142
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param time Reaction time (hours). Model results are valid for reaction times between 2 and 168 hours.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @param location Location for DBP formation, either in the "plant" (default), or in the distributions system, "ds".
#' @examples
#' example_dbp <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chemdose_dbp(cl2 = 2, time = 8)
#' example_dbp <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chemdose_dbp(cl2 = 3, time = 168, treatment = "coag", location = "ds")
#'
#' @export
#'
#' @returns A water class object with predicted DBP concentrations.
#'

# library(deSolve)
# library(ggplot2)
# library(reshape2)
# library(scales)
# library(tidywater)
# library(tidyverse)


# Define chloramine system simulation function, output a dataframe with a time series
simulate_chloramine1 <- function(water, initial_chemical, Free_mgL) { # time_m argument to be added 
  
  # Set general simulation parameters
  length_m <- 240
  ratio_step <- 0.2
  ratio_min <- 0.0
  ratio_max <- 15.0
  
  temp <- water@temp
  ph <- water@ph
  alk <- water@alk
  #tot_nh3 <- water@tot_nh3 # (tot_nh3 = NH3 + NH4+)
  tot_ocl <- water@tot_ocl # (tot_ocl is free chlorine, tot_ocl = HOCl + OCl-)
  
  
  #Set time steps
  time <- seq(from = 0, to = length_m*60, by = 60)
  # time can be assigned in the function, just a single number
  data_points <- length(time)
  
  #Get initial conditions based on various possible input scenarios
  
  #Calcualate initial total chlorine concentration
  #Free Chlorine
  if (initial_chemical == "chlorine") {
    #Set chlorine to nitrogen mass ratio number sequence
    CltoN_Mass <- seq(1, ratio_max, ratio_step)
    # CltoN_Mass <- can assign some value, or include in the function, or calculate from the free chlorine and NH3
    # may want to check this mass ratio later, note that in paper it's initially defined as a molar ratio mol Cl/mol N
    num_cond <- length(CltoN_Mass)
    
    #Calcualate initial total chlorine and total ammonia concentrations
    TOTCl_ini <- rep(Free_mgL/71000, num_cond) # total = given Free_mgL + tot_ocl? (check unit conversion, use convert units function)
    TOTNH_ini <- (Free_mgL/CltoN_Mass)/14000   # tot_nh3 from water? (check units, check units in a water tot_nh3, mg/L)
    # TOTNH_ini <- tot_nh3
    
  } # keep in moles, double check units
  
  #Free Ammonia
  if (initial_chemical == "ammonia") {
    #Set chlorine to nitrogen mass ratio number sequence
    CltoN_Mass <- seq(ratio_min, ratio_max, ratio_step)
    num_cond <- length(CltoN_Mass)
    
    #Calcualate initial total chlorine and total ammonia concentrations
    TOTCl_ini <- (CltoN_Mass*tot_nh3)/71000
    TOTNH_ini <- rep(tot_nh3/14000, num_cond)
    # TOTNH_ini <- tot_nh3
  }
  
  #Calcualate initial concentrations
  NH2Cl_ini <- rep(0, num_cond)
  NHCl2_ini <- rep(0, num_cond)
  NCl3_ini <- rep(0, num_cond)
  I_ini <- rep(0, num_cond)
  # NH2Cl_ini <- 0
  # NHCl2_ini <- 0
  # NCl3_ini <- 0
  # I_ini <- 0
  # these might not be true, because there's chlorine residual in previous function?
  
  # Convert temperature from Celsius to Kelvin
  T_K <- temp + 273.15
  
  # Calculate equilibrium constants for chloramine system adjusted for temperature
  KHOCl <- 10^(-(1.18e-4 * T_K^2 - 7.86e-2 * T_K + 20.5))  #10^-7.6
  KNH4 <- 10^(-(1.03e-4 * T_K^2 - 9.21e-2 * T_K + 27.6))   #10^-9.25
  KH2CO3 <- 10^(-(1.48e-4 * T_K^2 - 9.39e-2 * T_K + 21.2)) #10^-6.35
  KHCO3 <- 10^(-(1.19e-4 * T_K^2 - 7.99e-2 * T_K + 23.6))  #10^-10.33
  KW <- 10^(-(1.5e-4 * T_K^2 - 1.23e-1 * T_K + 37.3))      #10^-14
  
  # Calculate water species concentrations (moles/L)
  H <- 10^-ph
  OH <- KW/H
  
  # Calculate alpha values
  alpha0TOTCl <- 1/(1 + KHOCl/H)
  alpha1TOTCl <- 1/(1 + H/KHOCl)
  
  alpha0TOTNH <- 1/(1 + KNH4/H)
  alpha1TOTNH <- 1/(1 + H/KNH4)
  
  alpha0TOTCO <- 1/(1 + KH2CO3/H + KH2CO3*KHCO3/H^2)
  alpha1TOTCO <- 1/(1 + H/KH2CO3 + KHCO3/H)
  alpha2TOTCO <- 1/(1 + H/KHCO3 + H^2/(KH2CO3*KHCO3))
  
  # Calculate total carbonate concentration (moles/L)
  TOTCO <- (alk/50000 + H - OH)/(alpha1TOTCO + 2 * alpha2TOTCO)
  
  # Calculate carbonate species concentrations (moles/L)
  H2CO3 <- alpha0TOTCO*TOTCO
  HCO3 <- alpha1TOTCO*TOTCO
  CO3 <- alpha2TOTCO*TOTCO
  
  # Calculated rate constants (moles/L and seconds) adjusted for temperature
  k1 <- 6.6e8 * exp(-1510/T_K)                #4.2e6
  k2 <- 1.38e8 * exp(-8800/T_K)               #2.1e-5
  k3 <- 3.0e5 * exp(-2010/T_K)                #2.8e2        % -2080
  k4 <- 6.5e-7
  k5H <- 1.05e7 * exp(-2169/T_K)              #6.9e3        % off by a bit
  k5HCO3 <- 4.2e31 * exp(-22144/T_K)          #2.2e-1       % off by a bit
  k5H2CO3 <- 8.19e6 * exp(-4026/T_K)          #1.1e1
  k5 <- k5H*H + k5HCO3*HCO3 + k5H2CO3*H2CO3
  k6 <- 6.0e4
  k7 <- 1.1e2
  k8 <- 2.8e4
  k9 <- 8.3e3
  k10 <- 1.5e-2
  k11p <- 3.28e9*OH + 6.0e6*CO3                             # double check this and below
  k11OCl <- 9e4
  k12 <- 5.56e10
  k13 <- 1.39e9
  k14 <- 2.31e2
  
  # Define function for chloramine system
  chloramine <- function(t, y, parms) { # t argument is unused
    with(as.list(y), {
      
      dTOTNH <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H)
      dTOTCl <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 + k8*I*NHCl2 -
                   (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 + 2*k12*NHCl2*NCl3*OH + k13*NH2Cl*NCl3*OH -
                   2*k14*NHCl2*alpha1TOTCl*TOTCl)
      dNH2Cl <- (k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH - k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 - 2*k5*NH2Cl^2 +
                   2*k6*NHCl2*alpha1TOTNH*TOTNH*H - k9*I*NH2Cl - k10*NH2Cl*NHCl2 - k13*NH2Cl*NCl3*OH)
      dNHCl2 <- (k3*alpha0TOTCl*TOTCl*NH2Cl - k4*NHCl2 + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H - k7*NHCl2*OH - k8*I*NHCl2 -
                   k10*NH2Cl*NHCl2 - (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH -
                   k14*NHCl2*alpha1TOTCl*TOTCl)
      dNCl3 <- ((k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH - k13*NH2Cl*NCl3*OH)
      dI <- (k7*NHCl2*OH - k8*I*NHCl2 - k9*I*NH2Cl)
      list(c(dTOTNH, dTOTCl, dNH2Cl, dNHCl2, dNCl3, dI))
    })
  }
  
  #Initialize blank data frame for simulation results
  sim_data <- data.frame(TOTNH = numeric(),
                         TOTCl = numeric(),
                         NH2Cl = numeric(),
                         NHCl2 = numeric(),
                         NCl3 = numeric(),
                         I = numeric(),
                         Mass_Ratio = numeric()
  )
  
  for (i in 1:num_cond){
    #Set Initial Condition Variables
    yini <- c(TOTNH = TOTNH_ini[i],
              TOTCl = TOTCl_ini[i],
              NH2Cl = NH2Cl_ini[i],
              NHCl2 = NHCl2_ini[i],
              NCl3 = NCl3_ini[i],
              I = I_ini[i])
    
    # yin <- c(TOTNH = TOTNH_ini,   
    #          TOTCl = TOTCl_ini,
    #          NH2Cl = NH2Cl_ini,
    #          NHCl2 = NHCl2_ini,
    #          NCl3 = NCl3_ini,
    #          I = I_ini)
    
    #Solver of ODE System
    out <- cbind(as.data.frame(ode(func = chloramine,
                                   parms = NULL,
                                   y = yini,
                                   # y = yin,
                                   times = time,
                                   atol = 1e-12,
                                   rtol = 1e-12
    )
    ),
    Mass_Ratio = CltoN_Mass[i]
    )
    
    sim_data <- rbind(sim_data, out)
  }
  
  # Extract concentrations (moles/L) and convert to typical units (e.g., mg Cl2/L or mg N/L)
  sim_data$Total_Chlorine <- (sim_data$NH2Cl + sim_data$NHCl2*2 + sim_data$NCl3*3 + sim_data$TOTCl)*71000
  sim_data$Monochloramine <- sim_data$NH2Cl*71000
  sim_data$Dichloramine <- sim_data$NHCl2*71000*2
  sim_data$Trichloramine <- sim_data$NCl3*71000*3
  sim_data$Free_Chlorine <- sim_data$TOTCl*71000
  sim_data$Free_Ammonia <- sim_data$TOTNH*14000
  sim_data$Total_Ammonia_N <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*14000
  sim_data$Total_Ammonia_NH3 <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*17000
  sim_data$Cl2N <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_N
  sim_data$Cl2NH3 <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_NH3
  sim <- melt(sim_data, id.vars=c("time", "Mass_Ratio"), variable.name="chemical", value.name="concentration")
  
  # water@... <- ...
  
  
  #return(water)
  
  }


############################
# output for one time step 
# dose of chlorine (sum)
simulate_chloramine2 <- function(water,time) { # time in minutes
  
  time <- time*60 # convert to seconds, or define time with seconds as unit
  temp <- water@temp
  ph <- water@ph
  alk <- water@alk

  # in moles/L
  TOTCl_ini <- water@tot_ocl # +dose # (tot_ocl is free chlorine, tot_ocl = HOCl + OCl-)
  TOTNH_ini <- water@tot_nh3 # (tot_nh3 = NH3 + NH4+)
  # testing
  # TOTCl_ini <- TOTNH_ini/(71/14)*15
  
  # for testing purpose 
  # CltoN_Mass <-  TOTCl_ini/TOTNH_ini*(71/14)
  CltoN_Mass <- convert_units(TOTCl_ini,'cl2','M','mg/L')/convert_units(TOTNH_ini,'n','M','mg/L')
  
  #Calcualate initial concentrations
  NH2Cl_ini <- 0
  NHCl2_ini <- 0
  NCl3_ini <- 0
  I_ini <- 0
  # potential water slots for chloramines
  # these might not be true, because there's chlorine residual in previous function?
  # test with no chloramine as start
  # test with chloramine, check results, warning function
  
  # Convert temperature from Celsius to Kelvin
  T_K <- temp + 273.15
  
  # Calculate equilibrium constants for chloramine system adjusted for temperature
  KHOCl <- 10^(-(1.18e-4 * T_K^2 - 7.86e-2 * T_K + 20.5))  #10^-7.6
  KNH4 <- 10^(-(1.03e-4 * T_K^2 - 9.21e-2 * T_K + 27.6))   #10^-9.25
  KH2CO3 <- 10^(-(1.48e-4 * T_K^2 - 9.39e-2 * T_K + 21.2)) #10^-6.35
  KHCO3 <- 10^(-(1.19e-4 * T_K^2 - 7.99e-2 * T_K + 23.6))  #10^-10.33
  KW <- 10^(-(1.5e-4 * T_K^2 - 1.23e-1 * T_K + 37.3))      #10^-14
  
  # Calculate water species concentrations (moles/L)
  H <- 10^-ph
  OH <- KW/H
  
  # Calculate alpha values
  alpha0TOTCl <- 1/(1 + KHOCl/H)
  alpha1TOTCl <- 1/(1 + H/KHOCl)
  
  alpha0TOTNH <- 1/(1 + KNH4/H)
  alpha1TOTNH <- 1/(1 + H/KNH4)
  
  alpha0TOTCO <- 1/(1 + KH2CO3/H + KH2CO3*KHCO3/H^2)
  alpha1TOTCO <- 1/(1 + H/KH2CO3 + KHCO3/H)
  alpha2TOTCO <- 1/(1 + H/KHCO3 + H^2/(KH2CO3*KHCO3))
  
  # Calculate total carbonate concentration (moles/L)
  TOTCO <- (alk/50000 + H - OH)/(alpha1TOTCO + 2 * alpha2TOTCO)
  
  # Calculate carbonate species concentrations (moles/L)
  H2CO3 <- alpha0TOTCO*TOTCO
  HCO3 <- alpha1TOTCO*TOTCO
  CO3 <- alpha2TOTCO*TOTCO
  
  # Calculated rate constants (moles/L and seconds) adjusted for temperature
  k1 <- 6.6e8 * exp(-1510/T_K)                #4.2e6
  k2 <- 1.38e8 * exp(-8800/T_K)               #2.1e-5
  k3 <- 3.0e5 * exp(-2010/T_K)                #2.8e2        % -2080
  k4 <- 6.5e-7
  k5H <- 1.05e7 * exp(-2169/T_K)              #6.9e3        % off by a bit
  k5HCO3 <- 4.2e31 * exp(-22144/T_K)          #2.2e-1       % off by a bit
  k5H2CO3 <- 8.19e6 * exp(-4026/T_K)          #1.1e1
  k5 <- k5H*H + k5HCO3*HCO3 + k5H2CO3*H2CO3
  k6 <- 6.0e4
  k7 <- 1.1e2
  k8 <- 2.8e4
  k9 <- 8.3e3
  k10 <- 1.5e-2
  k11p <- 3.28e9*OH + 6.0e6*CO3                             # double check this and below
  k11OCl <- 9e4
  k12 <- 5.56e10
  k13 <- 1.39e9
  k14 <- 2.31e2
  
  # Define function for chloramine system
  chloramine <- function(t, y, parms) { # t argument is unused
    with(as.list(y), {
      
      dTOTNH <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H)
      dTOTCl <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 + k8*I*NHCl2 -
                   (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 + 2*k12*NHCl2*NCl3*OH + k13*NH2Cl*NCl3*OH -
                   2*k14*NHCl2*alpha1TOTCl*TOTCl)
      dNH2Cl <- (k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH - k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 - 2*k5*NH2Cl^2 +
                   2*k6*NHCl2*alpha1TOTNH*TOTNH*H - k9*I*NH2Cl - k10*NH2Cl*NHCl2 - k13*NH2Cl*NCl3*OH)
      # add in nitrite-/bromide-induced dNH2Cl loss
      
      dNHCl2 <- (k3*alpha0TOTCl*TOTCl*NH2Cl - k4*NHCl2 + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H - k7*NHCl2*OH - k8*I*NHCl2 -
                   k10*NH2Cl*NHCl2 - (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH -
                   k14*NHCl2*alpha1TOTCl*TOTCl)
      dNCl3 <- ((k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH - k13*NH2Cl*NCl3*OH)
      dI <- (k7*NHCl2*OH - k8*I*NHCl2 - k9*I*NH2Cl)
      list(c(dTOTNH, dTOTCl, dNH2Cl, dNHCl2, dNCl3, dI))
    })
  }
  
    yin <- c(TOTNH = TOTNH_ini,
             TOTCl = TOTCl_ini, 
             NH2Cl = NH2Cl_ini,
             NHCl2 = NHCl2_ini,
             NCl3 = NCl3_ini,
             I = I_ini)
    
    #Solver of ODE System
    out <- as.data.frame(ode(func = chloramine, # revisit as.data.frame vs. data.frame
                                   parms = NULL,
                                   y = yin,
                                   times = seq(0,240*60,by=60),  # read ode function
                                   atol = 1e-12,
                                   rtol = 1e-12
                             )
                         )
  
  sim_data <- out[out$time==time,]  
    
  # concentrations (moles/L)
  Total_Chlorine <- (sim_data$NH2Cl + sim_data$NHCl2*2 + sim_data$NCl3*3 + sim_data$TOTCl)
  Monochloramine <- sim_data$NH2Cl
  Dichloramine <- sim_data$NHCl2*2 
  Trichloramine <- sim_data$NCl3*3 
  Free_Chlorine <- sim_data$TOTCl
  Free_Ammonia <- sim_data$TOTNH
  Total_Ammonia_N <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)
  Total_Ammonia_NH3 <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)
  Cl2N <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_N
  Cl2NH3 <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_NH3
  # sim <- melt(sim_data, id.vars=c("time", "Mass_Ratio"), variable.name="chemical", value.name="concentration")
  
  water@tot_ocl <- Free_Chlorine
  water@tot_nh3 <- Free_Ammonia
    
  return(water)
  
}


#######################

test2 <- suppressWarnings(define_water(8, 20, 65, tot_ocl = 1, tot_nh3 = 1)) %>% 
  simulate_chloramine2(time=20)
test2@tot_nh3*14000

############
water <- suppressWarnings(define_water(8, 20, 65, tot_ocl = 2, tot_nh3 = 1))
# test1 <- simulate_chloramine1(water, 'chlorine', 1) 
test1 <- simulate_chloramine1(water, 'chlorine', Free_mgL=water@tot_nh3*15*14000 ) 
test1[test1['time']==1200&test1['chemical']=='Trichloramine'&test1['Mass_Ratio']==15,]


test3 <- simulate_chloramine('ammonia', TOTNH_mgL=1, pH=8, Alk=65, T_C=20, time_m=20)
test3[test3['time']==1200&test3['chemical']=='Trichloramine'&test3['Mass_Ratio']==15,]



#######
# next steps
# (checked) whenever needed, use the convert function instead of manual number
# (checked) check the simulate_chloramine 1,2 functions, if they give the same results (they should)
# (checked) check the questions on the margins of simulate_chloramine2
# (checked) if 1,2 give the same results, check against shiny app
# add in the part with something presence, correct for nitrite-/bromide-induced monochloramine loss 

# look into alpha calculations defined
# look into k calculations defined
# look into water treatment plant user's manual (for their alpha, k calculations, gibbs free energy...)
# ask about what outputs to keep, 
#   what slots they feed into, 
#   and which part of the treatment chain this breakpoint analysis fits into
#   revisit paper for the above

# put in checks for params missing
# fill in for function description
# write a loop for plotting

# add ks/as calculation in the main? or just better off to leave them in this function


# doses and time -- 
# chlorine dosing amount adequate?
# reaction time vs kinetics?
# chloramine decay -- free chlorine available?
# dose chlorine+ammonia -- (mono)chloramine, stability of the types of chloramine, stable residual
# chemical dosing -- affects basic parameters, or just what happens
# chlorine, slots consistency chlorine vs. free_chlorine, ph impacts