# Chloramine Breakpoint Curve Simulator
# This function carries out the Chlorine Breakpoint analysis, predicting the residual chlorine and chloramine concentrations

#' @title Calculate Chlorine and Chloramine Concentrations with the Breakpoint Chlorination approach
#' 
#' @description \code{simulate_breakpoint}, adopted from the U.S. EPA's Chlorine Breakpoint Curve Simulator, 
#' calculates chlorine and chlorinamine concentrations based on the two papers Jafvert & Valentine 
#' (Environ. Sci. Technol., 1992, 26 (3), pp 577-586) and Vikesland et al. (Water Res., 2001, 35 (7), pp 1766-1776).
#' Required arguments include an object of class "water" created by \code{\link{define_water}}, chlorine dose, and reaction time.
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including temperature, pH, and alkalinity.
#'
#' @details The function will calculate the Chlorine and Chloramine concentrations and update the "water"
#' class object proceed to the next steps of the treatment chain. 
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param time Reaction time (minutes).
#' @param cl2  Applied chlorine dose (mg/L as Cl2).
#' @examples
#' example_breakpoint <- suppressWarnings(define_water(8, 20, 65, free_chlorine = 2, tot_nh3 = 1)) %>% 
#'   simulate_chloramine2(time=20)
#' example_breakpoint <- suppressWarnings(define_water(8, 20, 65)) %>% 
#'   simulate_chloramine2(time=20, cl2=2)
#'
#'
#' @export
#'
#' @returns A water class object with predicted Chlorine and Chloramine concentrations.
#'

# library(deSolve)
# library(ggplot2)
# library(reshape2)
# library(scales)
# library(tidywater)
# library(tidyverse)
# devtools::load_all() # comment out in file, run in console pane only


############################

simulate_breakpoint <- function(water,time, cl2, nh3, use_slots = FALSE) {

  if (missing(cl2)) { # does this mean cl2 as an argument is missing or just cl2 = 0, and if 0, just means intentional not dosing because dosing has been included in free_chlorine or just intentional not/dosing?
    cl2 <- water@free_chlorine
    TOTCl_ini <- cl2 } else {
      if (use_slots == FALSE) {
        TOTCl_ini <- water@free_chlorine
        # warning('Chemdose cl2 is defined but not incorporated in initial free chlorine to avoid double dosing, change to use_slots = TRUE if needed')
      }
    } # check w/ Sierra
  # print(cl2)
  
  if (missing(nh3)) {
    nh3 <- water@tot_nh3
    TOTNH_ini <- nh3
  } else {
    if (use_slots == FALSE) {
      TOTNH_ini <- water@tot_nh3
      # warning('Chemdose nh3 is defined but not incorporated in initial free ammonia to avoid double dosing, change to use_slots = TRUE if needed')
    }
  }
  
  if (use_slots == TRUE) { # suggests that totcl starts with existing free_chlorine and dosing
    TOTCl_ini <- water@free_chlorine + convert_units(cl2,'cl2') 
    TOTNH_ini <- water@tot_nh3 + convert_units(nh3, 'n') 
  }
  # print(TOTCl_ini)
  # print(TOTNH_ini)
  
  if (missing(time)) {
    stop("Missing value for reaction time. Please check the function inputs required to calculate chlorine/chloramine decay.")
  }
  
  if (!is.na(water@nh2cl)| !is.na(water@nhcl2) | !is.na(water@ncl3)) {
    warning("Chloramine presence in water class object")
  }

  time <- time*60
  ph <- water@ph
  alk <- water@alk
  temp <- water@temp
  # Convert temperature from Celsius to Kelvin
  T_K <- temp + 273.15

  # in moles/L
  # TOTCl_ini <- water@free_chlorine + convert_units(cl2,'cl2') # +dose # (tot_ocl is free chlorine, tot_ocl = HOCl + OCl-)
  # TOTNH_ini <- water@tot_nh3 + convert_units(nh3, 'n') # (tot_nh3 = NH3 + NH4+)
  
  # in mg/L
  CltoN_Mass <- convert_units(TOTCl_ini,'cl2','M','mg/L')/convert_units(TOTNH_ini,'n','M','mg/L')
  # print(CltoN_Mass)
  
  ks <- correct_k(water)

  # Calculate equilibrium constants for chloramine system adjusted for temperature
  KHOCl <- 10^(-(1.18e-4 * T_K^2 - 7.86e-2 * T_K + 20.5))  #10^-7.6
  KNH4 <- ks$knh4
  KH2CO3 <- ks$k1co3
  KHCO3 <- ks$k2co3
  pkw <- round((4787.3 / (T_K)) + (7.1321 * log10(T_K)) + (0.010365 * T_K) - 22.801, 1)
  KW <- 10^-pkw
  H <- 10^-ph
  OH <- KW/H
  
  # Calculate alpha values
  alpha0TOTCl <- 1/(1 + KHOCl/H)
  alpha1TOTCl <- 1/(1 + H/KHOCl)
  
  alpha0TOTNH <- 1/(1 + KNH4/H)
  alpha1TOTNH <- 1/(1 + H/KNH4)
  
  alpha0TOTCO <- 1/(1 + KH2CO3/H + KH2CO3*KHCO3/H^2)
  alpha1TOTCO <- calculate_alpha1_carbonate(H,ks)
  alpha2TOTCO <- calculate_alpha2_carbonate(H,ks)
  
  # Calculate total carbonate concentration (moles/L)
  TOTCO <- (alk/50000 + H - OH)/(alpha1TOTCO + 2 * alpha2TOTCO) 
  
  # Calculate carbonate species concentrations (moles/L)
  H2CO3 <- alpha0TOTCO*TOTCO # tot carbonate - bicarbonate - carbonate
  HCO3 <- alpha1TOTCO*TOTCO
  CO3 <- alpha2TOTCO*TOTCO
  
  # Calculated rate constants (moles/L and seconds) adjusted for temperature # chloramine rate constants (leave as is, or add in dataframe)
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
  
  if (is.na(water@nh2cl)){
    water@nh2cl <- 0
  }
  if (is.na(water@nhcl2)){
    water@nhcl2 <- 0
  }
  if (is.na(water@ncl3)){
    water@ncl3 <- 0
  }

  # Define function for chloramine system
  chloramine <- function(t, y, parms) {
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
  
    I_ini <- 0
      
    yin <- c(TOTNH = TOTNH_ini,
             TOTCl = TOTCl_ini, 
             NH2Cl = water@nh2cl,
             NHCl2 = water@nhcl2,
             NCl3 = water@ncl3,
             I = I_ini)
    
    #Solver of ODE System
    out <- as.data.frame(ode(func = chloramine, # revisit as.data.frame vs. data.frame
                                   parms = NULL,
                                   y = yin,
                                   times = seq(0,time,by=60),  # read ode function
                                   atol = 1e-12,
                                   rtol = 1e-12
                             )
                         )
  
  sim_data <- tail(out,n=1)
  
  # concentrations (moles/L)
  water@free_chlorine <- sim_data$TOTCl
  water@nh2cl <- sim_data$NH2Cl
  water@nhcl2 <- sim_data$NHCl2*2 
  water@ncl3 <- sim_data$NCl3*3 
  water@combined_chlorine <- water@nh2cl + water@nhcl2 + water@ncl3
  water@tot_nh3 <- sim_data$TOTNH

  return(water)
  
}



