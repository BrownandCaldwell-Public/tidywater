# Chlorine/Chloramine Breakpoint Curve Simulator
# This function carries out the Chlorine Breakpoint analysis, predicting the residual chlorine and chloramine concentrations

#' @title Calculate Chlorine and Chloramine Concentrations with the Breakpoint Chlorination approach
#' 
#' @description \code{simulate_breakpoint}, adopted from the U.S. EPA's Chlorine Breakpoint Curve Simulator, 
#' calculates chlorine and chlorinamine concentrations based on the two papers Jafvert & Valentine 
#' (Environ. Sci. Technol., 1992, 26 (3), pp 577-586) and Vikesland et al. (Water Res., 2001, 35 (7), pp 1766-1776).
#' Required arguments include an object of class "water" created by \code{\link{define_water}}, chlorine dose, and reaction time.
#' The function also requires additional water quality parameters defined in \code{\link{define_water}}
#' including temperature, pH, and alkalinity.
#'
#' @details The function will calculate the Chlorine and Chloramine concentrations and update the "water"
#' class object proceed to the next steps of the treatment chain.
#' 
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}

#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param time Reaction time (minutes).
#' @param cl2 Applied chlorine dose (mg/L as Cl2).If not specified, use free_chlorine slot in water.
#' @param nh3 Applied ammonia dose (mg/L as N). If not specified, use tot_nh3 slot in water.
#' @param multi_cl_source # of chlorine source in use, default to 1 (1 or 2).
#' @param multi_nh3_source # of ammonia source in use, default to 1 (1 or 2).
#' @param cl_use_slot If TRUE, uses free_chlorine slot in water instead of chlorine dose. Input for cl2 will be ignored. 
#'                    Default to FALSE, no effect if multi_cl_source is set to 2. 
#' @param nh3_use_slot If TRUE, uses tot_nh3 slot in water instead of ammonia dose. Input for nh3 will be ignored. 
#'                    Default to FALSE, no effect if multi_nh3_source is set to 2.
#' 
#' @examples
#' example_breakpoint1 <- suppressWarnings(define_water(7.5, 20, 65, free_chlorine = 5, 
#'   tot_nh3 = 1)) %>% chemdose_chloramine(time = 40, cl2 = 2, nh3 = 1, 
#'   cl_use_slot = TRUE, multi_nh3_source = 2)
#' example_breakpoint2 <- suppressWarnings(define_water(8, 20, 65)) %>% 
#'   chemdose_chloramine(time = 20, cl2 = 2, nh3 = 2, multi_cl_source = 2)
#'
#' @importFrom deSolve ode
#' @importFrom utils tail
#' @export
#'
#' @returns A water class object with predicted Chlorine and Chloramine concentrations.
#'
#'
#
chemdose_chloramine <- function(water,time, cl2, nh3, 
                                multi_cl_source = 1, cl_use_slot = FALSE,  
                                multi_nh3_source = 1, nh3_use_slot = FALSE) {
  if (missing(time)) {
    stop("Missing value for reaction time. Please check the function inputs required to run chemdose_chloramine")
  }
  
  
  if (multi_cl_source != 1 & multi_cl_source != 2) {
    stop("multi_cl_source should be '1' or '2'. Please check the function input value for multi_cl_source.")
  }
  
  if (multi_nh3_source != 1 & multi_nh3_source != 2) {
    stop("multi_nh3_source should be '1' or '2'. Please check the function input value for multi_cl_source.")
  }
  
  if (missing(cl2)) { 
    cl2 <- water@free_chlorine
    TOTCl_ini <- cl2 
    message <- sprintf("Chlorine dose is not specified, use free chlorine in water (%f mol/L) as the initial free chlorine.",water@free_chlorine)
    warning(message)
    
    } else {
      if (multi_cl_source == 1 & cl_use_slot == FALSE) { 
        TOTCl_ini <- convert_units(cl2,'cl2') 
        message <- sprintf("Chlorine dose is used as the initial free chlorine. Free chlorine in water (%f mol/L) is ignored. 
        If want to use free chlorine in water, please set cl_use_slot to TRUE or remove function input cl2.
        If want to use both sources, please set multi_cl_source to 2.", water@free_chlorine)
        warning(message)
        
      } else if (multi_cl_source == 1 & cl_use_slot == TRUE) {
        TOTCl_ini <- water@free_chlorine
        message <- sprintf("Free chlorine in water (%f mol/L) is used instead of chlorine dose as the initial free chlorine. 
        If want to use chlorine dose, please set cl_use_slot to FALSE.
        If want to use both sources, please set multi_cl_source to 2.", water@free_chlorine)
        warning(message)
        
      } else if (multi_cl_source == 2) {
        TOTCl_ini <- water@free_chlorine + convert_units(cl2,'cl2') 
        message <- sprintf("Chlorine dose and free chlorine in water (%f mol/L) are both incorporated into the initial free chlorine.
        If want to use a single chlorine source, please set multi_cl_source to 1 and specify TRUE/FALSE for cl_use_slot.", water@free_chlorine)
        warning(message)
        
      }
    }
  
  if (missing(nh3)) {
    nh3 <- water@tot_nh3
    TOTNH_ini <- nh3 
    message <- sprintf("Ammonia dose is not specified, use free ammonia in water (%f mol/L) as the intial free ammonia.", water@tot_nh3)
    warning(message)
    
    } else {
      if (multi_nh3_source == 1 & nh3_use_slot == FALSE) { 
        TOTNH_ini <- convert_units(nh3, 'n') 
        message <- sprintf("Ammonia dose is used as the initial free ammonia. Free ammonia in water (%f mol/L) is ignored.
        If want to use free ammonia in water, please set nh3_use_slot to TRUE or remove function input nh3.
        If want to use both sources, please set multi_nh3_source to 2.", water@tot_nh3)
        warning(message)
        
      } else if (multi_nh3_source == 1 & nh3_use_slot == TRUE) {
        TOTNH_ini <- water@tot_nh3
        message <- sprintf("Free ammonia in water (%f mol/L) is used instead of ammonia dose as the initial free ammonia.
        If want to use ammonia dose, please set nh3_use_slot to FALSE.
        If want to use both sources, please set multi_nh3_source to 2.", water@tot_nh3)
        warning(message)
        
      } else if (multi_nh3_source == 2) {
        TOTNH_ini <- water@tot_nh3 + convert_units(nh3, 'n') 
        message <- sprintf("Ammonia dose and free ammonia in water (%f mol/L) are both incorporated into the initial free ammonia. 
        If want to use a single ammonia source, please set multi_nh3_source to 1 and specify TRUE/FALSE for nh3_use_slot.", water@tot_nh3)
        warning(message)
        
      }
    }
  
  if (!is.na(water@nh2cl)| !is.na(water@nhcl2) | !is.na(water@ncl3)) {
    warning("Chloramine presence in water class object")
  }
  
  if (water@combined_chlorine != 0) {
    warning("Chloramine presence in water as combined_chloramine. Breakdown of combined_chlorine is potentially based on pH but will be subject to future discussion. Ignore for now.")
  }

  time <- time*60
  ph <- water@ph
  alk <- water@alk
  temp <- water@temp
  T_K <- temp + 273.15

  # in moles/L
  # TOTCl_ini <- water@free_chlorine + convert_units(cl2,'cl2') # +dose # (tot_ocl is free chlorine, tot_ocl = HOCl + OCl-)
  # TOTNH_ini <- water@tot_nh3 + convert_units(nh3, 'n') # (tot_nh3 = NH3 + NH4+)
  
  # in mg/L
  # CltoN_Mass <- convert_units(TOTCl_ini,'cl2','M','mg/L')/convert_units(TOTNH_ini,'n','M','mg/L')
  
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
    deSolve::ode
    out <- as.data.frame(ode(func = chloramine, # revisit as.data.frame vs. data.frame
                                   parms = NULL,
                                   y = yin,
                                   times = seq(0,time,by=60),  # read ode function
                                   atol = 1e-12,
                                   rtol = 1e-12
                             )
                         )
  
  sim_data <- tail(out,n=1)
  # noticed that some values turn out to be less than 0 and just oscillate around 0 as the ode calculates, may be set to NA
  sim_data[sim_data < 0] <- 0
  
  # concentrations (moles/L)
  water@free_chlorine <- sim_data$TOTCl
  water@nh2cl <- sim_data$NH2Cl
  water@nhcl2 <- sim_data$NHCl2 
  water@ncl3 <- sim_data$NCl3 
  water@combined_chlorine <- water@nh2cl + water@nhcl2 + water@ncl3
  water@tot_nh3 <- sim_data$TOTNH

  return(water)
  
}




#' Apply `chemdose_chloramine` within a data frame and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{chemdose_chloramine}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' free_chlorine, combined_chlorine, nh2cl, nhcl2, ncl3, tot_nh3 slots will be updated depending on chlorine type.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water_chain}}.
#'
#' If the input data frame has a chlorine dose column (cl2), ammonia dose column (nh3), or time column (time), the function will use those columns. Note:
#' The function can only take cl2_dose and time inputs as EITHER a column or as function arguments, not both.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column named for the applied chlorine dose (cl2_dose),
#' and a column for time in hours.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "chlorinated_water".
#' @param cl2 Applied chlorine dose (mg/L as cl2). 
#' @param nh3 Applied ammonia dose (mg/L as N).
#' @param multi_cl_source # of chlorine source in use, default to 1 (1 or 2).
#' @param multi_nh3_source # of ammonia source in use, default to 1 (1 or 2).
#' @param cl_use_slot If TRUE, uses free_chlorine slot in water instead of chlorine dose. Input for cl2 will be ignored. 
#'                    Default to FALSE, no effect if multi_cl_source is set to 2. 
#' @param nh3_use_slot If TRUE, uses tot_nh3 slot in water instead of ammonia dose. Input for nh3 will be ignored. 
#'                    Default to FALSE, no effect if multi_nh3_source is set to 2.
#' @param time Reaction time (minutes).
#'
#' @seealso \code{\link{chemdose_chloramine}}
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#'
#'example_df <- water_df %>%
#'  mutate(free_chlorine = 5, tot_nh3 = 1) %>%
#'  define_water_chain() %>%
#'  balance_ions_chain() %>%
#'  mutate(
#'       time = 8,
#'       cl2 = 10,
#'       nh3 = 2,
#'       multi_cl_source = 1,
#'       multi_nh3_source = 1
#'   ) %>%
#'   chemdose_chloramine_chain(input_water = "balanced_water")
#'
#'
#'
#'
#'
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_chloramine_chain(input_water = "balanced_water", cl2_dose = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns A data frame containing a water class column with updated chlorine residuals.


chemdose_chloramine_chain <- function(df, input_water = "defined_water", output_water = "chlorinated_water",
                                      time = 0, cl2 = 0, nh3 = 0, 
                                      multi_cl_source = 0, cl_use_slot = FALSE,  
                                      multi_nh3_source = 0, nh3_use_slot = FALSE) {
  ID <- NULL # Quiet RCMD check global variable note

  arguments <- construct_helper(
    df, list("cl2" = cl2, "nh3" = nh3, "time" = time, 
             "multi_cl_source" = multi_cl_source, "multi_nh3_source" = multi_nh3_source),
    list("cl_use_slot" = cl_use_slot, "nh3_use_slot" = nh3_use_slot)
  )

  output <- df %>%
    subset(select = !names(df) %in% c("cl2", "nh3", "time", 
                                      "multi_cl_source", "cl_use_slot",
                                      "multi_nh3_source", "nh3_use_slot")) %>% 
    mutate(
      ID = row_number()
    ) %>%
    left_join(arguments, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        cl2 = cl2,
        nh3 = nh3,
        time = time,
        multi_cl_source = multi_cl_source,
        cl_use_slot = cl_use_slot,
        multi_nh3_source = multi_nh3_source,
        nh3_use_slot = nh3_use_slot
      ),
      chemdose_chloramine
    ))
}




# issues to note
# some values turned out to be 0, set them to 0 for now, could be NA
# unit conversion cl2 (M >- mg/L)
# time, may need to add a check if less than 1 or something ode can't be solved
# breakdown of total_chloramine if present, now ignored



# more examples to add

# example_df <- water_df %>%
#   mutate(free_chlorine = 5, tot_nh3 = 1)  %>%
#   define_water_chain() %>%
#   balance_ions_chain() %>%
#   chemdose_chloramine_chain(input_water = "balanced_water", cl2_dose = 4, time = 8)
# 
# example_df <- water_df %>%
#   mutate(free_chlorine = 10, tot_nh3 = 2)  %>%
#   define_water_chain() %>%
#   balance_ions_chain() %>%
#   mutate(
#     cl2_dose = seq(2, 24, 2),
#     time = 30
#   ) %>%
#   chemdose_chloramine_chain(input_water = "balanced_water")
# 
# example_df <- water_df %>%
#   mutate(free_chlorine = 6, tot_nh3 = 1)  %>%
#   define_water_chain() %>%
#   balance_ions_chain() %>%
#   mutate(time = 8) %>%
#   chemdose_chloramine_chain(
# # input_water = "balanced_water", cl2_dose = 6, treatment = "coag",    cl_type = "chloramine"
#   )




