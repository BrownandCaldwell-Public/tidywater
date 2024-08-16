# Chlorine/Chloramine Decay Modeling functions
# These functions predict chlorine residual concentration given reaction time 

#' @title Calculate total OCL
#'
#' @description \code{chlordose} calculates the decay of chlorine based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001). Required arguments include an object of class "water"
#' created by \code{\link{define_water}} chlorine/chloramine dose, type, reaction time, and treatment applied (if any).
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including TOC and UVA.
#'
#' @details The function will calculate haloacetic acids (HAA) as HAA5, and total trihalomethanes (TTHM).
#' The function returns a new object of class "water" with predicted chlorine/chloramine residual concentrations.
#' Use \code{summarise_wq} to quickly tabulate the results.
#'
#' @source Ct, raw: U.S. EPA (2001) equation 5-113
#' @source Ct, coagulated/treated: U.S. EPA (2001) equation 5-117
#' @source Ct corrected: U.S. EPA (2001) equation 5-118
#' @source CAt: U.S. EPA (2001) equation 5-120
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2 Applied chlorine or chloramine dose (mg/L as Cl2). Model results are valid for doses between 0.995 and 41.7 mg/L for raw water, 
#' and for doses between 1.11 and 24.7 mg/L for coagulated water.
#' @param time Reaction time (hours). Chlorine decay model results are valid for reaction times between 0.25 and 120 hours.Chloramine decay model
#' does not have a time restriction. 
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default), "coag" for
#' water that has been coagulated or softened.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @examples
#' example_cl2 <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chlordose(cl2 = 2, time = 8)
#' example_cl2 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chlordose(cl2 = 3, time = 168, treatment = "coag", location = "ds")
#'
#' @export
#'
chlordose <- function(water, cl2, time, treatment, cl_type) {
  toc = water@toc
  uv254 = water@uv254
  

  # Handle missing arguments with warnings (not all parameters are needed for all models).
   if (missing(cl2) | missing(time)) {
    stop("Missing value for cl2 or time. Please check the function inputs required to calculate chlorine/chloramine decay")
  }

  if ((cl_type != "chlorine" & cl_type != "chloramine") | missing(cl_type)) {
    stop("cl_type should be 'chlorine' or 'chloramine', or missing value for cl_type. Please check the spelling
         or add missing function inputs to calculate chlorine/chloramine decay.")
  }
  
  
  # chlorine decay model 
  if (cl_type == "chlorine") {
    
    if ((treatment != "raw" & treatment != "coag") | missing(treatment)) {
      stop("For chlorine decay, the treatment type should be 'raw' or 'coag', or missing value for treatment. 
         Please check the spelling or add missing function inputs to calculate chlorine decay.")
     } 
    
    if (is.na(toc) | is.na(uv254)) {
      stop("For chlorine decay, missing value for toc or uv254. Please add missing parameters to define_water.")
    } 
    
    # toc warnings
    if (treatment == "raw" & (toc < 1.2 | toc > 16)) { 
      warning("For chlorine decay estimate, TOC is outside the model bounds of 1.2 <= TOC <= 16 mg/L for raw water")
    }

    if (treatment == "coag" & (toc < 1.0 | toc > 11.1)) {
    warning("For chlorine decay estimate, TOC is outside the model bounds of 1.0 <= TOC <= 11.1 mg/L for coagulated water")
    }

    # uv254 warnings
    if (treatment == "raw" & (uv254 < 0.010 | uv254 > 0.730)) {
      warning("For chlorine decay estimate, uv254 is outside the model bounds of 0.010 <= uv254 <= 0.730 mg/L for raw water.")
    }
  
    if (treatment == "coag" & (uv254 < 0.012 | uv254 > 0.250)) {
      warning("For chlorine decay estimate, uv254 is outside the model bounds of 0.012 <= UV254 <= 0.250 cm-1 for coagulated water.")
    }

    # cl2 warnings
    if (treatment == "raw" & (cl2 < 0.995 | cl2 > 41.7)) {
      warning("For chlorine decay estimate, chlorine is outside the model bounds of 0.995 <= cl2 <= 41.7 mg/L for raw water")
    }
    
    if (treatment == "coag" & (cl2 < 1.11 | cl2 > 24.7)) {
      warning("For chlorine decay estimate, chlorine is outside the model bounds of 1.11 <= cl2 <= 24.7 mg/L for coagulated water")
    }
  
    # time warning
    if (time < 0.25 | time > 120) {
      warning("For chlorine decay estimate, reaction time is outside the model bounds of 0.25 (15minutes) <= time <= 120 hours.")
    }

    # define functions for chlorine decay
    clcoeffs <- tibble(
      treatment = c('raw','coag'),
      a = c(-0.8147, -0.8404),
      b = c(-2.2808, -0.404),
      c = c(-1.2971, -0.9108)
    )
    
    cl_decay <- function(modeled_cl, a, b, cl2, uv254, c, toc, time) {
      a * cl2 * log(cl2/modeled_cl) - b * (cl2/uv254)^c *toc*time + cl2 - modeled_cl
    }
    
    solve_modeled_cl <- function(a, b, cl2, uv254, c, toc, time) {
      uniroot(
       cl_decay,
       c(0.1,100),
       a = a,
       b = b,
       cl2 = cl2,
       uv254 = uv254,
       c = c,
       toc = toc,
       time = time
      )$root
    }

    # estimate chlorine decay based on level of treatment - results in ug/L
    if (treatment == "raw") {
      predicted_cl <- clcoeffs %>%
        filter(treatment == "raw") %>%
        rowwise() %>%
        mutate(modeled_cl = solve_modeled_cl(a, b, cl2, uv254, c, toc, time))
        ungroup()
    } else if (treatment == "coag") {
      predicted_cl <- clcoeffs %>%
        filter(treatment == "coag") %>%
        rowwise() %>%
        mutate(modeled_cl = solve_modeled_cl(a, b, cl2, uv254, c, toc, time))
        ungroup()
    }


    # apply chlorine residual correction, no significant difference in the parameters for different treatment
    corrected_modeled_cl <- cl2 + (predicted_cl - cl2) / 0.85
    water@ocl <- corrected_model_cl

  } 
  
  
    #chloramine decay model
  else if (cl_type == "chloramine") { 
    
    # chloramine decay model stop
    if (is.na(uv254)) {
      stop("For chloramine decay estimate, missing value for uv254 for chloramine decay estimate. 
           Please add missing parameters to define_water.")
    }
 
      cacoeffs <- tibble(
      a = -0.99,
      b = -0.015
    )
    
    # define functions for chloramine decay
    ca_decay <- function(modeled_ca, a, b, cl2, uv254, time) {
      a * cl2 * log(cl2/modeled_ca) - b * uv254 * time + cl2 - modeled_ca
    }
    
    solve_modeled_ca <- function(a, b, cl2, uv254, time) {
      uniroot(
        ca_decay,
        c(0.1,100),
        a = a,
        b = b,
        cl2 = cl2,
        uv254 = uv254,
        time = time
      )$root
    }
  
    # estimate chloramine decay
    predicted_cl <- cacoeffs %>%
    rowwise() %>%
    mutate(modeled_ca = solve_modeled_ca(a, b, cl2, uv254, time))%>%
    ungroup()
  
    water@ocl <- predicted_cl
  }
    
  
  return(water)
  
}

#-------------------------------------
# setwd("C:/Users/PChen/Rscripts/tidywater/R")
# library(tidywater,tidyverse)

# devtools::load_all()
# example_cl2 <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2)) %>%
# chlordose(cl2 = 2, time = 8, treatment = "raw", cl_type = "chlorine")






