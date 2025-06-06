#' @title Determine if TOC meets requirements
#' @description This function takes input parameters for raw water including TOC,
#' pH, and alkalinity, and calculates the removal percentage for TOC. It then
#' checks compliance with regulations based on these inputs.
#'
#' @details The function prints the input parameters and the calculated removal
#' percentage for TOC. It checks compliance with regulations considering the raw
#' TOC, alkalinity, and removal percentage. If the conditions are met, it prints
#' "In compliance"; otherwise, it prints "Not in compliance" and stops execution
#' with an error message.
#' @param raw_toc Numeric value representing the raw TOC (mg/L).
#'
#' @param ph Numeric value representing the pH of the water.
#'
#' @param alk Numeric value representing the alkalinity (mg/L as calcium carbonate).
#'
#' @param final_toc Numeric value representing the final TOC (mg/L).
#'
#' @examples
#' toc_regulations(5, 7, 60, 2)
#'
#' @export
#'
#' @returns The function return "In compliance" if the conditions are met,
#' otherwise it stops execution with an error message.
#'
#'

# See link here for regulations https://github.com/BrownandCaldwell/tidywater/issues/328
toc_regulations <- function(water, raw_toc, final_toc) {
  ph <- water@ph
  alk <- water@alk

  # Input parameters for raw water:
  print(paste("Raw TOC (mg/L):", raw_toc))
  print(paste("pH:", ph))
  print(paste("Alkalinity (mg/L as calcium carbonate):", alk))
  print(paste("Final TOC (mg/L):", final_toc))

  #Calculate removal percentage for TOC:
  removal <- (raw_toc - final_toc) / raw_toc * 100
  message("Removal percentage: ", removal)
  
  required_compliance <- NA
  
  #Checking compliance considering inputs:


  if (raw_toc > 2 & raw_toc <= 4) {
    if (alk <= 60) required_compliance <- 35
    else if (alk > 60 & alk <= 120) required_compliance <- 25
    else if (alk > 120) required_compliance <- 15
  } else if (raw_toc > 4 & raw_toc <= 8) {
    if (alk <= 60) required_compliance <- 45
    else if (alk > 60 & alk <= 120) required_compliance <- 35
    else if (alk > 120) required_compliance <- 25
  } else if (raw_toc > 8) {
    if (alk <= 60) required_compliance <- 50
    else if (alk > 60 & alk <= 120) required_compliance <- 40
    else if (alk > 120) required_compliance <- 30
  }
  
  
  if (!is.na(required_compliance) && removal >= required_compliance) {
    return(tibble::tibble(compliance_status = "In Compliance"))
  } else {
    return(tibble::tibble(
      compliance_status = paste0("Only ", round(removal, 1), "% TOC removed, requires minimum ", required_compliance, "% Compliance")
    ))
  }
}

library(tidywater)
library(tibble)

#test the function with raw parameters:
water <- define_water(ph = 8, alk = 44, temp = 20)
toc_regulations(water = water, raw_toc = 5, final_toc = 3)


# devtools::load_all()


 

