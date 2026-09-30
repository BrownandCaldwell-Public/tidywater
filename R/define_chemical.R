#'@title Create a chemical class object given a chemical's paramaters
#'
#' @description This function matches user input chemicals with a database of common chemicals, allowing the user to define and use
#' a greater variety of chemical paramaters in their calculations.  
#' 
#' @details Coming soon to theaters near you!
#'
#' @param formula Associated chemical formula
#' @param dose Dosage of the chemical 
#' @param strength from 100% to 1%, measure of the ammount of chemical to its solution
#' @param dose_unit Units the chemical is in, defaults to mg/L as chemical 
#' @param hydration Waters of hydration
#' @param cost How much the chemical costs 
#' @param solids_formula formula for solids 
#' 
#' @examples 
#' 
#' naoh_50p <- define_chemical(formula="naoh",dose=12, strength = 50, dose_unit = "M")
#' 
#' @export
#' 
#' @return An S4 chemical class object with user defined paramaters. 
#'   



define_chemical<-function(

  formula,
  dose = 0,
  strength = 100, 
  dose_unit = 'mg/L as chemical', 
  hydration = NA_real_,
  cost = NA_real_,
  solids_formula = NA_character_
) {

  # If things are missing, error. If missing formula, abort. 
  if (missing(formula)){
    cli::cli_abort("No formula provided, no formula no chemical.")
  }
  if (missing(dose)){
    tidywater_warn("No dose provided, defaulting to zero.")
  }
  if (missing(strength)){
    tidywater_warn("No strength provided, defaulting to 100% strength")
  }
  if (missing(dose_unit)){
    tidywater_warn("No dose_unit provided, defaulting to mg/L as chemical.")
  }

  
chemical <- methods::new(
  "chemical",
  formula = formula,
  dose = dose,
  strength = strength,
  dose_unit=dose_unit,
  hydration=hydration,
  cost = cost,
  solids_formula = solids_formula
)

 return(chemical) 
  
}
