



define_chemical<-function(

  formula,
  dose = 0,
  strength = 100, 
  dose_unit = 'mg/L as chemical', 
  hydration = NA_real_,
  cost = NA_real_

) {

  # If things are missing, error. If missing formula, abort. 
  if (missing(formula)){
    cli::cli_abort("No formula provided, no formula no chemical.")
  }
  if (missing(dose)){
    cli::cli_warn("No dose provided, defaulting to zero.")
  }
  if (missing(strength)){
    cli::cli_warn("No strength provided, defaulting to 100% strength")
  }
  if (missing(dose_unit)){
    cli::cli_warn("No dose_unit provided, defaulting to mg/L as chemical.")
  }

  
chemical <- methods::new(
  "chemical",
  formula = formula,
  dose = dose,
  strength = strength,
  dose_unit=dose_unit,
  hydration=hydration,
  cost = cost
)

 return(chemical) 
  
}