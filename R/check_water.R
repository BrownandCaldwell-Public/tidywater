#' @title check a water object for common issues and applicable models
#' 
#' @description This function takes a previously defines water class input and checks common mismatch issues with water quality. 
#' It returns a table wwith the check, the value, and a flag if there is an issue. 
#' It will also check to see what other tidywater models are applicable to the input water unless the user specifies to skip that step.
#' 
#' @details This function serves as a heuristic check for users to check common issues with water data.
#' 
#' @param water a water class object
#' @param skip_models a boolean to specify whether to skip the check for applicable models, defaults to FALSE
#' 
#' @examples 
#' 
#' water1<-define_water(ph=7, alk = 70)
#' check_water(water1)
#' 
#' water2<-define_water( ph=7, alk = 70, ca = 20, na = 20, cl = 20, so4 = 20)
#' check_water(water2, skip_models = TRUE)
#' 
#' 
#' @export
#' 
#' @return a tibble with the check, value, and flag for any issues found. If skip_models = FALSE, also includes a check for applicable models with tidywater functions.


check_water <- function(water, skip_models = FALSE) {

#vallidate water!!
  
 if (!methods::is(water, "water")){
   stop("input object needs to be a water class")
 }


# init tibble
  results <- tibble::tibble(
    check = character(),
    value = character(),
    flag = character()
  )
#ion bal RPD 
  
   cations <- sum(
    water@na, 
    2 * water@ca, 
    2 * water@mg, 
    water@k, 
    water@h, 
    na.rm = TRUE)
  
  anions <- sum(
    water@cl,
    2 * water@so4,
    water@hco3,
    2 * water@co3,
    water@h2po4,
    2 * water@hpo4,
    3 * water@po4,
    water@oh,
    water@ocl,
    na.rm = TRUE
  )

  PD<- abs(2*(anions-cations)/(anions+cations))*100

  if ((!is.na(water@tot_hard) & !is.na(water@ca) & !is.na(water@mg)) &
    (water@tot_hard != 0 & water@ca != 0 & water@mg != 0)) {
  check_tot_hard <- abs(water@tot_hard - calculate_hardness(water@ca, water@mg)) /
    mean(c(water@tot_hard, calculate_hardness(water@ca, water@mg)))
  results <- tibble::add_row(results,
    check = "Total hardness check",
    value = paste0(round(check_tot_hard * 100, 1), "%"),
    flag  = if (check_tot_hard > 0.10) "Total hardness does not match calculated hardness from Ca and Mg" else "none"
  )
} else {
  results <- tibble::add_row(results,
    check = "Total hardness check",
    value = NA_character_,
    flag  = "tot_hard, Ca, or Mg missing."
  )
}



  # should this be >= or stay as >
  if (is.na(PD)){
    results<-tibble::add_row(results,
    check = "Ion Balance Reasonable Percent Difference",
    value =NA_character_,
    flag = "Ions are missing, cannot compare to sum of ions"
    )
  }
  else if (PD<=10){
    results<-tibble::add_row(results,
    check = "Ion Balance Reasonable Percent Difference",
    value =paste0(round(PD, 1),"%"),
    flag = "none"
    )

  }
  else{
    results<-tibble::add_row(results,
    check = "Ion Balance Reasonable Percent Difference",
    value =paste0(round(PD, 1),"%"),
    flag = "Unreasonable PD > 10%"
    )
  }

# TDS check I'm essentially doing the same thing as before 
  ions<-cations+anions

# re use same PD varible (maybe issue if tds is NA?)
  PD <- abs(2*(ions-water@tds)/(ions+water@tds))

  if (is.na(PD)){
    results<-tibble::add_row(results,
    check = "TDS check",
    value =NA_character_,
    flag = "TDS missing, cannot compare to sum of ions"
    )
  }
  else if (PD<=10){
    results<-tibble::add_row(results,
    check = "TDS check",
    value =paste0(round(PD, 1),"%"),
    flag = "none"
    )

  }
  else{
    results<-tibble::add_row(results,
    check = "TDS check",
    value =paste0(round(PD, 1),"%"),
    flag = "Unreasonable PD > 10%"
    )
  }
  

  # Ionic strength major ions 

 if ((!is.na(water@ca) | !is.na(water@na)) & (!is.na(water@cl) | !is.na(water@so4))){
    results<-tibble::add_row(results,
    check = "Ionic strength test",
    value =NA_character_,
    flag = "none"
    )
 }
  else if (is.na(water@ca)&is.na(water@na)){
    results<-tibble::add_row(results,
    check = "Ionic strength test",
    value ="Cations Ca and Na missing",
    flag = "Missing major ions"
    )
 }
  else if (is.na(water@cl)&is.na(water@so4)){
    results<-tibble::add_row(results,
    check = "Ionic strength test",
    value ="Anions Cl and SO4 missing",
    flag = "Missing major ions"
    )
 }
  else {
  results <- tibble::add_row(results,
    check = "Ionic strength test",
    value = "Ca, Na, Cl, and SO4 all missing",
    flag = "Missing major ions"
  )
}

  # Chloramine check
  
  if (water@tot_nh3 > 0 & (water@free_chlorine > 0 | water@combined_chlorine > 0)) {
    results<-tibble::add_row(results,
    check = "Chloramine formation",
    value ="Cl and NH3 both present",
    flag = "Chloramines may be formed, use chemdose_chloramine for breakpoint calcs"
    )
 }
  else if (!(water@tot_nh3 > 0) & (water@free_chlorine > 0 | water@combined_chlorine > 0)){
    results<-tibble::add_row(results,
    check = "Chloramine formation",
    value ="There is no total Nitrogen",
    flag = "Chloramines will not be formed because N is missing"
    )
 }
  else if ((water@tot_nh3 > 0) & !(water@free_chlorine > 0 | water@combined_chlorine > 0)){
    results<-tibble::add_row(results,
    check = "Chloramine formation",
    value ="There is no total Cl",
    flag = "Chloramines will not be formed because Cl is missing"
    )
 }
  else {
    results<-tibble::add_row(results,
    check = "Chloramine formation",
    value ="Total nitrogen and Cl missing",
    flag = "Chloramines will not be formed because N and Cl are missing"
    )
  }

#  applicable  models

#   validate_water_bool <- function(water, slots){
#     if (any(sapply(slots, function(sl) is.na(methods::slot(water, sl))))) {
#    return(FALSE)
#   }
# else{
#       return(TRUE)
#     }

#   }
  
  
  if (!skip_models){
# each necessary paramater for each function 
models <- list(
 "chemdose_ph, decarbonate_ph, dissolve_cu, opensys_ph, chemdose_chloramine" = c("ph", "alk"),
  "dissolve_pb"                          = c("ph", "alk", "is"),
  "chemdose_toc, chemdose_dbp"           = c("ph", "doc", "uv254"),
  "gac_toc, gacrun_toc, gacbv_toc"      = c("ph", "doc"),
  "chemdose_chlordecay"                  = c("toc", "uv254"),
  "biofilter_toc, pac_toc"              = c("doc"),
  "ozonate_bromate"                      = c("br", "ph")
)

  
  # take each paramater associated with a group of functions and see if those paramaters are !na  in the input water
  #filters out function groups that have na paramaters in the water
final_models <- Filter(function(v) all(!is.na(sapply(v, function(sl) methods::slot(water, sl)))), models)  
  


  if(!length(final_models)==0){
  results<-tibble::add_row(results,
    check = "Applicable models",
    value =paste(names(final_models), collapse = "; "),
    flag = NA_character_
  )
}
  else{
   results<-tibble::add_row(results,
    check = "Applicable models",
    value = "No applicable models...",
    flag = NA_character_
  ) 
  }
}
  
  return(results)
}
