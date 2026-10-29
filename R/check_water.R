#' @title check a water object for common issues and applicable models
#' 
#' @description This function takes a previously defined water class input and checks common mismatch issues with water quality. 
#' It returns a list with the check, the value, and a flag if there is an issue. 
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

#validate water!!
  
 if (!methods::is(water, "water")){
   cli::cli_abort("Input object needs to be a water class (an S4 object)")
 }


# init list for check result
  results <- list()



  #total hardness
 hard_calc <- calculate_hardness(
  convert_units(water@ca, "ca", "M", "mg/L"),
  convert_units(water@mg, "mg", "M", "mg/L")
)

  if ((!is.na(water@tot_hard) & !is.na(water@ca) & !is.na(water@mg)) &
    (water@tot_hard != 0 & water@ca != 0 & water@mg != 0)) {
  check_tot_hard <- abs(water@tot_hard - hard_calc) /
    mean(c(water@tot_hard, hard_calc))
    if (check_tot_hard > 0.10) {
      results<-append(results,
        ("User-entered total hardness differs >10% from calculated hardness.")
       )
    }
  } else {
  results <-append(results,
    ("Total hardness check: tot_hard, Ca, or Mg missing.")
  )
}



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

  IONS_PD<- abs(2*(anions-cations)/(anions+cations))*100
  
  
  if (is.na(IONS_PD)){
    results<-append(results,
    list(
     paste0("Ion Balance Reasonable Percent Difference: Ions are missing, cannot compare to sum of ions.")
    ))
  }
  else if (IONS_PD>=10){
    results<-append(results,
    list(
     paste0("Ion Balance Reasonable Percent Difference: ", round(IONS_PD, 1), "%.", " Unreasonable PD > 10%.")
    ))
  }

# TDS check, essentially doing the same thing as before 
  
  # unit convrsions!
  ions_mgL <- sum(
  convert_units(water@na,  "na",  "M", "mg/L"),
  convert_units(water@cl,  "cl",  "M", "mg/L"),
  convert_units(water@ca,  "ca",  "M", "mg/L"),
  convert_units(water@mg,  "mg",  "M", "mg/L"),
  convert_units(water@k,   "k",   "M", "mg/L"),
  convert_units(water@so4, "so4", "M", "mg/L"),
  na.rm = TRUE
)

  TDS_PD <- abs(2 * (ions_mgL - water@tds) / (ions_mgL + water@tds)) * 100

  if (is.na(TDS_PD)){
    results<-append(results,
      paste0("TDS check: TDS or ions missing, cannot compare to sum of ions.")
    )
  }
  else if (TDS_PD >=10){
    results<-append(results,
      paste0("TDS check: ", round(TDS_PD, 1), "%.", " Unreasonable PD > 10%.")
    )
  }
 
  # Chloramine check
  

  if ((water@tot_nh3 > 0 | is.na(water@tot_nh3)) & ((water@free_chlorine > 0 | is.na(water@free_chlorine)) | 
    (water@combined_chlorine > 0 | is.na(water@combined_chlorine))) ){
    results<-append(results,
    list(
      paste0("Chloramine formation: Cl and NH3 both present. Chloramines may be formed, 
      use chemdose_chloramine for breakpoint calcs.")
    ))
 }
  
# ionic strength check
  
  if (is.na(water@is)){
    results<-append(results,
    list(
      paste0("Ionic strength: IS missing.")
    ))
  }


  # count issues before the applicable models message is added
  n_issues <- length(results)

#  applicable  models

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
  # filters out function groups that have na paramaters in the water
final_models <- Filter(function(v) all(!is.na(sapply(v, function(sl) methods::slot(water, sl)))), models)  
  


  if(length(final_models)>0){
  results<-append(results,
    list(
      paste0("Applicable models: ", paste(names(final_models), collapse = "; "),"." )
    )
  )
}
  else{
   results<-append(results,
    list(
      paste0("No applicable models...")
    )
  ) 
  }
  }
  if (n_issues == 0){
    results<-append(results, paste0("No issues found with water input."))
  }
  return(results)
}
