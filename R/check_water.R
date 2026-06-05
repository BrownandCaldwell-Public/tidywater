# Behavior: 
# returns a tibble with cols for CHECK, VALUE, and FLAG:

# Ion Balance RPD -- cat vs an RPD, flagg if > 10%
# TDS check -- measured/estimated TDS vs. sum-of-ions TDS, flagged if diference is large 
# Ionic strength -- flag if major ions are missing 
# Applicable models

# Migrate warnings from define_water 
# - hardness RPD >10% 
# - chlorng and ammonia both present 
# - major ions missing (ionic strength warning) 


check_water <- function(water) {

#vallidate water!!
  
 if (!is(water, "water")){
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
  ions<-cations-anions

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
    flag = "Unresonable PD > 10%"
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
    value ="There is no total Cl",
    flag = "Chloramines will not be formed because Cl is missing"
    )
 }
  else if ((water@tot_nh3 > 0) & !(water@free_chlorine > 0 | water@combined_chlorine > 0)){
    results<-add_row(results,
    check = "Chloramine formation",
    value ="There is no total Nitrogen",
    flag = "Chloramines will not be formed because N is missing"
    )
 }
  else {
    results<-tibble::add_row(results,
    check = "Chloramine formation",
    value ="Total nitrogen and Cl missing",
    flag = "Chloramines will not be formed because N and Cl are missing"
    )
  }

#  aplicable models

#   validate_water_bool <- function(water, slots){
#     if (any(sapply(slots, function(sl) is.na(methods::slot(water, sl))))) {
#    return(FALSE)
#   }
# else{
#       return(TRUE)
#     }

#   }
  
  
  
# each nessacary paramater for each function 
models <- list(
 "chemdose_ph, decarbonate_ph, dissolve_cu, opensys_ph, chemdose_chloramine, calculate_corrosion" = c("ph", "alk"),
  "dissolve_pb"                          = c("ph", "alk", "is"),
  "chemdose_toc, chemdose_dbp"           = c("ph", "doc", "uv254"),
  "gac_toc, gacrun_toc, gacbv_toc"      = c("ph", "doc"),
  "calculate_corrosion"           = c("cl", "so4"),
  "calculate_corrosion"   = c("cl", "so4", "alk"),
  "chemdose_chlordecay"                  = c("toc", "uv254"),
  "biofilter_toc, pac_toc"              = c("doc"),
  "ozonate_bromate"                      = c("br", "ph")
)

  
  # take each paramater accociated with a group of functions and see if those paramaters are !na  in the input water
  #filters out function groups that have na paramaters in the water
final_models <- Filter(function(v) all(!is.na(sapply(v, function(sl) methods::slot(water, sl)))), models)  
  


  if(!length(final_models)==0){
  results<-tibble::add_row(results,
    check = "Aplicable models",
    value =paste(names(final_models), collapse = "; "),
    flag = NA_character_
  )
}
  else{
   results<-tibble::add_row(results,
    check = "Aplicable models",
    value = "No aplicable models...",
    flag = NA_character_
  ) 
  }
  
  
  return(results)
}
