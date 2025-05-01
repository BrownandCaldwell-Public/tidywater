#' @title Determine if TOC meets requirements
#' @description This function takes a vector of waters defined by [define_water]
#' and a vector of ratios and outputs a new object with updated ions and pH.
#' For a single blend use `blend_waters`; for a dataframe use `blend_waters_chain`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#'
#' @details
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.#'
#'
#' @param waters Vector of source waters created by [define_water]. For `chain` function, this can include
#' quoted column names and/or existing single water objects unquoted.
#' @param ratios Vector of ratios in the same order as waters. (Blend ratios must sum to 1). For `chain` function,
#' this can also be a list of quoted column names.
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' water1 <- define_water(7, 20, 50)
#' water2 <- define_water(7.5, 20, 100, tot_nh3 = 2)
#' blend_waters(c(water1, water2), c(.4, .6))
#'
#' @export
#'
#' @returns `blend_waters` returns a water class object with blended water quality parameters.

# See link here for regulations https://github.com/BrownandCaldwell/tidywater/issues/328
toc_regulations <- function(raw_toc, ph, alk, final_toc) {
  
  # Input parameters for raw water:
  print(paste("Raw TOC (mg/L):", raw_toc))
  print(paste("pH:", ph))
  print(paste("Alkalinity (mg/L as calcium carbonate):", alk))
  print(paste("Final TOC (mg/L):", final_toc))
  
  #Calculate removal percentage for TOC:
  removal <- (raw_toc- final_toc) / raw_toc *100
  print(paste("Removal percentage:", removal))

  #Checking compliance considering inputs:
  
if(raw_toc >2 & raw_toc <4 & alk <= 60 & removal >=35) {

print("In compliance")
  return()

} else if (raw_toc >4 & raw_toc <8 & alk <=60 & removal>=45) {
  
  print("In compliance")
  return()
  
}
  else if(raw_toc >8 & alk <=60 & removal >=50) {
    
    print("In compliance")
    return()
  }

  else if(raw_toc >2 & raw_toc <4 & alk >60 & alk <=120 & removal >=25) {
    
    print("In compliance")
   return()
  }
    
  else if(raw_toc >4 & raw_toc <8 & alk >60 & alk <=120 & removal >=35){
    print("In compliance")
    return()
  }
  
  else if(raw_toc >8 & alk >60 & alk <=120 & removal >=40) {
    
    print("In compliance")
    return()
    
  }
  else if(raw_toc >2 & raw_toc <4 & alk >120 & removal >=15) {
    print("In compliance")
    return()
    
  }
  else if(raw_toc >4 & raw_toc <8 & alk >120 & removal >=25) {
    print("In compliance")
    return()
  }
  
  else if(raw_toc >8 & alk >120 & removal >=30) {
    print("In compliance")
   return()

  }
  
  else {
    print("Not in compliance")
    stop("Based on raw water TOC and Alkalinity values, you have not removed the required amount of TOC")
  }

}

#test the function with raw water parameters
toc_regulations(5, 7, 60, 2)



