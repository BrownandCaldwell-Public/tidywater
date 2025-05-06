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


 
