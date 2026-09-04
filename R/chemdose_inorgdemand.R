#'@title Inorganic Oxidant Demand
#'
#' @description Calculates Cl residual from inorganic scavengers present in the input water object: Mg, Fe, and Ammonia. The output water will have updated `free_chlorine` slot
#' to reflect the chlorine residual after accounting for the inorganic demand. The `mn`, `fe`, and `tot_nh3` slots will be set to 0, as they are assumed to be fully oxidized by the chlorine dose.
#' Other products of the oxidation reactions will not be tracked or updated in the output water object. 
#' 
#' The inorganic demand of ammonia is calculated assuming all ammonia is converted to nitrogen gas, not considering the conversion of ammonia to nitrate. Additionally, use `chemdose_chloramine`  
#' if interested in more precise breakpoint chlorination calculations.
#'
#' @param water Input water object with mn, fe, and tot_nh3 slots defined.
#' @param cl_dose Chlorine dose in mg/L Cl2.
#'
#' @examples
#'
#' water1<-define_water(ph = 7, alk = 70, mn = 1, fe = 2, tot_nh3 = 1)
#' cl_residual <- chemdose_inorgdemand(water1, 20)
#'
#' @export
#' 
#' @returns `chemdose_inorgdemand` returns a water class object with updated free_chlorine slot that reflects the chlorine residual.
#'

chemdose_inorgdemand <- function(water, cl_dose) {
  validate_water(water, c("mn", "fe", "tot_nh3"))

    # convert each species to mg/L
    mn  <- convert_units(water@mn, "mn", "M", "mg/L")
    fe  <- convert_units(water@fe, "fe", "M", "mg/L")
    nh3 <- convert_units(water@tot_nh3, "n", "M", "mg/L") # tot nh3 stored as N 

    mn_demands <- mn * 1.29
    fe_demands <- fe * 0.64
    nh3_demands <- nh3 * 7.6
    residual <- cl_dose - (mn_demands + fe_demands + nh3_demands)
  
    if (residual < 0) {
      cli::cli_abort("There is too much oxidant in the water, all available Cl will be used")
    } else {
      output_water <- water
      output_water@free_chlorine <- convert_units(residual, "cl2")
      output_water@mn <- 0 
      output_water@fe <- 0 
      output_water@tot_nh3 <- 0

      return(output_water)
    }
}
