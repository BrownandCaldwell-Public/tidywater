#' @title OH radical scavenging function
#'
#' @description The background hydroxyl radical scavenging term is a measure of the degradation rate of non-target constituents in a water. The term is defined as the 
#' sum of the reaction rate of the constituent multiplied by the concentration of the constituent: `sum(k_i * [C]i)` where `k_i` is the reaction rate of a contaminant and `[C]i` its concentration.
#' Supporting Information in Wang et al. 2019 provides the definition and additional methods for calculating background scavenging. 
#' 
#' @details Rate constants used in the code:
#' * DOC: 2.5e4 L/mg C/s (Larson and Zepp 1988; 3.0e8 M_C^-1 s^-1 divided by 12,011 mg C/mol)
#' * HCO3-: 8.5e6 M^-1 s^-1 (Buxton et al. 1988)
#' * CO3 2-: 3.9e8 M^-1 s^-1 (Buxton et al. 1988)
#' * NO2-: 1.0e10 M^-1 s^-1 (Coddington et al. 1999; Vinge et al. 2020)
#' * NO3-: 1.0e5 M^-1 s^-1 (Buxton et al. 1988)
#' * H2O2: 2.7e7 M^-1 s^-1 (Buxton et al. 1988)
#'
#' Other reported DOC rate constants, for use as `kdoc_adjust`. Sources report these per mole of carbon
#' (M_C^-1 s^-1); they are converted to L/mg C/s by dividing by 12,011 mg C/mol. Brezonik and
#' Katsoyiannis values are as compiled in Wols and Hofman-Caris (2012), Table 2.
#' * 2.41e4 (2.9e8 M_C^-1 s^-1): lake and river water, Brezonik and Fulkerson-Brekken (1998)
#' * 2.25e4 (2.7e8 M_C^-1 s^-1): Lake Zurich, Katsoyiannis et al. (2011)
#' * 1.67e4 (2.0e8 M_C^-1 s^-1): Lake Jonsvatnet, Katsoyiannis et al. (2011)
#' * 1.75e4 (2.1e8 M_C^-1 s^-1): Lake Greifensee, Katsoyiannis et al. (2011)
#' * 2.91e4 (3.5e8 M_C^-1 s^-1): effluent organic matter, Katsoyiannis et al. (2011)
#' * 1.67e4 (2.0e8 M_C^-1 s^-1): model value, Wols et al. (2014), Table 3
#' * 5.29e4 (6.35e8 M_C^-1 s^-1): midpoint of the 3.5e8 to 9.2e8 range cited by Vinge et al. (2021)
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source Brezonik and Fulkerson-Brekken (1998)
#' @source Buxton et al. (1988)
#' @source Coddington et al. (1999)
#' @source Katsoyiannis et al. (2011)
#' @source Larson and Zepp (1988)
#' @source Vinge et al. (2020)
#' @source Vinge et al. (2021)
#' @source Wang et al. (2019)
#' @source Wols and Hofman-Caris (2012)
#' @source Wols et al. (2014)
#' 
#' @param water Source water of tidywater 'water' object type 
#' @param dose Hydrogen peroxide (h2o2) dose applied in mg/L, defaults to 0 
#' @param kdoc_adjust Optional input for a different k_DOC rate constant for fitting data, defaults to 2.5e4 L/mg C/s. See Details for other reported values.
#' 
#' @examples
#' water1 <- define_water(ph=7, doc=3, alk = 70, no3 = 4)
#' h2o2_dose<-20
#' 
#' background_scavenging <- scavenge_oh(water1, h2o2_dose)
#' 
#' background_SC_adjusted <- scavenge_oh(water1, h2o2_dose, 48065)
#'
#' @export
#' 
#' @returns `scavenge_oh` returns the total background hydroxyl radical scavenging term in units of s^-1.
#' 

scavenge_oh <- function(water, dose = 0, kdoc_adjust = 2.5e4) {

  # Should still work with DI water, so will not check any specific water slots
  validate_water(water, c())
  dose_M <- convert_units(dose, "h2o2", "mg/L", "M")

  # List of each constituent k value to access easier and for clarity.
  k <- list(
    doc = kdoc_adjust, #L/mg s-1
    hco3 = 8.50E+06, # M-1 s-1
    co3 = 3.90E+08, # M-1 s-1
    no2 = 1.00E+10, # M-1 s-1
    no3 = 1.00E+05, # M-1 s-1
    h2o2 = 2.70E+07 # M-1 s-1
  )

  k_S <- list(
    doc = k$doc * water@doc, # s-1
    no3 = k$no3 * water@no3, # s-1
    no2 = k$no2 * water@no2, # s-1
    h2o2 = k$h2o2 * dose_M, # s-1
    hco3 = k$hco3 * water@hco3, # s-1
    co3 = k$co3 * water@co3 # s-1
  )

  # Warn when a major scavenger is missing. no2 and no3 are usually NA and contribute little, so they are not reported.
  missing_slots <- intersect(names(k_S)[vapply(k_S, anyNA, logical(1))], c("doc", "hco3", "co3"))
  if (length(missing_slots) > 0) {
    tidywater_warn("Missing {.val {missing_slots}} treated as 0 in the OH radical scavenging term.")
  }

  # turn each NA value to 0 because NA's will mess up execution and 0's just won't contribute anything.
  k_S <- lapply(k_S, function(x) {
    x[is.na(x)] <- 0
    return(x)
  })

  # sum all of the scavenging terms
  SC <- Reduce(`+`, k_S)
  return(SC)
}
