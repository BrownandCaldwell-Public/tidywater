#' OH- scavenger model for reuse 
#' 
#' @title OH- scavenging function
#' 
#' @description The background hydroxyl radical scavenging term is a measure of the degradation rate of non-target constituents in a water. The term is defined as the 
#' sum of the reaction rate of the constituent multiplied by the concentration of the constituent: sum(k_i * [C]i) where k_i is the reaction rate of a contaminant and [C]i its concentration.
#' Supporting Information in Wang et al. 2019 provides the definition and additional methods for calculating background scavenging. 
#' 
#' @details Scavenging rates used in the code: 
#'   k(OH,DOC/TOC)	2.50E+04	L/mg s-1	Larson et al. 1988
#'   k(OH,HCO3-)	8.50E+06	M-1 s-1	Buxton et al.,1988
#'   k(OH,CO3-2)	3.90E+08	M-1 s-1	Buxton et al.,1988
#'   k(OH,NO2-)	1.00E+10 M-1 s-1 Coddington et al. 1999; Vinge et al 2020 
#'   k(OH,NO3-)	1.00E+05	M-1 s-1	Keen et al., 2012
#'   k(OH,H2O2)	2.70E+07	M-1 s-1	Buxton et al.,1988
#' 
#' Other reported k_doc values from literature
#' 
#' other K values, with different units -- are these all for k_DOC?
#' 2.30E+04,  Brezonik and Fulkerson-Brekken (1998) Mc^-1s^-1
#' 2.70E+04, Katsoyiannis et al. (2011) Mc^-1s^-1
#' 2.00E+04, Katsoyiannis et al. (2011) Mc^-1s^-1
#' 2.10E+04, Katsoyiannis et al. (2011) Mc^-1s^-1
#' 3.50E+04, Katsoyiannis et al. (2011) Mc^-1s^-1
#' 6.35E+08, Vinge et al (2021) M^-1 s^-1
#' 2.00E+08 Wols et al (2014) - 33 M^-1 s^-1
#'  
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source Katsoyiannis et al. (2011)
#' @source Vinge et al (2021)
#' @source Wols et al (2014)
#' @source Brezonik and Fulkerson-Brekken (1998)
#' @source Buxton et al.,1988
#' @source Coddington et al. 1999
#' @source Vinge et al 2020 
#' @source Larson et al. 1988
#' 
#' @param water Source water of tidywater 'water' object type 
#' @param dose Hydrogen peroxide (h2o2) dose applied in mg/L, defaults to 0 
#' @param kdoc_adjust Optional input for a different k_DOC rate constant for fitting data, defaults to 2.5e4 L/mg s-1
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

  # List of each constituent k value to access easier and for clarity.
  k <- list(
    doc = kdoc_adjust, #L/mg s-1
    # 48065
    hco3 = 8.50E+06, # M-1 s-1
    co3 = 3.90E+08, # M-1 s-1
    no2 = 1.00E+10, # M-1 s-1
    no3 = 1.00E+05, # M-1 s-1
    h2o2 = 2.70E+07 # M-1 s-1
  )

  k_S <- list(
    doc = k$doc * water@toc, # s-1  # check that we can use toc here instead of doc?
    no3 = k$no3 * water@no3, # s-1
    no2 = k$no2 * water@no2, # s-1
    h2o2 = k$h2o2 * dose, # s-1
    hco3 = k$hco3 * water@hco3, # s-1
    co3 = k$co3 * water@co3 # s-1
  )

  # turn each NA value to 0 because NA's will mess up execution and 0's just won't contribute anything.
  k_S <- lapply(k_S, function(x) {
    x[is.na(x)] <- 0
    return(x)
  })

  # sum all of the scavenging terms
  SC <- sum(unlist(k_S))
  return(SC)
}
