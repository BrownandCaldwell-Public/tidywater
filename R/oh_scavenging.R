#' oh scavenger model for reuse 
#' 
#' @title OH scavenger 
#' 
#' @description The backround hydroxyl radical scavenging term is a measure of the degredation rate of non target consituients in a water. The term is defined as the 
#' sum of the reaction rate of the constituient multiplied by the concentration of the constiuitent: sum(k_i * [C]i) where k_i is the reaction rate of a contaminant and [C]i its concentration.
#' Suporting Information in Wang et al. 2019 provides the definition and additional methods for calculating backround scavenging. 
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
#' other K values, with different units
#' 2.90E+00,  Brezonik and Fulkerson-Brekken (1998) Mc^-1s^-1
#' 2.70E+00, Katsoyiannis et al. (2011) Mc^-1s^-1
#' 2.00E+00, Katsoyiannis et al. (2011) Mc^-1s^-1
#' 2.10E+00, Katsoyiannis et al. (2011) Mc^-1s^-1
#' 6.35E+08, Vinge et al (2021) M^-1 s^-1
#' 2.00E+08 Wols et al (2014) - 33 M^-1 s^-1
#' 
#' ATTICUS VERIFY THESE VALUES 
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
#' 
#' @param water source water of TW 'water' object type 
#' @param dose hydrogen peroxide (h2o2) dose applied, defaults to 0 
#' @param kdoc_adjust user input for a different k_DOC rate constant for fitting data, defaults to literature value 
#' 
#' 
#' @examples
#' water1 <- define_water(ph=7, doc=3, alk = 70, no3 = 4)
#' h2o2_dose<-20
#' 
#' backround_scavenging <- oh_scavenger(water1, h2o2_dose)
#' 
#' backround_SC_adjusted <- oh_scavenger(water1, h2o2_dose, 48065)



oh_scavenger <- function(water, dose = 0, kdoc_adjust = 2.5e4) {

  validate_water(water, c("ph"))

  if (is.na(water@dic) & is.na(water@alk)) {
    dic <- 0
    # assume that DIC is 0 if not specified to calculate carbonate balances
    # doing this because nathan's original script calculated the balance like this
    # it may be a little useless because TW has built in, far better, carb bal calcs
    # FEEDBACK ON THIS: nathan used DIC measurement to calculate carbonate balance in his scavenging calculator, which I kept in the code to verify our calculator
    # however, is this just a cumbersome feature or good? Not sure. 

    cli::cli_warn("No alk or dic provided. Assuming that dic = 0 to calculate carbonate balance.")
  } else {
    dic <- water@dic
  }
  # List of each constituent k value to access easier and for clarity.
  k <- list(
    DOC = 2.5e4, #L/mg s-1
    # 48065
    HCO3 = 8.50E+06, # M-1 s-1
    CO3 = 3.90E+08, # M-1 s-1
    NO2 = 1.00E+10, # M-1 s-1
    NO3 = 1.00E+05, # M-1 s-1
    H2O2 = 2.70E+07 # M-1 s-1
  )

  # pkas for calculating carbonate balance from DIC
  pka_h2co3 <- 6.35
  pka_hco3 <- 10.33

  if (kdoc_adjust != 2.5e4) {
    k$DOC <- kdoc_adjust
  }

  k_S <- list(
    DOC = k$DOC * water@toc, # s-1
    NO3 = k$NO3 * water@no3, # s-1
    NO2 = k$NO2 * water@no2, # s-1
    H2O2 = k$H2O2 * dose, # s-1
    HCO3 = 0, # s-1
    CO3 = 0 # s-1
  )

  # FEEDBACK: should this still work of alk = 0, feels like it should. 
  # use pre-calculated carbonate balance in define_water to calculate ks vals for co3 and hco3 
  if ((!is.na(water@alk) & water@alk != 0)) {
    k_S$HCO3 <- water@hco3 * k$HCO3 # s-1
    k_S$CO3 <- water@co3 * k$CO3 # s-1
    # use DIC carbonate balance to calculate co3
  } else {
    hco3 <- (dic * (10^-water@ph) * (10^-pka_h2co3)) /
      ((10^-water@ph) * (10^-water@ph) + (10^-water@ph) * 10^(-pka_h2co3) + (10^(-pka_h2co3)) * 10^(-pka_hco3))
    co3 <- (dic * (10^-pka_h2co3) * (10^-pka_hco3)) /
      ((10^-water@ph) * (10^-water@ph) + (10^-water@ph) * 10^(-pka_h2co3) + (10^(-pka_h2co3)) * 10^(-pka_hco3))
    k_S$HCO3 <- hco3 * k$HCO3 # s-1
    k_S$CO3 <- co3 * k$CO3 # s-1
  }

  # turn each NA value to 0 because NA's will mess up execution and 0's just won't contribute anything.
k_S <- lapply(k_S, function(x) {
    x[is.na(x)] <- 0
    return(x)
  })

  # sum all of the scavenging terms
  SC <- sum(unlist(k_S))
  return(SC)
}
