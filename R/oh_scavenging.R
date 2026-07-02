# purpose of this function is to take in a water and use existing k values to get an estimated oh scavenging rate
# should be in units of s^-1
#
# so far this has all been taken from Syd's arcadia sheet

oh_scavenging <- function(water,kdoc ="standard") {
  # K vals from syd's arcadia sheet
  # k(OH,DOC/TOC)	2.50E+04	L/mg s-1	Larson et al. 1988
  # k(OH,DOC/TOC) SRDOM	1.86E+04	L/mg s-1	Westerhoff et al. 2007
  # k(OH,HCO3-)	8.50E+06	M-1 s-1	Buxton et al.,1988
  # k(OH,CO3-2)	3.90E+08	M-1 s-1	Buxton et al.,1988
  # k(OH,NO2-)	1.00E+09	M-1 s-1	Buxton et al.,1988
  # k(OH,NO3-)	1.00E+05	M-1 s-1	Keen et al., 2012
  # k(OH,H2O2)	2.70E+07	M-1 s-1	Buxton et al.,1988

  uvabs254 <- water@uv254

 validate_water(water, c("toc", "alk", "no3", "ph", "uv254"))

  # avaraging k from literature vals for DOC
  DOC_avg <- mean(c(
    2.50E+04, # arson et al. 1988 L/mg s^-1
    # same that the scavenging calc uses 

    2.94E+08, #found by BC L/mg s^-1

   # 3.00E+08, #Larson and Zepp (1988) Mc^-1s^-1

    # 2.90E+00, # Brezonik and Fulkerson-Brekken (1998) Mc^-1s^-1
    # 2.70E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 2.00E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 2.10E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 6.35E+08, #Vinge et al (2021) M^-1 s^-1
    # 2.00E+08 #Wols et al (2014) - 33 M^-1 s^-1
  ))
# not sure how to deal with different units... maybe 3 different avareages??


  k <- list(
    DOC = if (kdoc=="standard") 2.5e4 else DOC_avg, # L/mg s-1
    DOC_SRDOM = 1.86E+04, # L/mg s-1
    HCO3 = 8.50E+06, # M-1 s-1
    CO3 = 3.90E+08, # M-1 s-1
    #NO2 = 1.00E+09, # M-1 s-1
    NO3 = 1.00E+05, # M-1 s-1
    H2O2 = 2.70E+07 # M-1 s-1
  )

  k_S_DOC <- k$DOC * water@toc # s-1
  k_S_NO <- k$NO3 * water@no3 # s-1
  #note this is reading the k val for DOC_SRDOM, but i suspect it may be an excell error and it shoud be k for hco3
  k_S_HCO3 <- (((water@alk / (100 * 1000)) + 10^-(uvabs254) - 10^-(14 - uvabs254)) /
    (2 * ((10^-10.33) / (10^-uvabs254)) + 1)) *
    k$DOC_SRDOM
  #same thing here, reading k vals from doc_srdom and hco3 but its probably just been shifted in excel
  # also more siginifcant figures here explain numerical differences from excel
  k_S_CO3 <- ((10^-10.3) / (10^-water@ph)) * (k_S_HCO3 / k$DOC_SRDOM) * k$HCO3 # s-1

  SC <- k_S_DOC + k_S_NO + k_S_HCO3 + k_S_CO3
  return(SC)
}
