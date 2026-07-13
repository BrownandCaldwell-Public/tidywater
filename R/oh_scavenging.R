# purpose of this function is to take in a water and use existing k values to get an estimated oh scavenging rate
# should be in units of s^-1
#
# so far this has all been taken from Syd's arcadia sheet

oh_scavenging <- function(water, dose=0, no2) {
  # K vals from syd's arcadia sheet
  # k(OH,DOC/TOC)	2.50E+04	L/mg s-1	Larson et al. 1988
  # k(OH,DOC/TOC) SRDOM	1.86E+04	L/mg s-1	Westerhoff et al. 2007
  # k(OH,HCO3-)	8.50E+06	M-1 s-1	Buxton et al.,1988
  # k(OH,CO3-2)	3.90E+08	M-1 s-1	Buxton et al.,1988
  # k(OH,NO2-)	1.00E+09	M-1 s-1	Buxton et al.,1988
  # k(OH,NO3-)	1.00E+05	M-1 s-1	Keen et al., 2012
  # k(OH,H2O2)	2.70E+07	M-1 s-1	Buxton et al.,1988v

  # what if no alk is given but the water contains hco3 and co3 

 #validate_water(water, c("toc", "alk", "no3", "ph", "uv254"))

  # avaraging k from literature vals for DOC
  #DOC_avg <- mean(c(
   # 2.50E+04, # arson et al. 1988 L/mg s^-1
    # same that the scavenging calc uses 

    # 2.94E+08, #found by BC M^-1 s^-1

   # 3.00E+08, #Larson and Zepp (1988) Mc^-1s^-1

    # 2.90E+00, # Brezonik and Fulkerson-Brekken (1998) Mc^-1s^-1
    # 2.70E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 2.00E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 2.10E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 6.35E+08, #Vinge et al (2021) M^-1 s^-1
    # 2.00E+08 #Wols et al (2014) - 33 M^-1 s^-1
 # ))
# not sure how to deal with different units... maybe 3 different avareages??


  k <- list(
    DOC =  2.5e4  ,
    DOC_SRDOM = 1.86E+04, # L/mg s-1
    HCO3 = 8.50E+06, # M-1 s-1
    CO3 = 3.90E+08, # M-1 s-1
    NO2 = 1.00E+09, # M-1 s-1
    NO3 = 1.00E+05, # M-1 s-1
    H2O2 = 2.70E+07 # M-1 s-1
  )

  if ((!is.na(water@alk) & water@alk != 0) & (!is.na(water@uv254) & water@uv254 != 0)) {
  k_S_DOC <- k$DOC * water@toc # s-1
  k_S_NO3 <- k$NO3 * water@no3 # s-1
  #note this is reading the k val for DOC_SRDOM, but i suspect it may be an excell error and it shoud be k for hco3
  k_S_HCO3 <- (((water@alk / (100 * 1000)) + 10^-(water@uv254) - 10^-(14 - water@uv254)) /
    (2 * ((10^-10.33) / (10^-water@uv254)) + 1)) *
    k$HCO3 # s-1
  #same thing here, reading k vals from doc_srdom and hco3 but its probably just been shifted in excel
  # also more siginifcant figures here explain numerical differences from excel
  k_S_CO3 <- ((10^-10.3) / (10^-water@ph)) * (k_S_HCO3 / k$DOC_SRDOM) * k$CO3 # s-1
  k_S_NO2 <- k$NO2 * no2
  k_S_H2O2 <- k$H2O2 * dose # s-1
  }
  else if( !is.na(water@hco3)& !is.na(water@co3)){
  k_S_DOC <- k$DOC * water@toc # s-1
  k_S_NO3 <- k$NO3 * water@no3 # s-1
  #note this is reading the k val for DOC_SRDOM, but i suspect it may be an excell error and it shoud be k for hco3
  k_S_HCO3 <-water@hco3* k$HCO3 # s-1
  #same thing here, reading k vals from doc_srdom and hco3 but its probably just been shifted in excel
  # also more siginifcant figures here explain numerical differences from excel
  k_S_CO3 <- water@co3 * k$CO3 # s-1
  k_S_NO2 <- k$NO2 * no2
  k_S_H2O2 <- k$H2O2 * dose # s-1
  }

  if (is.na(k_S_DOC)) k_S_DOC<-0
  if (is.na(k_S_NO3)) k_S_NO3<-0
  if (is.na(k_S_HCO3)) k_S_HCO3<-0
  if (is.na(k_S_CO3)) k_S_CO3<-0
  if (is.na(k_S_NO2)) k_S_NO2<-0
  if (is.na(k_S_H2O2)) k_S_H2O2<-0

  SC <- (k_S_DOC) + k_S_NO3 + k_S_HCO3 + k_S_CO3 + k_S_H2O2
  return(SC)
}

#2.6% difference from Syd's original values 


