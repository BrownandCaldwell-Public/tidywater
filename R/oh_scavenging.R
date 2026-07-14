oh_scavenging <- function(water, dose=0, kdoc_adjust="default") {
  # K vals from syd's arcadia sheet
  # k(OH,DOC/TOC)	2.50E+04	L/mg s-1	Larson et al. 1988
  # k(OH,DOC/TOC) SRDOM	1.86E+04	L/mg s-1	Westerhoff et al. 2007
  # k(OH,HCO3-)	8.50E+06	M-1 s-1	Buxton et al.,1988
  # k(OH,CO3-2)	3.90E+08	M-1 s-1	Buxton et al.,1988
  # k(OH,NO2-)	1.00E+09	M-1 s-1	Buxton et al.,1988
  # k(OH,NO3-)	1.00E+05	M-1 s-1	Keen et al., 2012
  # k(OH,H2O2)	2.70E+07	M-1 s-1	Buxton et al.,1988

  # other K values, with different units 
    # 2.90E+00, # Brezonik and Fulkerson-Brekken (1998) Mc^-1s^-1
    # 2.70E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 2.00E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 2.10E+00, #Katsoyiannis et al. (2011) Mc^-1s^-1
    # 6.35E+08, #Vinge et al (2021) M^-1 s^-1
    # 2.00E+08 #Wols et al (2014) - 33 M^-1 s^-1


  # List of each constituent k value to access easier and for clarity.
  k <- list(
    DOC =  2.5e4  ,
    DOC_SRDOM = 1.86E+04, # L/mg s-1
    HCO3 = 8.50E+06, # M-1 s-1
    CO3 = 3.90E+08, # M-1 s-1
    NO2 = 1.00E+09, # M-1 s-1
    NO3 = 1.00E+05, # M-1 s-1
    H2O2 = 2.70E+07 # M-1 s-1
  )

  if(kdoc_adjust != "default") k$DOC<-kdoc_adjust                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
  
# init scavenging terms to 0 
  k_S_NO3<-0
  k_S_NO3<-0
  k_S_DOC<-0
  k_S_HCO3<-0
  k_S_CO3<-0
  k_S_NO2<-0
  k_S_H2O2<-0

  # checking to see how HCO3 and CO3 are calculated, if uv and alk exist, use the same formulas as in Syd's original 
  # if not, use simpler logic. 
  if ((!is.na(water@alk) & water@alk != 0) & (!is.na(water@uv254) & water@uv254 != 0)) {
  k_S_DOC <- k$DOC * water@toc # s-1
  k_S_NO3 <- k$NO3 * water@no3 # s-1
  k_S_HCO3 <- (((water@alk / (100 * 1000)) + 10^-(water@uv254) - 10^-(14 - water@uv254)) /
    (2 * ((10^-10.33) / (10^-water@uv254)) + 1)) *
    k$HCO3 # s-1
  k_S_CO3 <- ((10^-10.3) / (10^-water@ph)) * (k_S_HCO3 / k$DOC_SRDOM) * k$CO3 # s-1
  k_S_NO2 <- k$NO2 * water@no2
  k_S_H2O2 <- k$H2O2 * dose # s-1
  }
  # the else part, dosen't calculate carbonate bal I believe
  else if( !is.na(water@hco3)& !is.na(water@co3)){
  k_S_DOC <- k$DOC * water@toc # s-1
  k_S_NO3 <- k$NO3 * water@no3 # s-1
  k_S_HCO3 <-water@hco3* k$HCO3 # s-1
  k_S_CO3 <- water@co3 * k$CO3 # s-1
  k_S_NO2 <- k$NO2 * water@no2 # s-1
  k_S_H2O2 <- k$H2O2 * dose # s-1
  }

  # turn each NA value to 0 because NA's will mess up execution and 0's just won't contribute anything. 
  if (is.na(k_S_DOC)) k_S_DOC<-0
  if (is.na(k_S_NO3)) k_S_NO3<-0
  if (is.na(k_S_HCO3)) k_S_HCO3<-0
  if (is.na(k_S_CO3)) k_S_CO3<-0
  if (is.na(k_S_NO2)) k_S_NO2<-0
  if (is.na(k_S_H2O2)) k_S_H2O2<-0

  # summ all of the scavenging terms 
  SC <- (k_S_DOC) + k_S_NO3 + k_S_HCO3 + k_S_CO3 + k_S_H2O2 +k_S_NO2
  return(SC)
}

#2.6% difference from Syd's original values 


