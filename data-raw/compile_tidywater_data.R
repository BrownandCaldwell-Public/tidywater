# Generate data frames of data used across tidywater functions

# List of molecular weights for different chemical additions
mweights<-data.frame(na=22.98977,
                     k=39.0983,
                     cl=35.453,
                     so4=96.0626,
                     caco3=100.0869,
                     hcl=36.46094,
                     h2so4=98.079,
                     h3po4=97.995181,
                     naoh=39.9971,
                     na2co3=105.98844,
                     nahco3=84.00661,
                     caoh2=74.09268,
                     mgoh2=58.31968,
                     cl2=70.906)

usethis::use_data(mweights,overwrite = TRUE)

#List of acid dissociation constants
discons<-data.frame(#Carbonic acid
                     k1co3=10^-6.35, #H2CO3<-->HCO3- + H+
                     k2co3=10^-10.33, #HCO3<-->CO32- + H+
                     
                     #Sulfate
                     kso4=10^-1.99, #H2SO4<-->2H+ + SO42-
                     
                     #Phosphate
                     k1po4=10^-2.16, #H3PO4<-->H+ + H2PO4-
                     k2po4=10^-7.20, #H2PO4-<-->H+ + HPO42-
                     k3po4=10^-12.35, #HPO42--<-->H+ + PO43-
                     
                     #Hypochlorite
                     kocl=10^-7.6) #HOCl<-->H+ + OCl-

usethis::use_data(discons, overwrite = TRUE)

#Default water quality parameters - need to research and document sources for these values
wq<-data.frame(water_type=c("dw","gw","sw","ww"),
               ph=c(7,7,7,7),
               temp=c(20,25,25,25),
               alk=c(25,100,50,100),
               tot_hard=c(100,150,100,300),
               c_hard=c(50,75,50,150),
               na=c(10,50,20,44),
               k=c(10,0,0,0),
               cl=c(10,20,50,100),
               so4=c(40,40,40,40))

usethis::use_data(wq, overwrite = TRUE)
