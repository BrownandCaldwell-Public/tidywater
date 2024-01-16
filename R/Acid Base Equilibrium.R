

# Acid/Base Equilibrium Functions
# These functions determine pH and alkalinity after blending and chemical addition

# Author: R. Mulhern, based on spreadsheet calculations by C. Corwin
# Reviewers:

#' Define water vector
#' 
#' This function takes water quality parameters and creates a standard data frame that forms the input and output of all pH functions.
#' Carbonate balance is calculated and units are converted to mol/L
#' 
#' @param type Water type = drinking water, groundwater, surface water, or waste water
#' @param ph water pH
#' @param temp Temperature in degree C
#' @param alk Alkalinity in mg/L as CaCO3
#' @param tot_hard Total hardness in mg/L as CaCO3
#' @param c_hard Calcium hardness in mg/L as CaCO3
#' @param na Sodium in mg/L Na+
#' @param k Potassium in mg/L K+
#' @param cl Chloride in mg/L Cl-
#' @param so4 Sulfate in mg/L SO42-
#' @param tot_ocl Chlorine in mg/L as ??
#' 
#' @examples
#' # Put example code here
#' 
#' @export
#' 
waterdef<-function(ph,temp,alk,tot_hard,c_hard,na,k,cl,so4,tot_ocl=0,type){
  
  #Handle missing arguments with no default water type defined
  if(missing(ph) & missing(type)){
    stop("Missing value for pH. If not known, specify water type to use default estimated value.")
  }
  
  if(missing(temp) & missing(type)){
    stop("Missing value for temperature. If not known, specify water type to use default estimated value.")
  }
  
  if(missing(alk) & missing(type)){
    stop("Missing value for alkalinity. If not known, specify water type to use default estimated value.")
  }
  
  if(missing(tot_hard) & missing(type)){
    stop("Missing value for total hardness. If not known, specify water type to use default estimated value.")
  }
  
  if(missing(c_hard) & missing(type)){
    stop("Missing value for calcium hardness. If not known, specify water type to use default estimated value.")
  }
  
  if(missing(na) & missing(type)){
    stop("Missing value for sodium (Na+). If not known, specify water type to use default estimated value.")
  }
  
  if(missing(k) & missing(type)){
    stop("Missing value for potassium (K+). If not known, specify water type to use default estimated value.")
  }
  
  if(missing(cl) & missing(type)){
    stop("Missing value for chloride (Cl-). If not known, specify water type to use default estimated value.")
  }
  
  if(missing(so4) & missing(type)){
    stop("Missing value for sulfate (SO4_2-). If not known, specify water type to use default estimated value.")
  }
  
  #Handle missing water type when all other parameters are specified or unknown water type
  if(missing(ph)==FALSE & missing(temp)==FALSE & missing(alk)==FALSE & missing(tot_hard)==FALSE & missing(c_hard)==FALSE 
     & missing(na)==FALSE & missing(k)==FALSE & missing(cl)==FALSE & missing(so4)==FALSE & missing(type)){
    type=NA
  } else if ((type %in% wq$water_type)==FALSE){
    stop("Unknown water type. Options include drinking water (dw), groundwater (gw), surface water (sw), or wastewater (ww).")
  }
  
  #Restrict data frame of default water quality values to only those for specified water type
  wq=wq%>%
    filter(water_type==type)
  
  #Handle missing arguments with a valid water type defined (warning only)
  if(missing(ph) & missing(type)==FALSE){
    ph=wq$ph
    warning("Missing value for pH. Default value will be used based on entered water type.")
  }
  
  if(missing(temp) & missing(type)==FALSE){
    temp=wq$temp
    warning("Missing value for temperature. Default value will be used based on entered water type.")
  }
  
  if(missing(alk) & missing(type)==FALSE){
    alk=wq$alk
    warning("Missing value for alkalinity. Default value will be used based on entered water type.")
  }
  
  if(missing(tot_hard) & missing(type)==FALSE){
    tot_hard=wq$tot_hard
    warning("Missing value for total hardness. Default value will be used based on entered water type.")
  }
  
  if(missing(c_hard) & missing(type)==FALSE){
    c_hard=wq$c_hard
    warning("Missing value for calcium hardness. Default value will be used based on entered water type.")
  }
  
  if(missing(na) & missing(type)==FALSE){
    na=wq$na
    warning("Missing value for sodium (Na+). Default value will be used based on entered water type.")
  }
  
  if(missing(k) & missing(type)==FALSE){
    k=wq$k
    warning("Missing value for potassium (K+). Default value will be used based on entered water type.")
  }
  
  if(missing(cl) & missing(type)==FALSE){
    cl=wq$cl
    warning("Missing value for chloride (Cl-). Default value will be used based on entered water type.")
  }
  
  if(missing(so4) & missing(type)==FALSE){
    so4=wq$so4
    warning("Missing value for sulfate (SO4_2-). Default value will be used based on entered water type.")
  }
  
  #Calculate kw from temp
  tempa=temp+273.15 #absolute temperature (K)
  pkw=round((4787.3/(tempa))+(7.1321*log10(tempa))+(0.010365*tempa)-22.801,1) #water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  kw=10^-pkw
  
  #convert major ion concentration inputs to mol/L
  na=na/mweights$na/1000
  ca=c_hard/mweights$caco3/1000
  mg=(tot_hard-c_hard)/mweights$caco3/1000
  k=k/mweights$k/1000
  cl=cl/mweights$cl/1000
  so4=so4/mweights$so4/1000
  h=10^-ph
  oh=kw/h
  
  #calculate carbonate system balance
  #k1co3=10^-6.35 #first dissociation rate constant for carbonic acid H2CO3<-->HCO3- + H+
  #k2co3=10^-10.33 #second dissociation rate constant for carbonic acid HCO3<-->CO32- + H+
  alpha1=(discons$k1co3*h)/(h^2+discons$k1co3*h+discons$k1co3*discons$k2co3) #proportion of total carbonate as HCO3-
  alpha2=(discons$k1co3*discons$k2co3)/(h^2+discons$k1co3*h+discons$k1co3*discons$k2co3) #proportion of total carbonate as CO32-
  alk_eq=(alk*2)/(mweights$caco3*1000) #convert alkalinity input to equivalents/L
  tot_co3=(alk_eq+h-oh)/(alpha1+2*alpha2) #calculate total carbonate concentration
  hco3=tot_co3*alpha1
  co3=tot_co3*alpha2
  
  cba=hco3+2*co3+oh-h #calculate total acid base balance, equivalent to alkalinity in most natural waters
  
  #Compile complete source water data frame to save to environment
  water_df=data.frame(ph,temp,alk,tot_hard,na,ca,mg,k,cl,so4,hco3,co3,h,oh,tot_ocl,tot_co3,cba,kw,alk_eq)
  return(water_df)
}

#### Function to calculate the pH from a given water quality vector. Not exported in namespace.

phfinal<-function(water){
  cba<-water$cba
  kw<-water$kw
  
  #### COMPILE ACID DISSOCIATION CONSTANTS
  
  #Carbonate
  # k1co3=10^-6.35 #H2CO3<-->HCO3- + H+
  # k2co3=10^-10.33 #HCO3-<-->CO32- + H+
  tot_co3=water$tot_co3
  
  #Sulfate
  #kso4=10^-1.99 #H2SO4<-->2H+ + SO42-
  if(is.null(water$so4_dose)){
    so4_dose=0
  } else {so4_dose=water$so4_dose}
  
  #Phosphate
  #k1po4=10^-2.16 #H3PO4<-->H+ + H2PO4-
  #k2po4=10^-7.20 #H2PO4-<-->H+ + HPO42-
  #k3po4=10^-12.35 #HPO42--<-->H+ + PO43-
  if(is.null(water$po4_dose)){
    po4_dose=0
  } else {po4_dose=water$po4_dose}
  
  #Hypochlorite
  #kocl=10^-7.6 #HOCl<-->H+ + OCl-
  if(is.null(water$tot_ocl)){
    tot_ocl=0
  } else {tot_ocl=water$tot_ocl}
  
  #### SOLVE FOR pH
  n=0
  p1=10^-n
  F = -1 #set starting value for F
  while (F<0){
    n=n+1
    p1=10^-n
    F = kw/p1 + 
      (2 + p1 / discons$kso4) * (so4_dose / (p1 / discons$kso4 + 1)) + 
      (p1 ^ 2 / discons$k2po4 / discons$k3po4 + 2 * p1 / discons$k3po4 + 3) * (po4_dose / (p1 ^ 3 / discons$k1po4 / discons$k2po4 / discons$k3po4 + p1 ^ 2 / discons$k2po4 / discons$k3po4+ p1 / discons$k3po4 + 1)) + 
      (p1 / discons$k2co3 + 2) * (tot_co3 / (p1 ^ 2 / discons$k1co3 / discons$k2co3 + p1 / discons$k2co3 + 1)) +
      tot_ocl / (p1 / discons$kocl + 1) -
      p1 - cba
  }
  
  p2=p1
  p1=10^-(n-1)
  delta=0.1 #set starting value for delta
  while (delta>0.00001){
    F1 = kw/p1 +
      (2 + p1 / discons$kso4) * (so4_dose / (p1 / discons$kso4 + 1)) + 
      (p1 ^ 2 / discons$k2po4 / discons$k3po4 + 2 * p1 / discons$k3po4 + 3) * (po4_dose / (p1 ^ 3 / discons$k1po4 / discons$k2po4 / discons$k3po4 + p1 ^ 2 / discons$k2po4 / discons$k3po4+ p1 / discons$k3po4 + 1)) + 
      (p1 / discons$k2co3 + 2) * (tot_co3 / (p1 ^ 2 / discons$k1co3 / discons$k2co3 + p1 / discons$k2co3 + 1)) +
      tot_ocl / (p1 / discons$kocl + 1) -
      p1 - cba
    F2 = kw/p2 +
      (2 + p2 / discons$kso4) * (so4_dose / (p2 / discons$kso4 + 1)) + 
      (p2 ^ 2 / discons$k2po4 / discons$k3po4 + 2 * p2 / discons$k3po4 + 3) * (po4_dose / (p2 ^ 3 / discons$k1po4 / discons$k2po4 / discons$k3po4 + p2 ^ 2 / discons$k2po4 / discons$k3po4+ p2 / discons$k3po4 + 1)) + 
      (p2 / discons$k2co3 + 2) * (tot_co3 / (p2 ^ 2 / discons$k1co3 / discons$k2co3 + p2 / discons$k2co3 + 1)) +
      tot_ocl / (p2 / discons$kocl + 1) -
      p2 - cba
    pt = p2
    p2 = p2 - F2 * (p1-p2) / (F1-F2)
    p1 = pt
    delta=abs(p2-p1)*10^10
  }
  
  hfinal=p2
  phfinal=-log10(hfinal)
  return(round(phfinal,2))
}

#' Chemical Dose Function
#' 
#' This function takes chemical doses and a water data frame defined by 'waterdef' and outputs a new water data frame with updated ion balance and pH.
#' Units of all chemical additions in mg/L.
#' Returns data frame of dosed water quality.
#' 
#' @param water Source water data frame created by waterdef
#' @param hcl Hydrochloric acid: HCl -> H + Cl 
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3op4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param caocl2 Calcium hypochlorite: Ca(OCl)2 -> Ca + 2OCl
#' 
#' @seealso waterdef
#' 
#' @examples
#' # Put example code here
#' 
#' @export
#' 
dose<-function(water,hcl=0,h2so4=0,h3po4=0,naoh=0,na2co3=0,nahco3=0,caoh2=0,mgoh2=0,cl2=0,naocl=0,caocl2=0){
  
  if(missing(water)){
    stop("No source water vector defined. Create a source water vector using 'sourceWater' function.")}
  
  #### CONVERT INDIVIDUAL CHEMICAL ADDITIONS TO MOLAR ####
  
  #Hydrochloric acid (HCl) dose
  hcl=hcl/mweights$hcl*10^-3
  
  #Sulfuric acid (H2SO4) dose
  h2so4=h2so4/mweights$h2so4*10^-3
  
  #Phosphoric acid (H3PO4) dose
  h3po4=h3po4/mweights$h3po4*10^-3
  
  #Caustic soda (NaOH) dose
  naoh=naoh/mweights$naoh*10^-3
  
  #Soda ash (Na2CO3) dose
  na2co3=na2co3/mweights$na2co3*10^-3
  
  #Sodium bicarbonate (NaHCO3) dose
  nahco3=nahco3/mweights$nahco3*10^-3
  
  #Lime (Ca(OH)2) dose
  caoh2=caoh2/mweights$caoh2*10^-3
  
  #Magnesium hydroxide (Mg(OH)2) dose
  mgoh2=mgoh2/mweights$mgoh2*10^-3
  
  #Chlorine gas (Cl2)
  cl2=cl2/mweights$cl2*10^-3
  
  #Sodium hypochlorite (NaOCl) as Cl2
  naocl=naocl/mweights$cl2*10^-3
  
  #Calcium hypochlorite (Ca(OCl)2) as Cl2
  caocl2=caocl2/mweights$cl2*10^-3
  
  #### CALCULATE NEW ION BALANCE FROM ALL CHEMICAL ADDITIONS ####
  
  #Total sodium
  na_dose=naoh + 2*na2co3 + nahco3 + naocl
  tot_na=water$na + na_dose
  
  #Total calcium
  ca_dose=caoh2 + caocl2/2 
  tot_ca=water$ca + ca_dose
  
  #Total magnesium
  mg_dose=mgoh2
  tot_mg=water$mg + mg_dose
  
  #Total potassium
  k_dose=0
  tot_k=water$k + k_dose
  
  #Total chloride 
  cl_dose=hcl + cl2
  tot_cl=water$cl + cl_dose
  
  #Total sulfate
  so4_dose=h2so4
  tot_so4=water$so4 + so4_dose
  
  #Total phosphate
  po4_dose=h3po4
  tot_po4=po4_dose
  
  #Total hypochlorite
  ocl_dose=cl2 + naocl + caocl2
  tot_ocl=water$tot_ocl + ocl_dose
  
  #Total carbonate
  co3_dose=na2co3 + nahco3
  tot_co3=water$tot_co3 + co3_dose
  
  #Update acid/base balance equation with each chemical addition
  cba=water$cba + na_dose + 2*ca_dose + 2*mg_dose - cl_dose  
  
  #Calculate new pH, H+ and OH- concentrations
  kw=water$kw
  ph_inputs=data.frame(tot_cl,tot_so4,so4_dose,tot_po4,po4_dose,tot_na,na_dose,tot_ocl,tot_co3,cba,kw)
  ph=phfinal(ph_inputs)
  h=10^-ph
  oh=kw/h
  
  #Calculate new carbonate system balance
  k1co3=10^-6.35 #first dissociation rate constant for carbonic acid H2CO3<-->HCO3- + H+
  k2co3=10^-10.33 #second dissociation rate constant for carbonic acid HCO3<-->CO32- + H+
  alpha1=(discons$k1co3*h)/(h^2+discons$k1co3*h+discons$k1co3*discons$k2co3) #proportion of total carbonate as HCO3-
  alpha2=(discons$k1co3*discons$k2co3)/(h^2+discons$k1co3*h+discons$k1co3*discons$k2co3) #proportion of total carbonate as CO32-
  hco3=tot_co3*alpha1
  co3=tot_co3*alpha2
  
  #Calculate new alkalinity (mg/L as CacO3)
  alk_eq=(hco3+2*co3+oh-h)
  alk=(alk_eq/2)*mweights$caco3*1000
  
  #Calculate new hardness (mg/L as CaCO3)
  tot_hard=(tot_ca*mweights$caco3*1000)+(tot_mg*mweights$caco3*1000)
  
  #Compile complete dosed water data frame
  dosed_water_df=data.frame(ph,
                            temp=water$temp,
                            alk,
                            tot_hard,
                            na=tot_na,
                            ca=tot_ca,
                            mg=tot_mg,
                            k=tot_k,
                            cl=tot_cl,
                            so4=tot_so4,
                            hco3,co3,h,oh,
                            tot_co3,
                            tot_ocl,
                            cba,
                            kw,
                            alk_eq)
  return(dosed_water_df)
}

#' Water Summary Table
#' 
#' This function takes a source water vector and outputs a formatted summary table.
#' 
#' @param water Source water vector created by link function here
#' 
#' @importFrom knitr kable kables
#' 
#' @examples
#' # Put example code here
#' 
#' @export
#' 
watersummary<-function(water){
  
  #Compile main WQ parameters to print
  params=data.frame(pH=water$ph,
                    Temp=water$temp,
                    Alkalinity=water$alk,
                    Total_Hardness=water$tot_hard)
  
  params=params%>%
    gather(key=param,value=result)%>%
    mutate(units=c("-","deg C","mg/L as CaCO3","mg/L as CaCO3"))
  
  tab1=knitr::kable(params,
             format="simple",
             col.names=c("Key water quality parameters","Result","Units"))
  
  #Compile major ions to print
  ions=data.frame(Na=water$na,
                  Ca=water$ca,
                  Mg=water$mg,
                  K=water$k,
                  Cl=water$cl,
                  SO4=water$so4,
                  HCO3=water$hco3,
                  CO3=water$co3,
                  H=water$h,
                  OH=water$oh)
  
  ions=ions%>%
    gather(key=ion,value=c_mol)
  
  tab2=knitr::kable(ions,
             format="simple",
             col.names=c("Major ions in source water","Concentration (mol/L)"),
             format.args=list(scientific=TRUE),
             digits=10)
  
  #print(kables(list(tab1,tab2)))
  return(knitr::kables(list(tab1,tab2)))
}

#' Water Summary Plot
#' 
#' This function takes a source water vector and outputs an ion balance plot.
#' 
#' @param water Source water vector created by link function here
#' @param title Optional plot title
#' 
#' 
#' @examples
#' # Put example code here
#' 
#' @export
#' 
waterplot<-function(water,title=""){
  
  #Compile major ions to plot
  ions=data.frame(Na=water$na,
                  Ca=water$ca,
                  Mg=water$mg,
                  K=water$k,
                  Cl=water$cl,
                  SO4=water$so4,
                  HCO3=water$hco3,
                  CO3=water$co3,
                  H=water$h,
                  OH=water$oh)
  
  ions%>%
    gather(key=ion,value=concentration)%>%
    mutate(type=case_when(ion %in% c("Na","Ca","Mg","K","H")==TRUE~"Cations",
                          TRUE~"Anions"))%>%
    ggplot(aes(x=concentration,y=type,fill=ion))+
    geom_bar(stat="identity",
             width=0.5,
             #aes(fill=ion),
             alpha=0.5,
             color="black")+
    geom_text(aes(label=ifelse(concentration>10e-5,ion,""),fontface="bold"),
              size=3.5,
              position=position_stack(vjust=0.5))+
    theme_bw()+
    theme(axis.title = element_text(face="bold"))+
    labs(x="Concentration (mol/L)", 
         y="Major cations and anions",
         title=title,
         subtitle=paste0("pH=",water$ph))+
    guides(fill="none")
}
