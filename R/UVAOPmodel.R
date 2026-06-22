# notes from the weekend:Input params from the app, optical path length, Vol, Area, time, target removals of caffeine, sucralose, and carbmazepine, oxidants, ph, nh3, alk, doc, cl, phosphate, backround transmittance, oh backround schavenging. 
# A lot of these exist in tidywater already! 
# Possible implementation plan: parse out relevant rate reactions from merged ode, and 
# remove all cl in other functions to see what we are left with. I think some of the actuall 
# solving logic is in the graphing function. The graphing functions can be dropped entirely. 
# So that means that for our base function (dosing one water, not a df yet) will have following inputs 
# (water object, exposure time, path, vol, area, backround transmitance) provided all other params are
#  validated in water already. Maybe it would work better to make a 'tank'
#  object which holds the params for vol and all that? Probably not, also it would be not very tidywater standard lol. 


# is caffiene other weird species nessacary??



uvaop_model<-function(water, exposure_time, path, vol, area, back_transmitance, irradiance, dose){
  # water is a water object from tidywater
  # exposure_time is in seconds
  # path is in cm
  # vol is in liters
  # area is in square meters
  # back_transmitance is the background transmittance


  #convert irradidance given as mW/cm^2 to energy flux 

  flux<- irradiance * 1e-3 * (254e-9 / (6.626e-34 * 3e8 * 6.022e23)) * 1000 # mol·L^-2·s^-1

  absorbance<- -log10(back_transmitance) # absormabance from transmittance

  temp<- water@temp+273.15 # convert to kelvin

  # species 
  
  H<- 10^(-water@ph) 
  OH<- 10^(-14+water@ph) 
  TIC_molesL<- water@toc *1.22/61000


#!!!
  #Phosphate_moles<- water@phosphate/1000 
  
  #wrong syntax for phospate here, conversion may also be different, currently expecting
  # that phosphate is given as mM/L

  # some of these values may exist in TW already
  K1 <- 10^-6.77 # H2CO3 <-> HCO3 + H
    K2 <- 10^-9.93 # HCO3 <-> CO3 + H
    Den <- H*H + H*K1 + K1*K2
    H2CO3 <- TIC_molesL*H*H/Den
    HCO3 <- TIC_molesL*H*K1/Den
    CO3 <- TIC_molesL*K1*K2/Den

# Not sure what these are 
    Ka1 <- 10^(-2.1)
    Ka2 <- 10^(-7.2)
    Ka3 <- 10^(-12.3)

    # same here 
    D <- 1 + (Ka1/H) + (Ka1*Ka2)/(H^2) + (Ka1*Ka2*Ka3)/(H^3)

    #phosphate species in moles/L
#!!!
# H3PO4  <- Phosphate_moles / D
# H2PO4  <- Phosphate_moles * (Ka1/H) / D
# HPO4   <- Phosphate_moles * (Ka1*Ka2)/(H^2) / D
# PO4    <- Phosphate_moles * (Ka1*Ka2*Ka3)/(H^3) / D

# code then inits the state vector for the ode solver, but I expect that this will be different when we incorporate water objects
# or maybe this is output water, which I wil say it is for now. 

output_water <- water

# Nitrogen compounds
output_water@tot_nh3 <- 0         # NH3 (Ammonia)
output_water@nh4 <- water@tot_nh3 / 14000  # NH4⁺

# H2O2
dose <- dose / (34 * 1000)        # convert mg/L to mol/L

# Organic pollutants
# output_water@dioxane <- water@dioxane  # not a water slot
output_water@doc <- 0             # zero out doc for ODE

# Oxygen species
# output_water@O2 <- 0            # O2 not a water slot; handled as local var
O2_init <- 0

# pH-dependent species — use lowercase slot names
output_water@h <- water@h         # H⁺
output_water@oh <- water@oh       # OH⁻

# Carbonate species — use lowercase slot names
output_water@hco3 <- HCO3         # HCO3⁻
output_water@h2co3 <- H2CO3       # H2CO3
output_water@co3 <- CO3           # CO3²⁻

# Chloride-related species — (none needed here)

# Phosphate species
output_water@h2po4 <- water@h2po4
output_water@hpo4  <- water@hpo4

# Pharmaceuticals — not water slots; use local vars
# output_water@caffeine      <- water@caffeine      # not a water slot
# output_water@sucralose     <- water@sucralose     # not a water slot
# output_water@carbamazepine <- water@carbamazepine # not a water slot
caffeine_init      <- 0  # TODO: pass as argument
sucralose_init     <- 0  # TODO: pass as argument
carbamazepine_init <- 0  # TODO: pass as argument

# output_water@unknown <- 0  # not a water slot

# Initial state vector
state <- c(
  output_water@tot_nh3,  # NH3
  output_water@nh4,      # NH4+
  # dioxane,             # not a water slot
  output_water@doc,      # DOC
  O2_init <- 0,               # O2
  output_water@h,        # H+
  output_water@oh,       # OH-
  output_water@hco3,     # HCO3-
  output_water@h2co3,    # H2CO3
  output_water@co3,      # CO3²-
  output_water@h2po4,    # H2PO4-
  output_water@hpo4,     # HPO4²-
  caffeine_init <-0 ,         # caffeine
  sucralose_init <-0 ,        # sucralose
  carbamazepine_init <-0     # carbamazepine
)
  
  
params<- list(
  L = path, 
  V = vol,
  A = area, 
  E0 = flux, 
  a_background = absorbance,
  dose = dose)

time <- seq(0, exposure_time, by = 1)  
  
out <- deSolve::ode(
  y = state, 
  times = time, 
  func = merged_ODE(t = time, water = output_water, params = params) , 
  parms = params,
  method= "ode45")


#Determine time necessary for 0.5-log elimination
# Calculate the 0.5-log thresholds for 1,4-dioxane, caffeine, and sucralose

threshold_diox <- water@dioxane * 10^(-0.5)
threshold_carb <- water@carbamazepine * 10^(-0.5)
threshold_sucralose <- water@sucralose * 10^(-0.5)


# init elim times 
time_diox <- NA_real_
time_carb <- NA_real_
time_sucralose <- NA_real_



# Find the crossing time for 1,4-dioxane
for (i in seq_len(nrow(out) - 1)) {
  if ((out[i, 34] > threshold_diox && out[i + 1, 34] <= threshold_diox) ||
      (out[i, 34] < threshold_diox && out[i + 1, 34] >= threshold_diox)) {
    time_diox <- approx(x = out[c(i, i + 1), 34], y = t[c(i, i + 1)], xout = threshold_diox)$y
    break
  }
}

# Find the crossing time for caffeine
for (i in seq_len(nrow(out) - 1)) {
  if ((out[i, 61] > threshold_carb && out[i + 1, 61] <= threshold_carb) ||
      (out[i, 61] < threshold_carb && out[i + 1, 61] >= threshold_carb)) {
    time_carb <- approx(x = out[c(i, i + 1), 61], y = t[c(i, i + 1)], xout = threshold_carb)$y
    break
  }
}

# Find the crossing time for sucralose
for (i in seq_len(nrow(out) - 1)) {
  if ((out[i, 62] > threshold_sucralose && out[i + 1, 62] <= threshold_sucralose) ||
      (out[i, 62] < threshold_sucralose && out[i + 1, 62] >= threshold_sucralose)) {
    time_sucralose <- approx(x = out[c(i, i + 1), 62], y = t[c(i, i + 1)], xout = threshold_sucralose)$y
    break
  }
}



}

merged_ODE <- function(t, water, params) {
  # # Extract app and t_f from parms (deSolve passes a single parms argument)
  # app <- parms$app
  # t_f <- parms$t_f

  # -----------------------------------------------------------------------
  # Extract species from state vector (64x1), all in mol/L
  # -----------------------------------------------------------------------
  NH3        <- water@tot_nh3
  NH4            <- water@nh4
  H2O2           <- params$dose / (34 * 1000)  # convert mg/L to mol/L
  OH_rad         <- water@oh
  #initing species not already in water object to zero 
  #!!! issue here is that not all are in TW already, 
  # the question is do we need this coverage, or can we simplify the 4 types 
  #of o3 (for example) to just one O3?
  HO2_rad        <- 0
  HO2_minus      <- 0
  O3P            <- 0
  O1D            <- 0
  NOH            <- 0
  NO_rad         <- 0
  NO2_rad        <- 0
  NO2_minus      <- 0
  N2O3           <- 0
  N2O4           <- 0
  NO3_minus      <- 0
  dioxane        <- 0
  doc            <- water@doc
  O2             <- 0
  H_plus         <- water@h
  OH_minus       <- water@oh
  HCO3_minus     <- water@hco3
  H2CO3          <- water@h2co3
  CO3_2          <- 0
  NH2O2_rad      <-0
  N2O            <-0
  CO3_rad        <- water@co3
  N2             <-0
  O_rad_minus    <-0
  HPO4_2         <- water@hpo4
  H2PO4_minus    <-0
  HPO4_rad       <-0
  O2_super       <-0
  O3_var         <-0
  O3_rad         <- 0
  H_atom         <- water@h
  O3_rad_minus   <-0
  NOM            <-0
  caffeine       <-0
  sucralose      <-0
  OHexposure     <- 0
  carbamazepine <- 0

  # -----------------------------------------------------------------------
  # Initialize all derivatives to zero
  # -----------------------------------------------------------------------
 derivs<-c(
  dNH3_dt            <- 0,
  dNH4_dt            <- 0,
  dH2O2_dt           <- 0,
  dOH_rad_dt         <- 0,
  dHO2_rad_dt        <- 0,
  dHO2_minus_dt      <- 0,
  dO3P_dt            <- 0,
  dO1D_dt            <- 0,
  dNOH_dt            <- 0,
  dNO_rad_dt         <- 0,
  dNO2_rad_dt        <- 0,
  dNO2_minus_dt      <- 0,
  dN2O3_dt           <- 0,
  dN2O4_dt           <- 0,
  dNO3_minus_dt      <- 0,
  ddioxane_dt        <- 0,
  ddoc_dt            <- 0,
  dO2_dt             <- 0,
  dH_plus_dt         <- 0,
  dOH_minus_dt       <- 0,
  dHCO3_minus_dt     <- 0,
  dH2CO3_dt          <- 0,
  dCO3_2_dt          <- 0,
  dNH2O2_rad_dt      <- 0,
  dN2O_dt            <- 0,
  dCO3_rad_dt        <- 0,
  dN2_dt             <- 0,
  dO_rad_minus_dt    <- 0,
  dHPO4_2_dt         <- 0,
  dH2PO4_minus_dt    <- 0,
  dHPO4_rad_dt       <- 0,
  dO2_super_dt       <- 0,
  dO3_var_dt         <- 0,
  dO3_rad_dt         <- 0,
  dH_atom_dt         <- 0,
  dO3_rad_minus_dt   <- 0,
  dNOM_dt            <- 0,
  dcaffeine_dt       <- 0,
  dsucralose_dt      <- 0,
  dOHexposure_dt     <- 0,
  dcarbamazepine_dt  <- 0
 )

  # -----------------------------------------------------------------------
  # Global parameters
  # -----------------------------------------------------------------------
  T  <- water@temp
  L  <- params$L
  V  <- params$V
  A  <- params$A
  E0 <- params$E0


  # Absorbance coefficients (M-1 cm-1)
  a_H2O2       <- 18   * H2O2
  a_HO2_minus  <- 228  * HO2_minus
  a_background <- params$a_background
  a_sucralose  <- 14.7 * sucralose

  a_total <- a_H2O2 + a_HO2_minus + a_sucralose + a_background / L

  # Quantum yields
  phi_H2O2      <- 0.5
  phi_HO2_rad   <- 0.5
  phi_sucralose <- 0.49

  # Photolysis prefactor
  pf <- (E0 / L) * (1 - 10^(-a_total * L))


 
with(as.list(derivs), {

  dydt<-c()
  
    for (i in 1:36){
      dydt[i] <-0
    }

  


  # R25: NH2O2• -> NO• + H2O
  r25 <- 1.0e8 * NH2O2_rad
  dNH2O2_rad_dt <- dNH2O2_rad_dt - r25
  dNO_rad_dt    <- dNO_rad_dt    + r25


  # R27: NH2O2• -> N2O
  r27 <- 5.98e8 * NH2O2_rad
  dNH2O2_rad_dt <- dNH2O2_rad_dt - r27
  dN2O_dt       <- dN2O_dt       + r27

  # R29: NO• + OH• -> NO2- + H+
  r29 <- 1.0e10 * NO_rad * OH_rad
  dNO_rad_dt    <- dNO_rad_dt    - r29
  dOH_rad_dt    <- dOH_rad_dt    - r29
  dNO2_minus_dt <- dNO2_minus_dt + r29
  dH_plus_dt    <- dH_plus_dt    + r29

  # R30: NO2- + OH• -> NO2• + OH-
  r30 <- 1.2e10 * NO2_minus * OH_rad
  dNO2_minus_dt <- dNO2_minus_dt - r30
  dOH_rad_dt    <- dOH_rad_dt    - r30
  dNO2_rad_dt   <- dNO2_rad_dt   + r30
  dOH_minus_dt  <- dOH_minus_dt  + r30

  # R31: 2NO• + O2 -> 2NO2•
  r31 <- 2.1e6 * NO_rad^2 * O2
  dNO_rad_dt  <- dNO_rad_dt  - 2 * r31
  dNO2_rad_dt <- dNO2_rad_dt + 2 * r31

  # R32: NO• + NO2• -> N2O3
  r32 <- 1.1e9 * NO_rad * NO2_rad
  dNO_rad_dt  <- dNO_rad_dt  - r32
  dNO2_rad_dt <- dNO2_rad_dt - r32
  dN2O3_dt    <- dN2O3_dt    + r32

  # R33: N2O3 -> NO• + NO2•
  r33 <- 4.3e6 * N2O3
  dN2O3_dt    <- dN2O3_dt    - r33
  dNO_rad_dt  <- dNO_rad_dt  + r33
  dNO2_rad_dt <- dNO2_rad_dt + r33

  # R34: N2O3 + H2O -> 2NO2- + 2H+
  r34 <- 1.6e3 * N2O3
  dN2O3_dt      <- dN2O3_dt      - r34
  dNO2_minus_dt <- dNO2_minus_dt + 2 * r34
  dH_plus_dt    <- dH_plus_dt    + 2 * r34

  # R35: 2NO2• -> N2O4
  r35 <- 4.5e8 * NO2_rad^2
  dNO2_rad_dt <- dNO2_rad_dt - 2 * r35
  dN2O4_dt    <- dN2O4_dt    + r35

  # R36: N2O4 + H2O -> NO2- + NO3- + 2H+
  r36 <- 1.0e3 * N2O4
  dN2O4_dt      <- dN2O4_dt      - r36
  dNO2_minus_dt <- dNO2_minus_dt + r36
  dNO3_minus_dt <- dNO3_minus_dt + r36
  dH_plus_dt    <- dH_plus_dt    + 2 * r36

    #hydrogeen and oxygen radical reactions

  # R37: H2O -> H+ + OH-
  r37_extra <- 1.00e-3
  dH_plus_dt   <- dH_plus_dt   + r37_extra
  dOH_minus_dt <- dOH_minus_dt + r37_extra

  # R38: H+ + OH- -> H2O
  r38_extra <- 1.00e11 * H_plus * OH_minus
  dH_plus_dt   <- dH_plus_dt   - r38_extra
  dOH_minus_dt <- dOH_minus_dt - r38_extra

  # R39: OH• + OH• -> H2O2
  r39_extra <- 5.50e9 * OH_rad^2
  dOH_rad_dt <- dOH_rad_dt - 2 * r39_extra
  dH2O2_dt   <- dH2O2_dt   + r39_extra

  # R40: OH• + OH- -> O•- + H2O
  r40_extra <- 1.20e10 * OH_rad * OH_minus
  dOH_rad_dt      <- dOH_rad_dt      - r40_extra
  dO_rad_minus_dt <- dO_rad_minus_dt + r40_extra

  # R64: OH• + HCO3- -> CO3•- + H2O
  r64_extra <- 8.50e6 * OH_rad * HCO3_minus
  dOH_rad_dt     <- dOH_rad_dt     - r64_extra
  dHCO3_minus_dt <- dHCO3_minus_dt - r64_extra
  dCO3_rad_dt    <- dCO3_rad_dt    + r64_extra

  # R65: OH• + H2CO3 -> CO3•- + H2O + H+
  r65_extra <- 1.00e6 * OH_rad * H2CO3
  dOH_rad_dt  <- dOH_rad_dt  - r65_extra
  dH2CO3_dt   <- dH2CO3_dt   - r65_extra
  dCO3_rad_dt <- dCO3_rad_dt + r65_extra
  dH_plus_dt  <- dH_plus_dt  + r65_extra

  # R66: OH• + CO3•- -> products
  r66_extra <- 3.00e9 * OH_rad * CO3_rad
  dOH_rad_dt  <- dOH_rad_dt  - r66_extra
  dCO3_rad_dt <- dCO3_rad_dt - r66_extra

  # R67: CO3•- + CO3•- -> products
  r67_extra <- 3.00e7 * CO3_rad^2
  dCO3_rad_dt <- dCO3_rad_dt - r67_extra

  # R68: 1,4-dioxane + OH• -> products
  r68_extra <- 3.1e9 * dioxane * OH_rad
  ddioxane_dt <- ddioxane_dt - r68_extra
  dOH_rad_dt  <- dOH_rad_dt  - r68_extra

  # Caffeine reactions
  rcaffeine_OH <- 6.9e9 * caffeine * OH_rad
  dcaffeine_dt <- dcaffeine_dt - rcaffeine_OH
  dOH_rad_dt   <- dOH_rad_dt   - rcaffeine_OH
  # Sucralose reactions
  rsucralose_OH <- 1.6e9 * sucralose * OH_rad
  dsucralose_dt <- dsucralose_dt - rsucralose_OH
  dOH_rad_dt    <- dOH_rad_dt    - rsucralose_OH

  # -----------------------------------------------------------------------
  # TABLE A-1 Reactions (R71-R89)
  # -----------------------------------------------------------------------

  # R88: NH4+ -> NH3 + H+
  r88 <- 42 * NH4
  dNH4_dt    <- dNH4_dt    - r88
  dNH3_dt    <- dNH3_dt    + r88
  dH_plus_dt <- dH_plus_dt + r88

  # R89: NH3 + H+ -> NH4+
  r89 <- 8.4e10 * NH3 * H_plus
  dNH3_dt    <- dNH3_dt    - r89
  dH_plus_dt <- dH_plus_dt - r89
  dNH4_dt    <- dNH4_dt    + r89

  # -----------------------------------------------------------------------
  # TABLE C-1: Initiation and Radical Reactions
  # -----------------------------------------------------------------------

  # R_C5: H2O2 + hv -> 2OH•
  r_C5 <- pf * (a_H2O2 / a_total) * phi_H2O2
  dH2O2_dt   <- dH2O2_dt   - r_C5
  dOH_rad_dt <- dOH_rad_dt + 2 * r_C5

  # R_C6: HO2- + H2O + hv -> 2OH• + OH-
  r_C6 <- pf * (a_HO2_minus / a_total) * phi_HO2_rad
  dHO2_minus_dt <- dHO2_minus_dt - r_C6
  dOH_rad_dt    <- dOH_rad_dt    + 2 * r_C6
  dOH_minus_dt  <- dOH_minus_dt  + r_C6

  # R_C11: OH• + doc -> products
  r_C11 <- 3.0e8 * OH_rad * doc
  dOH_rad_dt <- dOH_rad_dt - r_C11
  ddoc_dt    <- ddoc_dt    - r_C11

  # R_C13: OH• + CO3(2-) -> CO3•- + OH-
  r_C13 <- 3.9e8 * OH_rad * CO3_2
  dOH_rad_dt   <- dOH_rad_dt   - r_C13
  dCO3_rad_dt  <- dCO3_rad_dt  + r_C13
  dOH_minus_dt <- dOH_minus_dt + r_C13

  # R_C16: OH• + O•- -> HO2-
  r_C16 <- 1.0e10 * OH_rad * O_rad_minus
  dOH_rad_dt      <- dOH_rad_dt      - r_C16
  dO_rad_minus_dt <- dO_rad_minus_dt - r_C16
  dHO2_minus_dt   <- dHO2_minus_dt   + r_C16

  # R_C17: OH• + HO2- -> HO2• + OH-
  r_C17 <- 7.5e9 * OH_rad * HO2_minus
  dOH_rad_dt   <- dOH_rad_dt   - r_C17
  dHO2_rad_dt  <- dHO2_rad_dt  + r_C17
  dOH_minus_dt <- dOH_minus_dt + r_C17

  # R_C19: OH• + H2O2 -> HO2• + H2O
  r_C19 <- 3.2e7 * OH_rad * H2O2
  dOH_rad_dt  <- dOH_rad_dt  - r_C19
  dH2O2_dt    <- dH2O2_dt    - r_C19
  dHO2_rad_dt <- dHO2_rad_dt + r_C19

  # R_C23: OH• + O2•- -> OH- + O2
  r_C23 <- 7.0e9 * OH_rad * O2_super
  dOH_rad_dt   <- dOH_rad_dt   - r_C23
  dO2_super_dt <- dO2_super_dt - r_C23
  dOH_minus_dt <- dOH_minus_dt + r_C23
  dO2_dt       <- dO2_dt       + r_C23

  # R_C1_24: OH• + HPO4(2-) -> HPO4•- + OH-
  r_C1_24 <- 1.5e5 * OH_rad * HPO4_2
  dOH_rad_dt   <- dOH_rad_dt   - r_C1_24
  dHPO4_2_dt   <- dHPO4_2_dt   - r_C1_24
  dHPO4_rad_dt <- dHPO4_rad_dt + r_C1_24
  dOH_minus_dt <- dOH_minus_dt + r_C1_24

  # R_C1_25: OH• + H2PO4- -> HPO4•- + OH-
  r_C1_25 <- 2.0e4 * OH_rad * H2PO4_minus
  dOH_rad_dt      <- dOH_rad_dt      - r_C1_25
  dH2PO4_minus_dt <- dH2PO4_minus_dt - r_C1_25
  dHPO4_rad_dt    <- dHPO4_rad_dt    + r_C1_25

  # R62 (C-1): CO3•- + HO2- -> HCO3- + O2•-
  r62 <- 3.0e7 * CO3_rad * HO2_minus
  dCO3_rad_dt    <- dCO3_rad_dt    - r62
  dHO2_minus_dt  <- dHO2_minus_dt  - r62
  dHCO3_minus_dt <- dHCO3_minus_dt + r62
  dO2_super_dt   <- dO2_super_dt   + r62

  # R68 (C-1): O•- + HO2- -> O2•- + OH-
  r68 <- 1.0e8 * O_rad_minus * HO2_minus
  dO_rad_minus_dt <- dO_rad_minus_dt - r68
  dHO2_minus_dt   <- dHO2_minus_dt   - r68
  dO2_super_dt    <- dO2_super_dt    + r68
  dOH_minus_dt    <- dOH_minus_dt    + r68

  # R69 (C-1): O•- + O2 -> O3•-
  r69 <- 3.6e9 * O_rad_minus * O2
  dO_rad_minus_dt  <- dO_rad_minus_dt  - r69
  dO2_dt           <- dO2_dt           - r69
  dO3_rad_minus_dt <- dO3_rad_minus_dt + r69

  # R71 (C-1): O•- + H2O2 -> O2•- + H2O
  r71c <- 4.0e8 * O_rad_minus * H2O2
  dO_rad_minus_dt <- dO_rad_minus_dt - r71c
  dH2O2_dt        <- dH2O2_dt        - r71c
  dO2_super_dt    <- dO2_super_dt    + r71c

  # R73 (C-1): O2•- + H+ -> HO2•
  r73c <- 5.0e10 * O2_super * H_plus
  dO2_super_dt <- dO2_super_dt - r73c
  dH_plus_dt   <- dH_plus_dt   - r73c
  dHO2_rad_dt  <- dHO2_rad_dt  + r73c

  # R76 (C-1): O3•- -> O•- + O2
  r76c <- 2.6e3 * O3_rad
  dO3_rad_minus_dt <- dO3_rad_minus_dt - r76c
  dO_rad_minus_dt  <- dO_rad_minus_dt  + r76c
  dO2_dt           <- dO2_dt           + r76c

  # R77 (C-1): HO2• + H2O2 -> OH• + H2O + O2
  r77c <- 3.0 * HO2_rad * H2O2
  dHO2_rad_dt <- dHO2_rad_dt - r77c
  dH2O2_dt    <- dH2O2_dt    - r77c
  dOH_rad_dt  <- dOH_rad_dt  + r77c
  dO2_dt      <- dO2_dt      + r77c

  # R78 (C-1): HO2• + O2•- -> HO2- + O2
  r78c <- 9.7e7 * HO2_rad * O2_super
  dHO2_rad_dt   <- dHO2_rad_dt   - r78c
  dO2_super_dt  <- dO2_super_dt  - r78c
  dHO2_minus_dt <- dHO2_minus_dt + r78c
  dO2_dt        <- dO2_dt        + r78c

  # R79 (C-1): HO2• -> H+ + O2•-
  r79c <- 7.0e5 * HO2_rad
  dHO2_rad_dt  <- dHO2_rad_dt  - r79c
  dH_plus_dt   <- dH_plus_dt   + r79c
  dO2_super_dt <- dO2_super_dt + r79c

  # R81 (C-1): HO2• + HO2• -> H2O2 + O2
  r81c <- 8.3e5 * HO2_rad^2
  dHO2_rad_dt <- dHO2_rad_dt - 2 * r81c
  dH2O2_dt    <- dH2O2_dt    + r81c
  dO2_dt      <- dO2_dt      + r81c

  # -----------------------------------------------------------------------
  # TABLE C3-2: Chlorine Photolysis Chemistry
  # -----------------------------------------------------------------------

  # -----------------------------------------------------------------------
  # TABLE C3-3
  # -----------------------------------------------------------------------

  # R54: OH• + HO2• -> O2 + H2O
  r_C33_54 <- 6.60e9 * OH_rad * HO2_rad
  dOH_rad_dt  <- dOH_rad_dt  - r_C33_54
  dHO2_rad_dt <- dHO2_rad_dt - r_C33_54
  dO2_dt      <- dO2_dt      + r_C33_54

  # -----------------------------------------------------------------------
  # TABLE C3-8: HO2• and O2•- Reactions
  # -----------------------------------------------------------------------

  # R137: HO2• + H2O2 -> O2 + OH• + H2O
  r_C38_137 <- 3.00 * H2O2 * HO2_rad
  dH2O2_dt    <- dH2O2_dt    - r_C38_137
  dHO2_rad_dt <- dHO2_rad_dt - r_C38_137
  dOH_rad_dt  <- dOH_rad_dt  + r_C38_137
  dO2_dt      <- dO2_dt      + r_C38_137

  # R138: HO2• + O2•- -> O2 + HO2-
  r_C38_138 <- 9.70e7 * HO2_rad * O2_super
  dHO2_rad_dt   <- dHO2_rad_dt   - r_C38_138
  dO2_super_dt  <- dO2_super_dt  - r_C38_138
  dHO2_minus_dt <- dHO2_minus_dt + r_C38_138
  dO2_dt        <- dO2_dt        + r_C38_138

  # R141: O2•- + H2O2 -> O2 + OH• + OH-
  r_C38_141 <- 1.30e-1 * O2_super * H2O2
  dO2_super_dt <- dO2_super_dt - r_C38_141
  dH2O2_dt     <- dH2O2_dt     - r_C38_141
  dOH_rad_dt   <- dOH_rad_dt   + r_C38_141
  dO2_dt       <- dO2_dt       + r_C38_141
  dOH_minus_dt <- dOH_minus_dt + r_C38_141

  # R143: O2•- + CO3•- -> CO3(2-) + O2
  r_C38_143 <- 6.00e8 * O2_super * CO3_rad
  dO2_super_dt <- dO2_super_dt - r_C38_143
  dCO3_rad_dt  <- dCO3_rad_dt  - r_C38_143
  dCO3_2_dt    <- dCO3_2_dt    + r_C38_143
  dO2_dt       <- dO2_dt       + r_C38_143


  # -----------------------------------------------------------------------
  # TABLE C3-9: CO3•- Reactions
  # -----------------------------------------------------------------------

  # R146: CO3•- + H2O2 -> HCO3- + HO2•
  r_C39_146 <- 4.30e5 * CO3_rad * H2O2
  dCO3_rad_dt    <- dCO3_rad_dt    - r_C39_146
  dHCO3_minus_dt <- dHCO3_minus_dt + r_C39_146
  dHO2_rad_dt    <- dHO2_rad_dt    + r_C39_146

  # R147: CO3•- + HO2- -> CO3(2-) + HO2•
  r_C39_147 <- 3.00e7 * CO3_rad * HO2_minus
  dCO3_rad_dt <- dCO3_rad_dt - r_C39_147
  dCO3_2_dt   <- dCO3_2_dt   + r_C39_147
  dHO2_rad_dt <- dHO2_rad_dt + r_C39_147

  # R149: CO3•- + NOM -> product
  r_C39_149 <- 6.96e5 * CO3_rad * NOM
  dCO3_rad_dt <- dCO3_rad_dt - r_C39_149

  # -----------------------------------------------------------------------
  # TABLE C3-10: Equilibrium Reactions
  # -----------------------------------------------------------------------

  # R151: H2O2 -> H+ + HO2-
  r_C310_151 <- 1.26e-1 * H2O2
  dH2O2_dt      <- dH2O2_dt      - r_C310_151
  dH_plus_dt    <- dH_plus_dt    + r_C310_151
  dHO2_minus_dt <- dHO2_minus_dt + r_C310_151

  # R152: H+ + HO2- -> H2O2
  r_C310_152 <- 5.00e10 * H_plus * HO2_minus
  dH_plus_dt    <- dH_plus_dt    - r_C310_152
  dHO2_minus_dt <- dHO2_minus_dt - r_C310_152
  dH2O2_dt      <- dH2O2_dt      + r_C310_152

  # R164: HCO3- + H+ -> H2CO3
  r_C310_164 <- 5.00e10 * HCO3_minus * H_plus
  dHCO3_minus_dt <- dHCO3_minus_dt - r_C310_164
  dH_plus_dt     <- dH_plus_dt     - r_C310_164
  dH2CO3_dt      <- dH2CO3_dt      + r_C310_164

  # R165: H2CO3 -> HCO3- + H+
  r_C310_165 <- 5.00e5 * H2CO3
  dH2CO3_dt      <- dH2CO3_dt      - r_C310_165
  dHCO3_minus_dt <- dHCO3_minus_dt + r_C310_165
  dH_plus_dt     <- dH_plus_dt     + r_C310_165

  # R166: CO3(2-) + H+ -> HCO3-
  r_C310_166 <- 5.00e10 * CO3_2 * H_plus
  dCO3_2_dt      <- dCO3_2_dt      - r_C310_166
  dH_plus_dt     <- dH_plus_dt     - r_C310_166
  dHCO3_minus_dt <- dHCO3_minus_dt + r_C310_166

  # R167: HCO3- -> CO3(2-) + H+
  r_C310_167 <- 2.50 * HCO3_minus
  dHCO3_minus_dt <- dHCO3_minus_dt - r_C310_167
  dCO3_2_dt      <- dCO3_2_dt      + r_C310_167
  dH_plus_dt     <- dH_plus_dt     + r_C310_167

  # -----------------------------------------------------------------------
  # TABLE C3-11: O3 and O3•- Reactions
  # -----------------------------------------------------------------------

  # R169: O(3P) + O2 -> O3
  r_C311_169 <- 4.00e9 * O3P * O2
  dO3P_dt    <- dO3P_dt    - r_C311_169
  dO2_dt     <- dO2_dt     - r_C311_169
  dO3_var_dt <- dO3_var_dt + r_C311_169

  # R170: O3 -> O(3P) + O2
  r_C311_170 <- 4.50e-6 * O3_var
  dO3_var_dt <- dO3_var_dt - r_C311_170
  dO3P_dt    <- dO3P_dt    + r_C311_170
  dO2_dt     <- dO2_dt     + r_C311_170

  # R177: O3 + OH• -> O2 + HO2•
  r_C311_177 <- 1.10e8 * O3_var * OH_rad
  dO3_var_dt  <- dO3_var_dt  - r_C311_177
  dOH_rad_dt  <- dOH_rad_dt  - r_C311_177
  dO2_dt      <- dO2_dt      + r_C311_177
  dHO2_rad_dt <- dHO2_rad_dt + r_C311_177

  # R178: O3 + OH- -> O2 + HO2-
  r_C311_178 <- 14.2 * O3_var * OH_minus
  dO3_var_dt    <- dO3_var_dt    - r_C311_178
  dOH_minus_dt  <- dOH_minus_dt  - r_C311_178
  dO2_dt        <- dO2_dt        + r_C311_178
  dHO2_minus_dt <- dHO2_minus_dt + r_C311_178

  # R179: O3 + H• -> O2 + OH•
  r_C311_179 <- 2.20e10 * O3_var * H_atom
  dO3_var_dt <- dO3_var_dt - r_C311_179
  dH_atom_dt <- dH_atom_dt - r_C311_179
  dO2_dt     <- dO2_dt     + r_C311_179
  dOH_rad_dt <- dOH_rad_dt + r_C311_179

  # R180: O3 + HO2• -> O2 + H+ + O3•-
  r_C311_180 <- 1.60e9 * O3_var * HO2_rad
  dO3_var_dt       <- dO3_var_dt       - r_C311_180
  dHO2_rad_dt      <- dHO2_rad_dt      - r_C311_180
  dO3_rad_minus_dt <- dO3_rad_minus_dt + r_C311_180
  dH_plus_dt       <- dH_plus_dt       + r_C311_180
  dO2_dt           <- dO2_dt           + r_C311_180

  # R181: O3 + HO2- -> O3•- + HO2•
  r_C311_181 <- 5.50e6 * O3_var * HO2_minus
  dO3_var_dt       <- dO3_var_dt       - r_C311_181
  dHO2_minus_dt    <- dHO2_minus_dt    - r_C311_181
  dO3_rad_minus_dt <- dO3_rad_minus_dt + r_C311_181
  dHO2_rad_dt      <- dHO2_rad_dt      + r_C311_181

  # R182: O3•- + OH• -> O2•- + HO2•
  r_C311_182 <- 8.50e9 * O3_rad_minus * OH_rad
  dO3_rad_minus_dt <- dO3_rad_minus_dt - r_C311_182
  dOH_rad_dt       <- dOH_rad_dt       - r_C311_182
  dO2_super_dt     <- dO2_super_dt     + r_C311_182
  dHO2_rad_dt      <- dHO2_rad_dt      + r_C311_182

  # R183: O3•- + H+ -> O2 + OH•
  r_C311_183 <- 5.20e10 * O3_rad_minus * H_plus
  dO3_rad_minus_dt <- dO3_rad_minus_dt - r_C311_183
  dH_plus_dt       <- dH_plus_dt       - r_C311_183
  dO2_dt           <- dO2_dt           + r_C311_183
  dOH_rad_dt       <- dOH_rad_dt       + r_C311_183

  # R185: O3•- -> O2 + O•-
  r_C311_185 <- 4.28e3 * O3_rad_minus
  dO3_rad_minus_dt <- dO3_rad_minus_dt - r_C311_185
  dO2_dt           <- dO2_dt           + r_C311_185
  dO_rad_minus_dt  <- dO_rad_minus_dt  + r_C311_185

  # R186: O3•- + O3•- -> product
  r_C311_186 <- 9.00e8 * O3_rad_minus^2
  dO3_rad_minus_dt <- dO3_rad_minus_dt - r_C311_186

  # R187: O3•- + O•- -> 2O2•-
  r_C311_187 <- 7.00e8 * O3_rad_minus * O_rad_minus
  dO3_rad_minus_dt <- dO3_rad_minus_dt - r_C311_187
  dO2_super_dt     <- dO2_super_dt     + r_C311_187

  # R188: O2•- + O3 -> O3•- + O2
  r_C311_188 <- 1.60e9 * O2_super * O3_var
  dO2_super_dt     <- dO2_super_dt     - r_C311_188
  dO3_rad_minus_dt <- dO3_rad_minus_dt + r_C311_188
  dO2_dt           <- dO2_dt           + r_C311_188

  # -----------------------------------------------------------------------
  # Background OH scavenging and OH exposure
  # -----------------------------------------------------------------------
  r_background_scavenging <- params$a_background * OH_rad
  dOH_rad_dt     <- dOH_rad_dt     - r_background_scavenging
  dOHexposure_dt <- OH_rad

  # -----------------------------------------------------------------------
  # Carbamazepine reactions
  # -----------------------------------------------------------------------
  rcarbamazepine_OH <- 8.8e9 * carbamazepine * OH_rad
  dcarbamazepine_dt <- dcarbamazepine_dt - rcarbamazepine_OH
  dOH_rad_dt        <- dOH_rad_dt        - rcarbamazepine_OH

  # -----------------------------------------------------------------------
  # Assemble derivative vector (64 elements)
  # NOTE: positions 56 and 59 both use dO3_rad_minus_dt, matching original
  # MATLAB (position 56 in MATLAB was labelled O3_rad but used dO3_rad_minus_dt)
  # -----------------------------------------------------------------------
  dydt[0]<-dNH3_dt
  dydt[1]<-dNH4_dt
  dydt[2]<-dH2O2_dt
  dydt[3]<-dOH_rad_dt
  dydt[4]<-dHO2_rad_dt
  dydt[5]<-dHO2_minus_dt      
  dydt[6]<-dO3P_dt
  dydt[7]<-dO1D_dt
  dydt[8]<-dNOH_dt
  dydt[9]<-dNO_rad_dt
  dydt[10]<-dNO2_rad_dt
  dydt[11]<-dNO2_minus_dt
  dydt[12]<-dN2O3_dt
  dydt[13]<-dN2O4_dt
  dydt[14]<-dNO3_minus_dt
  dydt[15]<-ddioxane_dt
  dydt[16]<-ddoc_dt
  dydt[17]<-dO2_dt
  dydt[18]<-dH_plus_dt
  dydt[19]<-dOH_minus_dt
  dydt[20]<-dHCO3_minus_dt
  dydt[21]<-dH2CO3_dt
  dydt[22]<-dCO3_2_dt
  dydt[23]<-dCO3_rad_dt
  dydt[24]<-dN2_dt
  dydt[25]<-dO_rad_minus_dt
  dydt[26]<-dHPO4_2_dt
  dydt[27]<-dH2PO4_minus_dt
  dydt[28]<-dHPO4_rad_dt
  dydt[29]<-dO2_super_dt
  dydt[30]<-dO3_var_dt
  dydt[31]<-dO3_rad_minus_dt
  dydt[32]<-dH_atom_dt
  dydt[33]<-dNOM_dt
  dydt[34]<-dcaffeine_dt
  dydt[35]<-dsucralose_dt
  dydt[36]<-dOHexposure_dt
  dydt[37]<-dcarbamazepine_dt
  dydt[36]<-dcarbamazepine_dt

  # Scale by time unit
  # t_scale <- t_f[[app$t_unit]]
  # dydt    <- t_scale * dydt

  return(list(dydt))
  })
  }

