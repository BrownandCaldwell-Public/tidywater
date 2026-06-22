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

  # species 
    
  TIC_molesL<- water@toc *1.22/61000

   H<-10^(-water@ph)

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
    # Ka1 <- 10^(-2.1)
    # Ka2 <- 10^(-7.2)
    # Ka3 <- 10^(-12.3)

    # # same here 
    # D <- 1 + (Ka1/H) + (Ka1*Ka2)/(H^2) + (Ka1*Ka2*Ka3)/(H^3)

    #phosphate species in moles/L
#!!!
# H3PO4  <- Phosphate_moles / D
# H2PO4  <- Phosphate_moles * (Ka1/H) / D
# HPO4   <- Phosphate_moles * (Ka1*Ka2)/(H^2) / D
# PO4    <- Phosphate_moles * (Ka1*Ka2*Ka3)/(H^3) / D

# code then inits the state vector for the ode solver, but I expect that this will be different when we incorporate water objects
# or maybe this is output water, which I wil say it is for now. 

output_water <- water

  state<-c(
    NH3 =water@tot_nh3,
    NH4 = water@tot_nh3 / 14000,
    H2O2 = dose / (34 * 1000), # convert mg/L to mol/L
    OH_rad = 10^(-14+water@ph),
    HO2_rad = 0,
    HO2_minus = 0,
    O3P=0,
    O1D=0,
    NOH=0,
    NO_rad=0,
    NO2_rad=0,
    NO2_minus=0,
    N2O3=0,
    N2O4=0,
    NO3_minus=0,
    dioxane=0,
    doc=water@doc,
    O2=0,
    H=10^(-water@ph),
    OH_minus=10^(-14+water@ph),
    HCO3_minus=water@hco3,
    H2CO3=water@h2co3,
    CO3_2=water@co3,
    NH2O2_rad=0,
    N2O=0,
    CO3_rad=0,
    N2=0,
    O_rad_minus=0,
    HPO4_2=water@hpo4,
    H2PO4_minus=water@h2po4,
    HPO4_rad=0,
    O2_super=0,
    O3_var=0,
    O3_rad=0,
    O3_rad_minus=0,
    H_atom=10^(-water@ph),
    H_plus=10^(-water@ph),
    #these are the same, probably wrong
    NOM=0,
    caffeine=0,
    sucralose=0,
    OHexposure=0,
    carbamazepine=0
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
  func = merged_ODE , 
  parms = params,
  method= "lsoda")


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

merged_ODE <- function(t, y, params) {
  # # Extract app and t_f from parms (deSolve passes a single parms argument)

  with(as.list(c(y,params)), {

  dydt <-rep(0,length(y))  # Initialize derivatives to zero
    
  # Absorbance coefficients (M-1 cm-1)
  a_H2O2       <- 18   * H2O2
  a_HO2_minus  <- 228  * HO2_minus
  a_background <- params$a_background
  a_sucralose  <- 14.7 * sucralose

  a_total <- a_H2O2 + a_HO2_minus + a_sucralose + a_background / L

  # Photolysis prefactor
  pf <- (E0 / L) * (1 - 10^(-a_total * L))

  # Quantum yields
  phi_H2O2      <- 0.5
  phi_HO2_rad   <- 0.5
  phi_sucralose <- 0.49


  # -----------------------------------------------------------------------
  # Global parameters
  # -----------------------------------------------------------------------
  L  <- params$L
  V  <- params$V
  A  <- params$A
  E0 <- params$E0

  # R25: NH2O2• -> NO• + H2O
  r25 <- 1.0e8 * NH2O2_rad
  dydt["NH2O2_rad"] <- dydt["NH2O2_rad"] - r25
  dydt["NO_rad"]    <- dydt["NO_rad"]    + r25


  # R27: NH2O2• -> N2O
  r27 <- 5.98e8 * NH2O2_rad
  dydt["NH2O2_rad"] <- dydt["NH2O2_rad"] - r27
  dydt["N2O"]       <- dydt["N2O"]       + r27

  # R29: NO• + OH• -> NO2- + H+
  r29 <- 1.0e10 * NO_rad * OH_rad
  dydt["NO_rad"]    <- dydt["NO_rad"]    - r29
  dydt["OH_rad"]    <- dydt["OH_rad"]    - r29
  dydt["NO2_minus"] <- dydt["NO2_minus"] + r29
  dydt["H_plus"]    <- dydt["H_plus"]    + r29

  # R30: NO2- + OH• -> NO2• + OH-
  r30 <- 1.2e10 * NO2_minus * OH_rad
  dydt["NO2_minus"] <- dydt["NO2_minus"] - r30
  dydt["OH_rad"]    <- dydt["OH_rad"]    - r30
  dydt["NO2_rad"]   <- dydt["NO2_rad"]   + r30
  dydt["OH_minus"]  <- dydt["OH_minus"]  + r30

  # R31: 2NO• + O2 -> 2NO2•
  r31 <- 2.1e6 * NO_rad^2 * O2
  dydt["NO_rad"]  <- dydt["NO_rad"]  - 2 * r31
  dydt["NO2_rad"] <- dydt["NO2_rad"] + 2 * r31

  # R32: NO• + NO2• -> N2O3
  r32 <- 1.1e9 * NO_rad * NO2_rad
  dydt["NO_rad"]  <- dydt["NO_rad"]  - r32
  dydt["NO2_rad"] <- dydt["NO2_rad"] - r32
  dydt["N2O3"]    <- dydt["N2O3"]    + r32

  # R33: N2O3 -> NO• + NO2•
  r33 <- 4.3e6 * N2O3
  dydt["N2O3"]    <- dydt["N2O3"]    - r33
  dydt["NO_rad"]  <- dydt["NO_rad"]  + r33
  dydt["NO2_rad"] <- dydt["NO2_rad"] + r33

  # R34: N2O3 + H2O -> 2NO2- + 2H+
  r34 <- 1.6e3 * N2O3
  dydt["N2O3"]      <- dydt["N2O3"]      - r34
  dydt["NO2_minus"] <- dydt["NO2_minus"] + 2 * r34
  dydt["H_plus"]    <- dydt["H_plus"]    + 2 * r34

  # R35: 2NO2• -> N2O4
  r35 <- 4.5e8 * NO2_rad^2
  dydt["NO2_rad"] <- dydt["NO2_rad"] - 2 * r35
  dydt["N2O4"]    <- dydt["N2O4"]    + r35

  # R36: N2O4 + H2O -> NO2- + NO3- + 2H+
  r36 <- 1.0e3 * N2O4
  dydt["N2O4"]      <- dydt["N2O4"]      - r36
  dydt["NO2_minus"] <- dydt["NO2_minus"] + r36
  dydt["NO3_minus"] <- dydt["NO3_minus"] + r36
  dydt["H_plus"]    <- dydt["H_plus"]    + 2 * r36

    #hydrogeen and oxygen radical reactions

  # R37: H2O -> H+ + OH-
  r37_extra <- 1.00e-3
  dydt["H_plus"]   <- dydt["H_plus"]   + r37_extra
  dydt["OH_minus"] <- dydt["OH_minus"] + r37_extra

  # R38: H+ + OH- -> H2O
  r38_extra <- 1.00e11 * H_plus * OH_minus
  dydt["H_plus"]   <- dydt["H_plus"]   - r38_extra
  dydt["OH_minus"] <- dydt["OH_minus"] - r38_extra

  # R39: OH• + OH• -> H2O2
  r39_extra <- 5.50e9 * OH_rad^2
  dydt["OH_rad"] <- dydt["OH_rad"] - 2 * r39_extra
  dydt["H2O2"]   <- dydt["H2O2"]   + r39_extra

  # R40: OH• + OH- -> O•- + H2O
  r40_extra <- 1.20e10 * OH_rad * OH_minus
  dydt["OH_rad"]      <- dydt["OH_rad"]      - r40_extra
  dydt["O_rad_minus"] <- dydt["O_rad_minus"] + r40_extra

  # R64: OH• + HCO3- -> CO3•- + H2O
  r64_extra <- 8.50e6 * OH_rad * HCO3_minus
  dydt["OH_rad"]     <- dydt["OH_rad"]     - r64_extra
  dydt["HCO3_minus"] <- dydt["HCO3_minus"] - r64_extra
  dydt["CO3_rad"]    <- dydt["CO3_rad"]    + r64_extra

  # R65: OH• + H2CO3 -> CO3•- + H2O + H+
  r65_extra <- 1.00e6 * OH_rad * H2CO3
  dydt["OH_rad"]  <- dydt["OH_rad"]  - r65_extra
  dydt["H2CO3"]   <- dydt["H2CO3"]   - r65_extra
  dydt["CO3_rad"] <- dydt["CO3_rad"] + r65_extra
  dydt["H_plus"]  <- dydt["H_plus"]  + r65_extra

  # R66: OH• + CO3•- -> products
  r66_extra <- 3.00e9 * OH_rad * CO3_rad
  dydt["OH_rad"]  <- dydt["OH_rad"]  - r66_extra
  dydt["CO3_rad"] <- dydt["CO3_rad"] - r66_extra

  # R67: CO3•- + CO3•- -> products
  r67_extra <- 3.00e7 * CO3_rad^2
  dydt["CO3_rad"] <- dydt["CO3_rad"] - r67_extra

  # R68: 1,4-dioxane + OH• -> products
  r68_extra <- 3.1e9 * dioxane * OH_rad
  dydt["dioxane"] <- dydt["dioxane"] - r68_extra
  dydt["OH_rad"]  <- dydt["OH_rad"]  - r68_extra

  # Caffeine reactions
  rcaffeine_OH <- 6.9e9 * caffeine * OH_rad
  dydt["caffeine"] <- dydt["caffeine"] - rcaffeine_OH
  dydt["OH_rad"]   <- dydt["OH_rad"]   - rcaffeine_OH
  # Sucralose reactions
  rsucralose_OH <- 1.6e9 * sucralose * OH_rad
  dydt["sucralose"] <- dydt["sucralose"] - rsucralose_OH
  dydt["OH_rad"]    <- dydt["OH_rad"]    - rsucralose_OH

  # -----------------------------------------------------------------------
  # TABLE A-1 Reactions (R71-R89)
  # -----------------------------------------------------------------------

  # R88: NH4+ -> NH3 + H+
  r88 <- 42 * NH4
  dydt["NH4"]    <- dydt["NH4"]    - r88
  dydt["NH3"]    <- dydt["NH3"]    + r88
  dydt["H_plus"] <- dydt["H_plus"] + r88

  # R89: NH3 + H+ -> NH4+
  r89 <- 8.4e10 * NH3 * H_plus
  dydt["NH3"]    <- dydt["NH3"]    - r89
  dydt["H_plus"] <- dydt["H_plus"] - r89
  dydt["NH4"]    <- dydt["NH4"]    + r89

  # -----------------------------------------------------------------------
  # TABLE C-1: Initiation and Radical Reactions
  # -----------------------------------------------------------------------

  # R_C5: H2O2 + hv -> 2OH•
  r_C5 <- pf * (a_H2O2 / a_total) * phi_H2O2
  dydt["H2O2"]   <- dydt["H2O2"]   - r_C5
  dydt["OH_rad"] <- dydt["OH_rad"] + 2 * r_C5

  # R_C6: HO2- + H2O + hv -> 2OH• + OH-
  r_C6 <- pf * (a_HO2_minus / a_total) * phi_HO2_rad
  dydt["HO2_minus"] <- dydt["HO2_minus"] - r_C6
  dydt["OH_rad"]    <- dydt["OH_rad"]    + 2 * r_C6
  dydt["OH_minus"]  <- dydt["OH_minus"]  + r_C6

  # R_C11: OH• + doc -> products
  r_C11 <- 3.0e8 * OH_rad * doc
  dydt["OH_rad"] <- dydt["OH_rad"] - r_C11
  dydt["doc"]    <- dydt["doc"]    - r_C11

  # R_C13: OH• + CO3(2-) -> CO3•- + OH-
  r_C13 <- 3.9e8 * OH_rad * CO3_2
  dydt["OH_rad"]   <- dydt["OH_rad"]   - r_C13
  dydt["CO3_rad"]  <- dydt["CO3_rad"]  + r_C13
  dydt["OH_minus"] <- dydt["OH_minus"] + r_C13

  # R_C16: OH• + O•- -> HO2-
  r_C16 <- 1.0e10 * OH_rad * O_rad_minus
  dydt["OH_rad"]      <- dydt["OH_rad"]      - r_C16
  dydt["O_rad_minus"] <- dydt["O_rad_minus"] - r_C16
  dydt["HO2_minus"]   <- dydt["HO2_minus"]   + r_C16

  # R_C17: OH• + HO2- -> HO2• + OH-
  r_C17 <- 7.5e9 * OH_rad * HO2_minus
  dydt["OH_rad"]   <- dydt["OH_rad"]   - r_C17
  dydt["HO2_rad"]  <- dydt["HO2_rad"]  + r_C17
  dydt["OH_minus"] <- dydt["OH_minus"] + r_C17

  # R_C19: OH• + H2O2 -> HO2• + H2O
  r_C19 <- 3.2e7 * OH_rad * H2O2
  dydt["OH_rad"]  <- dydt["OH_rad"]  - r_C19
  dydt["H2O2"]    <- dydt["H2O2"]    - r_C19
  dydt["HO2_rad"] <- dydt["HO2_rad"] + r_C19

  # R_C23: OH• + O2•- -> OH- + O2
  r_C23 <- 7.0e9 * OH_rad * O2_super
  dydt["OH_rad"]   <- dydt["OH_rad"]   - r_C23
  dydt["O2_super"] <- dydt["O2_super"] - r_C23
  dydt["OH_minus"] <- dydt["OH_minus"] + r_C23
  dydt["O2"]       <- dydt["O2"]       + r_C23

  # R_C1_24: OH• + HPO4(2-) -> HPO4•- + OH-
  r_C1_24 <- 1.5e5 * OH_rad * HPO4_2
  dydt["OH_rad"]   <- dydt["OH_rad"]   - r_C1_24
  dydt["HPO4_2"]   <- dydt["HPO4_2"]   - r_C1_24
  dydt["HPO4_rad"] <- dydt["HPO4_rad"] + r_C1_24
  dydt["OH_minus"] <- dydt["OH_minus"] + r_C1_24

  # R_C1_25: OH• + H2PO4- -> HPO4•- + OH-
  r_C1_25 <- 2.0e4 * OH_rad * H2PO4_minus
  dydt["OH_rad"]      <- dydt["OH_rad"]      - r_C1_25
  dydt["H2PO4_minus"] <- dydt["H2PO4_minus"] - r_C1_25
  dydt["HPO4_rad"]    <- dydt["HPO4_rad"]    + r_C1_25

  # R62 (C-1): CO3•- + HO2- -> HCO3- + O2•-
  r62 <- 3.0e7 * CO3_rad * HO2_minus
  dydt["CO3_rad"]    <- dydt["CO3_rad"]    - r62
  dydt["HO2_minus"]  <- dydt["HO2_minus"]  - r62
  dydt["HCO3_minus"] <- dydt["HCO3_minus"] + r62
  dydt["O2_super"]   <- dydt["O2_super"]   + r62

  # R68 (C-1): O•- + HO2- -> O2•- + OH-
  r68 <- 1.0e8 * O_rad_minus * HO2_minus
  dydt["O_rad_minus"] <- dydt["O_rad_minus"] - r68
  dydt["HO2_minus"]   <- dydt["HO2_minus"]   - r68
  dydt["O2_super"]    <- dydt["O2_super"]    + r68
  dydt["OH_minus"]    <- dydt["OH_minus"]    + r68

  # R69 (C-1): O•- + O2 -> O3•-
  r69 <- 3.6e9 * O_rad_minus * O2
  dydt["O_rad_minus"]  <- dydt["O_rad_minus"]  - r69
  dydt["O2"]           <- dydt["O2"]           - r69
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] + r69

  # R71 (C-1): O•- + H2O2 -> O2•- + H2O
  r71c <- 4.0e8 * O_rad_minus * H2O2
  dydt["O_rad_minus"] <- dydt["O_rad_minus"] - r71c
  dydt["H2O2"]        <- dydt["H2O2"]        - r71c
  dydt["O2_super"]    <- dydt["O2_super"]    + r71c

  # R73 (C-1): O2•- + H+ -> HO2•
  r73c <- 5.0e10 * O2_super * H_plus
  dydt["O2_super"] <- dydt["O2_super"] - r73c
  dydt["H_plus"]   <- dydt["H_plus"]   - r73c
  dydt["HO2_rad"]  <- dydt["HO2_rad"]  + r73c

  # R76 (C-1): O3•- -> O•- + O2
  r76c <- 2.6e3 * O3_rad
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] - r76c
  dydt["O_rad_minus"]  <- dydt["O_rad_minus"]  + r76c
  dydt["O2"]           <- dydt["O2"]           + r76c

  # R77 (C-1): HO2• + H2O2 -> OH• + H2O + O2
  r77c <- 3.0 * HO2_rad * H2O2
  dydt["HO2_rad"] <- dydt["HO2_rad"] - r77c
  dydt["H2O2"]    <- dydt["H2O2"]    - r77c
  dydt["OH_rad"]  <- dydt["OH_rad"]  + r77c
  dydt["O2"]      <- dydt["O2"]      + r77c

  # R78 (C-1): HO2• + O2•- -> HO2- + O2
  r78c <- 9.7e7 * HO2_rad * O2_super
  dydt["HO2_rad"]   <- dydt["HO2_rad"]   - r78c
  dydt["O2_super"]  <- dydt["O2_super"]  - r78c
  dydt["HO2_minus"] <- dydt["HO2_minus"] + r78c
  dydt["O2"]        <- dydt["O2"]        + r78c

  # R79 (C-1): HO2• -> H+ + O2•-
  r79c <- 7.0e5 * HO2_rad
  dydt["HO2_rad"]  <- dydt["HO2_rad"]  - r79c
  dydt["H_plus"]   <- dydt["H_plus"]   + r79c
  dydt["O2_super"] <- dydt["O2_super"] + r79c

  # R81 (C-1): HO2• + HO2• -> H2O2 + O2
  r81c <- 8.3e5 * HO2_rad^2
  dydt["HO2_rad"] <- dydt["HO2_rad"] - 2 * r81c
  dydt["H2O2"]    <- dydt["H2O2"]    + r81c
  dydt["O2"]      <- dydt["O2"]      + r81c

  # -----------------------------------------------------------------------
  # TABLE C3-2: Chlorine Photolysis Chemistry
  # -----------------------------------------------------------------------

  # -----------------------------------------------------------------------
  # TABLE C3-3
  # -----------------------------------------------------------------------

  # R54: OH• + HO2• -> O2 + H2O
  r_C33_54 <- 6.60e9 * OH_rad * HO2_rad
  dydt["OH_rad"]  <- dydt["OH_rad"]  - r_C33_54
  dydt["HO2_rad"] <- dydt["HO2_rad"] - r_C33_54
  dydt["O2"]      <- dydt["O2"]      + r_C33_54

  # -----------------------------------------------------------------------
  # TABLE C3-8: HO2• and O2•- Reactions
  # -----------------------------------------------------------------------

  # R137: HO2• + H2O2 -> O2 + OH• + H2O
  r_C38_137 <- 3.00 * H2O2 * HO2_rad
  dydt["H2O2"]    <- dydt["H2O2"]    - r_C38_137
  dydt["HO2_rad"] <- dydt["HO2_rad"] - r_C38_137
  dydt["OH_rad"]  <- dydt["OH_rad"]  + r_C38_137
  dydt["O2"]      <- dydt["O2"]      + r_C38_137

  # R138: HO2• + O2•- -> O2 + HO2-
  r_C38_138 <- 9.70e7 * HO2_rad * O2_super
  dydt["HO2_rad"]   <- dydt["HO2_rad"]   - r_C38_138
  dydt["O2_super"]  <- dydt["O2_super"]  - r_C38_138
  dydt["HO2_minus"] <- dydt["HO2_minus"] + r_C38_138
  dydt["O2"]        <- dydt["O2"]        + r_C38_138

  # R141: O2•- + H2O2 -> O2 + OH• + OH-
  r_C38_141 <- 1.30e-1 * O2_super * H2O2
  dydt["O2_super"] <- dydt["O2_super"] - r_C38_141
  dydt["H2O2"]     <- dydt["H2O2"]     - r_C38_141
  dydt["OH_rad"]   <- dydt["OH_rad"]   + r_C38_141
  dydt["O2"]       <- dydt["O2"]       + r_C38_141
  dydt["OH_minus"] <- dydt["OH_minus"] + r_C38_141

  # R143: O2•- + CO3•- -> CO3(2-) + O2
  r_C38_143 <- 6.00e8 * O2_super * CO3_rad
  dydt["O2_super"] <- dydt["O2_super"] - r_C38_143
  dydt["CO3_rad"]  <- dydt["CO3_rad"]  - r_C38_143
  dydt["CO3_2"]    <- dydt["CO3_2"]    + r_C38_143
  dydt["O2"]       <- dydt["O2"]       + r_C38_143


  # -----------------------------------------------------------------------
  # TABLE C3-9: CO3•- Reactions
  # -----------------------------------------------------------------------

  # R146: CO3•- + H2O2 -> HCO3- + HO2•
  r_C39_146 <- 4.30e5 * CO3_rad * H2O2
  dydt["CO3_rad"]    <- dydt["CO3_rad"]    - r_C39_146
  dydt["HCO3_minus"] <- dydt["HCO3_minus"] + r_C39_146
  dydt["HO2_rad"]    <- dydt["HO2_rad"]    + r_C39_146

  # R147: CO3•- + HO2- -> CO3(2-) + HO2•
  r_C39_147 <- 3.00e7 * CO3_rad * HO2_minus
  dydt["CO3_rad"] <- dydt["CO3_rad"] - r_C39_147
  dydt["CO3_2"]   <- dydt["CO3_2"]   + r_C39_147
  dydt["HO2_rad"] <- dydt["HO2_rad"] + r_C39_147

  # R149: CO3•- + NOM -> product
  r_C39_149 <- 6.96e5 * CO3_rad * NOM
  dydt["CO3_rad"] <- dydt["CO3_rad"] - r_C39_149

  # -----------------------------------------------------------------------
  # TABLE C3-10: Equilibrium Reactions
  # -----------------------------------------------------------------------

  # R151: H2O2 -> H+ + HO2-
  r_C310_151 <- 1.26e-1 * H2O2
  dydt["H2O2"]      <- dydt["H2O2"]      - r_C310_151
  dydt["H_plus"]    <- dydt["H_plus"]    + r_C310_151
  dydt["HO2_minus"] <- dydt["HO2_minus"] + r_C310_151

  # R152: H+ + HO2- -> H2O2
  r_C310_152 <- 5.00e10 * H_plus * HO2_minus
  dydt["H_plus"]    <- dydt["H_plus"]    - r_C310_152
  dydt["HO2_minus"] <- dydt["HO2_minus"] - r_C310_152
  dydt["H2O2"]      <- dydt["H2O2"]      + r_C310_152

  # R164: HCO3- + H+ -> H2CO3
  r_C310_164 <- 5.00e10 * HCO3_minus * H_plus
  dydt["HCO3_minus"] <- dydt["HCO3_minus"] - r_C310_164
  dydt["H_plus"]     <- dydt["H_plus"]     - r_C310_164
  dydt["H2CO3"]      <- dydt["H2CO3"]      + r_C310_164

  # R165: H2CO3 -> HCO3- + H+
  r_C310_165 <- 5.00e5 * H2CO3
  dydt["H2CO3"]      <- dydt["H2CO3"]      - r_C310_165
  dydt["HCO3_minus"] <- dydt["HCO3_minus"] + r_C310_165
  dydt["H_plus"]     <- dydt["H_plus"]     + r_C310_165

  # R166: CO3(2-) + H+ -> HCO3-
  r_C310_166 <- 5.00e10 * CO3_2 * H_plus
  dydt["CO3_2"]      <- dydt["CO3_2"]      - r_C310_166
  dydt["H_plus"]     <- dydt["H_plus"]     - r_C310_166
  dydt["HCO3_minus"] <- dydt["HCO3_minus"] + r_C310_166

  # R167: HCO3- -> CO3(2-) + H+
  r_C310_167 <- 2.50 * HCO3_minus
  dydt["HCO3_minus"] <- dydt["HCO3_minus"] - r_C310_167
  dydt["CO3_2"]      <- dydt["CO3_2"]      + r_C310_167
  dydt["H_plus"]     <- dydt["H_plus"]     + r_C310_167

  # -----------------------------------------------------------------------
  # TABLE C3-11: O3 and O3•- Reactions
  # -----------------------------------------------------------------------

  # R169: O(3P) + O2 -> O3
  r_C311_169 <- 4.00e9 * O3P * O2
  dydt["O3P"]    <- dydt["O3P"]    - r_C311_169
  dydt["O2"]     <- dydt["O2"]     - r_C311_169
  dydt["O3_var"] <- dydt["O3_var"] + r_C311_169

  # R170: O3 -> O(3P) + O2
  r_C311_170 <- 4.50e-6 * O3_var
  dydt["O3_var"] <- dydt["O3_var"] - r_C311_170
  dydt["O3P"]    <- dydt["O3P"]    + r_C311_170
  dydt["O2"]     <- dydt["O2"]     + r_C311_170

  # R177: O3 + OH• -> O2 + HO2•
  r_C311_177 <- 1.10e8 * O3_var * OH_rad
  dydt["O3_var"] <- dydt["O3_var"] - r_C311_177
  dydt["OH_rad"] <- dydt["OH_rad"] - r_C311_177
  dydt["O2"]     <- dydt["O2"]     + r_C311_177
  dydt["HO2_rad"] <- dydt["HO2_rad"] + r_C311_177

  # R178: O3 + OH- -> O2 + HO2-
  r_C311_178 <- 14.2 * O3_var * OH_minus
  dydt["O3_var"] <- dydt["O3_var"] - r_C311_178
  dydt["OH_minus"] <- dydt["OH_minus"] - r_C311_178
  dydt["O2"]     <- dydt["O2"]     + r_C311_178
  dydt["HO2_minus"] <- dydt["HO2_minus"] + r_C311_178

  # R179: O3 + H• -> O2 + OH•
  r_C311_179 <- 2.20e10 * O3_var * H_atom
  dydt["O3_var"] <- dydt["O3_var"] - r_C311_179
  dydt["H_atom"] <- dydt["H_atom"] - r_C311_179
  dydt["O2"]     <- dydt["O2"]     + r_C311_179
  dydt["OH_rad"] <- dydt["OH_rad"] + r_C311_179

  # R180: O3 + HO2• -> O2 + H+ + O3•-
  r_C311_180 <- 1.60e9 * O3_var * HO2_rad
  dydt["O3_var"]       <- dydt["O3_var"]       - r_C311_180
  dydt["HO2_rad"]      <- dydt["HO2_rad"]      - r_C311_180
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] + r_C311_180
  dydt["H_plus"]       <- dydt["H_plus"]       + r_C311_180
  dydt["O2"]           <- dydt["O2"]           + r_C311_180

  # R181: O3 + HO2- -> O3•- + HO2•
  r_C311_181 <- 5.50e6 * O3_var * HO2_minus
  dydt["O3_var"]       <- dydt["O3_var"]       - r_C311_181
  dydt["HO2_minus"]    <- dydt["HO2_minus"]    - r_C311_181
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] + r_C311_181
  dydt["HO2_rad"]      <- dydt["HO2_rad"]      + r_C311_181

  # R182: O3•- + OH• -> O2•- + HO2•
  r_C311_182 <- 8.50e9 * O3_rad_minus * OH_rad
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] - r_C311_182
  dydt["OH_rad"]       <- dydt["OH_rad"]       - r_C311_182
  dydt["O2_super"]     <- dydt["O2_super"]     + r_C311_182
  dydt["HO2_rad"]      <- dydt["HO2_rad"]      + r_C311_182

  # R183: O3•- + H+ -> O2 + OH•
  r_C311_183 <- 5.20e10 * O3_rad_minus * H_plus
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] - r_C311_183
  dydt["H_plus"]       <- dydt["H_plus"]       - r_C311_183
  dydt["O2"]           <- dydt["O2"]           + r_C311_183
  dydt["OH_rad"]       <- dydt["OH_rad"]       + r_C311_183

  # R185: O3•- -> O2 + O•-
  r_C311_185 <- 4.28e3 * O3_rad_minus
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] - r_C311_185
  dydt["O2"]           <- dydt["O2"]           + r_C311_185
  dydt["O_rad_minus"]  <- dydt["O_rad_minus"]  + r_C311_185

  # R186: O3•- + O3•- -> product
  r_C311_186 <- 9.00e8 * O3_rad_minus^2
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] - r_C311_186

  # R187: O3•- + O•- -> 2O2•-
  r_C311_187 <- 7.00e8 * O3_rad_minus * O_rad_minus
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] - r_C311_187
  dydt["O2_super"]     <- dydt["O2_super"]     + r_C311_187

  # R188: O2•- + O3 -> O3•- + O2
  r_C311_188 <- 1.60e9 * O2_super * O3_var
  dydt["O2_super"]     <- dydt["O2_super"]     - r_C311_188
  dydt["O3_rad_minus"] <- dydt["O3_rad_minus"] + r_C311_188
  dydt["O2"]           <- dydt["O2"]           + r_C311_188

  # -----------------------------------------------------------------------
  # Background OH scavenging and OH exposure
  # -----------------------------------------------------------------------
  r_background_scavenging <- params$a_background * OH_rad
  dydt["OH_rad"]     <- dydt["OH_rad"]     - r_background_scavenging
  dydt["OHexposure"] <- dydt["OHexposure"] + OH_rad

  # -----------------------------------------------------------------------
  # Carbamazepine reactions
  # -----------------------------------------------------------------------
  rcarbamazepine_OH <- 8.8e9 * carbamazepine * OH_rad
  dydt["carbamazepine"] <- dydt["carbamazepine"] - rcarbamazepine_OH
  dydt["OH_rad"]        <- dydt["OH_rad"]        - rcarbamazepine_OH



  return(list(dydt))
  })
  }

