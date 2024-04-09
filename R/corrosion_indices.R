# Corrosion and scaling indices
# This function calculates standard corrosion and scaling indices

#' Calculate six corrosion and scaling indices (AI, RI, LSI, LI, CSMR, CCPP)
#'
#' \code{calculate_corrosion} takes an object of class "water" created by \code{\link{define_water}} and calculates 
#' corrosion and scaling indices.
#' 
#' Aggressive Index (AI) - the corrosive tendency of water and its effect on asbestos cement pipe
#' Ryznar Index(RI) - a measure of scaling potential
#' Langelier Saturation Index (LSI) - describes the potential for calcium carbonate scale formation
#' Larson-skold Index (LI) - describes the corrosivity towards mild steel
#' Chloride-to-sulfate mass ratio (CSMR) - indicator of galvanic corrosion for lead solder pipe joints
#' Calcium carbonate precipitation potential (CCPP) - a prediction of the mass of calcium carbonate that will prepitate at equilibrium
#'
#' \code{calculate_corrosion} uses \code{uniroot} on \code{\link{chemdose_ph}} to match the required dose for the requested pH target.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param index The indices to be calculated. 
#' Default calculates all six indices: "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 10, tot_hard = 150, tds = 100, cl = 150, so4 = 200)
#'
#' @export
#'
calculate_corrosion <- function(water, index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr")) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}
  if (missing(index)) {
    stop("No corrosion/scaling index specified.")}
  if (is.na(water@tds)) {
    warning("TDS not specified. Langelier and Ryznar indices will not be calculated.")}
  if (is.na(water@ca)) {
    warning("Calcium or total hardness not specified. Aggressive, Ryznar, and Langelier indices will not be calculated.")}
  if (is.na(water@cl) | is.na(water@so4)) {
    warning("Chloride or sulfate not specified. Larson-Skold index and CSMR will not be calculated.")}
  
# for review/checks:
# downloaded this  ref from EPA https://nepis.epa.gov/Exe/ZyNET.exe/10003FIW.txt?ZyActionD=ZyDocument&Client=EPA&Index=1981%20Thru%201985&Docs=&Query=%28ryznar%29%20OR%20FNAME%3D%2210003FIW.txt%22%20AND%20FNAME%3D%2210003FIW.txt%22&Time=&EndTime=&SearchMethod=1&TocRestrict=n&Toc=&TocEntry=&QField=&QFieldYear=&QFieldMonth=&QFieldDay=&UseQField=&IntQFieldOp=0&ExtQFieldOp=0&XmlQuery=&File=D%3A%5CZYFILES%5CINDEX%20DATA%5C81THRU85%5CTXT%5C00000001%5C10003FIW.txt&User=ANONYMOUS&Password=anonymous&SortMethod=h%7C-&MaximumDocuments=1&FuzzyDegree=0&ImageQuality=r75g8/r75g8/x150y150g16/i425&Display=hpfr&DefSeekPage=x&SearchBack=ZyActionL&Back=ZyActionS&BackDesc=Results%20page&MaximumPages=1&ZyEntry=2#
  water <- define_water(ph = 7, temp = 25, alk = 10, tot_hard = 150, tds = 408, cl = 150, so4 = 200)
  
  ca_hard <- convert_units(water@ca, "ca", "M", "mg/L CaCO3")
  tempa = water@temp + 273.15
  
  #aggressive
                 # AWWA (1977) AWWA Standard for Asbestos-Cement Pressure Pipe, 4 in. through 24 in.,
                 # for Water and Other Liquids, AWWA C400-77, Rev of C400-75, American Water
                 # Works Association, Denver, CO.
  if ("aggressive" %in% index) {
    water@aggressive <- water@ph + log10(water@alk * ca_hard)
  }
  
  #CSMR
  if ("csmr" %in% index) {
    
    cl <- convert_units(water@cl, "cl", "M", "mg/L")
    so4 <- convert_units(water@so4, "so4", "M", "mg/L")
    
    water@csmr <- cl/so4
    
    if (water@alk > 50) {
      warning("Alkalinity is greater than 50 mg/L as CaCO3. CSMR less applicable to this water.")
    }
  }
  
  #larsonskold
  if ("larsonskold" %in% index) {
    # epm = equivalents per million
    # (epm Cl + epm SO4)/ (epm HCO3 + epm CO3)
    cl_meq <- convert_units(water@cl, "cl", "M", "meq/L")
    so4_meq <- convert_units(water@so4, "so4", "M", "meq/L")
    alk_meq <- water@alk_eq*1000
    
    water@larsonskold <- (cl_meq + so4_meq) / (alk_meq)
  }
  
  
  ###########################################################################################*
  ###########################################################################################*
  # LANGELIER MESS ------------------------------
  
  ##  USE AWWA TO CALC LSI ----
  
  # AWWA Spreadsheet - matches Chris's calcs pretty well
  # https://www.awwa.org/search/results?s=calculate%20ryznar
  # scroll down and click on the watermath-corrosivity exceldownloads
  # this gives equations for finding A and B (named differently than what's in the EPA doc - see above link)
  # equations are close but don't quite find the same constant value as in the EPA doc (tables 6.2, 6.3)
  # BUT EPA doesn't have the equations for these...
  
  # TDS correction, 2 options to calc (From AWWA spreadsheet)
  # A =  9.7 + ((2.5*(water@is)^0.5) / (1.0 + 5.3*(water@is)^0.5 + 5.5*water@is))
  A = (log10(water@tds) - 1) / 10
  
  # Temp correction
  # B = 2.24961-0.017853*water@temp + 8.238E-5*water@temp^2 - 4.1E-7*water@temp^3 
  B = -13.12 * log10(tempa) + 34.55
  # pH of saturation
  ph_s_awwa  = 9.3 + A + B - log10(ca_hard) + 0.4 - log10(water@alk)

  ##############################*
  # USE CHRIS' SPREADSHEET TO CALC LSI ----
  
# these activities are a bit different than what Chris has calc'd
  active_1 = calculate_activity(1, water@is, tempa) 
  active_2 = calculate_activity(2, water@is, tempa)
  alk_mol = convert_units(water@alk, "caco3", startunit = "mg/L CaCO3")
  ph_s_chris = ((2902.39 / tempa + 0.02379*tempa - 6.498) -
                  (171.9065 + 0.077993*tempa - 2839.319/tempa - 71.595 * log10(tempa)) -
                  log10(water@ca) - log10(alk_mol)- log10(active_1) - log10(active_2))
  
  ##############################*
  # USE MWH TO CALC LSI ----
  # see eq 22-30
  # Plummer, L., and Busenberg, E. (1982) "The Solubilities of Calcite Aragonite and
      # Vaterite in CO2-H2O Solutions between 0 and 90 Degrees C, and an Evaluation
      # of the Aqueous Model for the System CaCO3-CO2-H2O," Geochim. Cosmochim.
      # Acta, 46, 1, 1011–1023.
  
  # Langelier, W. (1936) "The Analytical Control of Anti-Corrosion Water Treatment,"
      # J. AWWA, 28, 11, 1500–1522.
  a_h = water@kw/water@oh #MWH eq 22-25
  K.2 = a_h*water@co3 /water@hco3   #MWH eq # 22-24
    
  log_k2 = -log10(K.2)
  
  #mixed solubility constant for CaCO3
  log_kso = 171.9065 + 0.077993*tempa - 2839.319/tempa - 71.595*log10(tempa) #From Plummer and Busenberg (1982), MWH table 22-9
  # log_kso = -log10(water@ca * water@co3) #MWH eq 22-23
  
  ph_s_mwh = log_k2 - log_kso - log10(water@ca) - log10(alk_mol) # from MWH eq 22-30
  water@ph - ph_s_mwh

  water@ph - ph_s_awwa  #-1.95
  water@ph - ph_s_chris #-1.92
  water@ph - ph_s_mwh   #-1.68
  ph_s = ph_s_chris
# NOTE!!!
  # Original paper has alk in equivalence. Chris and other use mols
  
  #langelier
  if ("langelier" %in% index | "ccpp" %in% index) {
   
    water@langelier <- water@ph - ph_s
  }
  
  #ryznar Ryznar 1944
  # Ryznar,T.(1944) "A New Index for Determining the Amount of Calcium Carbonate Scale Formed by a Water," J. AWWA, 36, 4, 472–486.
  if ("ryznar" %in% index) {
 
    water@ryznar <- 2*ph_s - water@ph
  }

  #CCPP (Merrill and Sanks, 1977a,b, 1978).
  
  # Merrill, D., and Sanks, R. (1977a) "Corrosion Control by Deposition of CaCO3
      # Films, Part I," J. AWWA, 69, 11, 592–599.
  # Merrill, D., and Sanks, R. (1977b) "Corrosion Control by Deposition of CaCO3
      # Films, Part 2," J. AWWA, 69, 12, 634–640.
  # Merrill, D., and Sanks, R. (1978) "Corrosion Control by Deposition of CaCO3 Films,
      # Part 3," J. AWWA, 70, 1, 12–18.
  
  # check against Chris's spreadsheet
  # https://legacy.azdeq.gov/environ/water/dw/download/vol_II_app_abc.pdf
  # https://awwa.onlinelibrary.wiley.com/doi/pdfdirect/10.1002/j.1551-8833.1998.tb08471.x
  # if ("ccpp" %in% index) {
  #    Ca_est = (ca_hard-water@langelier*20)/100.869/1000 # cell A821
  #    IS = water@is - 4*(water@ca - Ca_est) # cell C821
  #    
  #    # these activities don't quite match what Chris calculates
  #    activity_1 =  calculate_activity(1, IS, tempa) # cell D821
  #    activity_2 =  calculate_activity(2, IS, tempa) # cell E821
  #    activity_3 =  calculate_activity(3, IS, tempa) # cell F821
  #    tot_co3 = water@tot_co3 - (water@ca - Ca_est) # cell G821, mol/L
  #    Cb_Ca = (water@hco3 + 2*water@co3 + water@oh - water@h) - 2 * (water@ca - Ca_est) #cell H821
  #    
  #    # I think I should use solve_pH here, but this fn uses calc_alphas. Chris's spreadsheet uses activities. how to incorporate?
  #    ph = 9.9 #solve_ph(water) #cell J821
  #    h_conc = 10^-ph #cell I821 - solve_H?
  #    
  #   a_1 = calculate_alpha1_carbonate(h_conc, k) #cell K821
  #   a_2 = calculate_alpha2_carbonate(h_conc, k) #cell L821
  #   
  #   
  #   Ca_sol = (10^(-log_kso))/(activity_2^2)/a_2/tot_co3*(1+(10^-water@kw)/activity_1^2 * (10^-T48)*activity_2/h_conc +
  #                                                      (10^-T49) *activity_2*a_1*tot_co3 +
  #                                                      (10^-T50)*activity_2^2*a_2*tot_co3 +
  #                                                      (10^-T51) * activity_2^2*0)  #cell M821
  #   C1_C2 = (Ca_est - Ca_sol) * 1000000 #cell N821
  #   LHS_RHS = Ca_sol/Ca_est#cell O821
  #   Ca = convert_units(Ca_sol, "ca", "M", "mg/L CaCO3") #cell P821
  #   CCPP = ca_hard - Ca #cell Q821
  # }
  
  
  
  # using Trussell
  water <- define_water(ph = 8, temp = 25, alk = 220, ca_hard = 200)
  if ("ccpp" %in% index) {
    
    #before precipitation
    water1 <- define_water(ph = 8, temp = 25, alk = 220, ca_hard = 200) #initial pH
    ca = convert_units(water1@ca, "ca", "M", "mM")
    alk_meq = water1@alk_eq *1000 #meq/L
    #From Trussell eq S4. Charge coefficient for carbonate species in eq/mol.
    sig_co2 = (water1@hco3 + 2 * water1@co3)/water1@tot_co3 #meq/mol
    #electroneutrality shift, equation S6
    delta = (water1@oh - water1@h) *1000 #meq/L
    tot_co3 = water1@tot_co3 * 1000    # mmol/L
    Kso = ca * water1@co3*1000
    k = correct_k(water1)
    a_2 = calculate_alpha2_carbonate(water1@h, k)
    x = (tot_co3*sig_co2 -delta - alk_meq) / (sig_co2 - 2)
    pH_i = (ca - x) * (tot_co3 -x) *a_2  - Kso
    ph = 4 #initial guess
    
    # while(ph > 7) {
      
    # after precipitation
    water_loop <- define_water(ph = ph, temp = 25, alk = 220, ca_hard = 200)
    ca_loop = convert_units(water_loop@ca, "ca", "M", "mM")
    alk_meq_loop = water_loop@alk_eq *1000 #meq/L
    sig_co2_loop  = (water_loop@hco3 + 2 * water_loop@co3)/water_loop@tot_co3 #meq/mol
    delta_loop  = (water_loop@oh - water_loop@h) *1000 #meq/L
    tot_co3_loop  = water_loop@tot_co3 * 1000    # mmol/L
    Kso_loop  = ca_loop* water_loop@co3*1000
    k_loop  = correct_k(water_loop)
    a_2_loop  = calculate_alpha2_carbonate(water_loop@h, k)
    
    x_loop = (tot_co3_loop*sig_co2_loop -delta_loop - alk_meq_loop) / (sig_co2_loop - 2)
    # x= -.01

    pH_f = (ca_loop - x_loop) * (tot_co3_loop -x_loop) *a_2  - Kso_loop
    
    err = pH_i - pH_f
    err
    ph = ph - 0.1
    
      
    }
    
    
  }
  
}
