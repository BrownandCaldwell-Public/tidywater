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
water <- define_water(ph = 7, temp = 25, alk = 10, tot_hard = 150)
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
  water <- define_water(ph = 7, temp = 25, alk = 10, tot_hard = 150)
  

  ca_hard <- convert_units(water@ca, "ca", "M", "mg/L CaCO3")
  mg_hard <- convert_units(water@mg, "mg", "M", "mg/L CaCO3")
  tot_hard <- ca_hard + mg_hard
  

  # https://www.awwa.org/search/results?s=calculate%20ryznar
  # scroll down and click on the watermath-corrosivity exceldownloads
  # this gives equations for finding A and B (named differently than what's in the EPA doc)
  # equations are close but don't quite find the same constant value as in the EPA doc (tables 6.2, 6.3)
  # BUT EPA doesn't have the equations for these...
  # Trying to find A and B based on Chris's spreadsheet
  
  
  # Temp correction
  A = -13.12 * log10(water@temp + 273) + 34.55
  
  #TDS correction
  B =  9.3 + (log10(water@tds) - 1) / 10
  
  # pH of saturation
  ph_s  = A + B - log10(ca_hard) - log10(water@alk)
  #a and b  = constants related to temp and dissolved solids. are these the same as the K's we already have?

  #langelier
  if ("langelier" %in% index) {
    # ph -ph_s
    water@langelier <- water@ph - ph_s
    
    # From Chris's Spreadsheet 
    # k <- correct_k(water)
    # tempa = water@temp + 273.15
    # active_1 = calculate_activity(1, water@is, tempa) #not sure if calculating activities is what's happening in his spreadsheet
    # active_2 = calculate_activity(2, water@is, tempa) 
    # 
    # lsi = ph - (k$k2co3 -(171.9065+0.077993*tempa-2839.319/tempa-71.595*log10(tempa)) #I don't think we have this anywhere. looks like K for solid CaCO3 as calcite.
    #             - log(ca_hard) - log10(water@alk) - log(active_1) - log(active_2))
  
  }
  
  #ryznar
  if ("ryznar" %in% index) {
    # 2*pHs - pH
    water@ryznar <- 2*ph_s - water@ph
    
    # From Chris's Spreadsheet 
    # k <- correct_k(water)
    # tempa = water@temp + 273.15
    # active_1 = calculate_activity(1, water@is, tempa) 
    # active_2 = calculate_activity(2, water@is, tempa) 
    # 
    # RI = 2* (k$k2co3 -(171.9065+0.077993*tempa-2839.319/tempa-71.595*log10(tempa)) #I don't think we have this anywhere. looks like K for solid CaCO3 as calcite.
    #             - log(ca_hard) - log10(water@alk) - log(active_1) - log(active_2)) - water@ph
    
  }
  
  #aggressive
  if ("aggressive" %in% index) {
    water@aggressive <- water@ph + log10(water@alk * ca_hard)
  }
  
  #larsonskold
  # epm = equivalents per million
  # (epmCl + epm SO4)/ (epm HCO3 + epm CO3)
  
  cl_eq <- convert_units(water@cl, "cl", "M", "meq/L")
  so4_eq <- convert_units(water@so4, "so4", "M", "meq/L")
  
  if ("larsonskold" %in% index) {
    water@larsonskold <- (cl_eq + so4_eq) / (water@alk_eq)
  }
  
  #CSMR
    cl <- convert_units(water@cl, "cl", "M", "mg/L")
  so4 <- convert_units(water@so4, "so4", "M", "mg/L")
  
  if ("csmr" %in% index) {
    
    water@csmr <- cl/so4
    
    if (water@alk > 50) {
      warning("Alkalinity is greater than 50 mg/L as CaCO3. CSMR less applicable to this water.")
    }
  }
  
  #CCPP
  # check against Chris's spreadsheet
  # https://legacy.azdeq.gov/environ/water/dw/download/vol_II_app_abc.pdf
  if ("ccpp" %in% index) {
    water@ccpp <- 
  }
  
  
}
