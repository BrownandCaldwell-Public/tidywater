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
#' Calcium carbonate precipitation potential (CCPP) - a prediction of the mass of calcium carbonate that will precipitate at equilibrium
#' A positive CCPP value indicates the amount of CaCO3 (mg/L as CaCO3) that can be dissolved in the water. A negative CCPP
#' indicates how much CaCO3 will precipitate from solution.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param index The indices to be calculated. 
#'  Default calculates all six indices: "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"
#'  CCPP may not be able to be calculated sometimes, so it may be advantageous to leave this out of the function to avoid errors
#' 
#' @seealso \code{\link{define_water}}
#'
#'  @examples
#' water <- define_water(ph = 8, temp = 25, alk = 200, ca_hard = 200, tds = 450, cl = 150, so4 = 200) %>%
#'  calculate_corrosion()
#'
#' @export
#'
calculate_corrosion <- function(water, index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr")) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}
  if (is.na(water@ca)) {
    warning("Calcium or total hardness not specified. Aggressive, Ryznar, Langelier, and CCPP indices will not be calculated.")}
  if (is.na(water@cl) | is.na(water@so4)) {
    warning("Chloride or sulfate not specified. Larson-Skold index and CSMR will not be calculated.")}
  
  #aggressive
  # AWWA (1977) AWWA Standard for Asbestos-Cement Pressure Pipe, 4 in. through 24 in.,
  # for Water and Other Liquids, AWWA C400-77, Rev of C400-75, American Water
  # Works Association, Denver, CO.
  ca_hard <- convert_units(water@ca, "ca", "M", "mg/L CaCO3")
  
  if ("aggressive" %in% index) {
    water@aggressive <- water@ph + log10(water@alk * ca_hard)
  }
  
  #CSMR
  
  if ("csmr" %in% index) {
    cl <- convert_units(water@cl, "cl", "M", "mg/L")
    so4 <- convert_units(water@so4, "so4", "M", "mg/L")
    
    water@csmr <- cl/so4
    
    #keep this warning?
    # if (water@alk > 50) {
    #   warning("Alkalinity is greater than 50 mg/L as CaCO3. CSMR less applicable to this water.")
    # }
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
  # LANGELIER ------------------------------
  
  ##  USE AWWA TO CALC LSI ----
  
  # AWWA Spreadsheet - matches Chris's calcs pretty well
  # https://www.awwa.org/search/results?s=calculate%20ryznar
  # scroll down and click on the watermath-corrosivity exceldownloads
  # this gives equations for finding A and B
  
  # TDS correction, 2 options to calc (From AWWA spreadsheet)
  # A =  9.7 + ((2.5*(water@is)^0.5) / (1.0 + 5.3*(water@is)^0.5 + 5.5*water@is))
  A = (log10(water@tds) - 1) / 10
  
  # Temp correction
  # B = 2.24961-0.017853*water@temp + 8.238E-5*water@temp^2 - 4.1E-7*water@temp^3 
  tempa = water@temp + 273.15
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
  
  ph_s_mwh = log_k2 - log_kso - log10(water@ca) - log10(alk_mol) # from MWH eq 22-30

  # water@ph - ph_s_awwa  #-1.95
  # water@ph - ph_s_chris #-1.92
  # water@ph - ph_s_mwh   #-1.68
  ph_s = ph_s_mwh
  
  #langelier
  if ("langelier" %in% index) {
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
  
  # using Trussell https://brwncald-my.sharepoint.com/personal/ccorwin_brwncald_com/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fccorwin%5Fbrwncald%5Fcom%2FDocuments%2FMicrosoft%20Teams%20Chat%20Files%2FTrussell%201998%20JAWWA%2Epdf&parent=%2Fpersonal%2Fccorwin%5Fbrwncald%5Fcom%2FDocuments%2FMicrosoft%20Teams%20Chat%20Files&ga=1
  # water <- define_water(ph = 8, temp = 25, alk = 220, ca_hard = 200, tds = 238)
  
  if ("ccpp" %in% index) {
    
    log_kso = 171.9065 + 0.077993*tempa - 2839.319/tempa - 71.595*log10(tempa) #From Plummer and Busenberg (1982), MWH table 22-9
    K_so = 10^-log_kso
    
    solve_x <- function(x, water) {
      water2 <- chemdose_ph(water, caco3 = x)
      K_so/water2@co3 - water2@ca
    }
    
    root_x <- stats::uniroot(solve_x, interval = c(-100, 100),
                             water = water, extendInt = "yes")
    x = root_x$root
    x # not close to CHRIS
    convert_units(x, "caco3", endunit = "mM") #pretty close to Trussel (0.45)
    
    water@ccpp <- x
  }
  
  return(water)
}
