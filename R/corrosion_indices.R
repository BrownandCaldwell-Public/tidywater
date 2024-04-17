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
#' A positive CCPP value indicates the amount of CaCO3 (mg/L as CaCO3) that will precipitate. A negative CCPP
#' indicates how much CaCO3 can be dissolved in the water.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param index The indices to be calculated. 
#'  Default calculates all six indices: "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"
#'  CCPP may not be able to be calculated sometimes, so it may be advantageous to leave this out of the function to avoid errors
#' 
#' @seealso \code{\link{define_water}}
#'
#'  @examples
#' water <- define_water(ph = 8, temp = 25, alk = 200, ca_hard = 200, tds = 576, cl = 150, so4 = 200) %>%
#'  calculate_corrosion()
#' 
#'  water <- define_water(ph = 8, temp = 25, alk = 100, ca_hard = 50, tds = 200) %>%
#' calculate_corrosion(index = c("aggressive", "ccpp"))
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
  
  ###########################################################################################*
  # AGGRESSIVE ------------------------------
  ###########################################################################################*
  # AWWA (1977) AWWA Standard for Asbestos-Cement Pressure Pipe, 4 in. through 24 in.,
  # for Water and Other Liquids, AWWA C400-77, Rev of C400-75, American Water
  # Works Association, Denver, CO.

  if ("aggressive" %in% index) {
    ca_hard <- convert_units(water@ca, "ca", "M", "mg/L CaCO3")
    water@aggressive <- water@ph + log10(water@alk * ca_hard)
    
    if(water@aggressive == -Inf | water@aggressive == Inf){
     water@aggressive <- NA_real_
    }
    
  }
  
  ###########################################################################################*
  # CSMR ------------------------------
  ###########################################################################################*
 
  if ("csmr" %in% index) {
    cl <- convert_units(water@cl, "cl", "M", "mg/L")
    so4 <- convert_units(water@so4, "so4", "M", "mg/L")
    water@csmr <- cl/so4
    
    if(is.nan(water@csmr) | water@csmr == Inf){
      water@csmr <- NA_real_
    }
  }

  ###########################################################################################*
  # LARSONSKOLD ------------------------------
  ###########################################################################################*
  
  if ("larsonskold" %in% index) {
    # epm = equivalents per million
    # (epm Cl + epm SO4)/ (epm HCO3 + epm CO3)
    cl_meq <- convert_units(water@cl, "cl", "M", "meq/L")
    so4_meq <- convert_units(water@so4, "so4", "M", "meq/L")
    alk_meq <- water@alk_eq*1000
    
    water@larsonskold <- (cl_meq + so4_meq) / (alk_meq)
  }
  
  ###########################################################################################*
  # CALCULATE pH OF SATURATION (ph_s) ----
  # see MWH eq 22-30
  # Plummer, L., and Busenberg, E. (1982) "The Solubilities of Calcite Aragonite and
  # Vaterite in CO2-H2O Solutions between 0 and 90 Degrees C, and an Evaluation
  # of the Aqueous Model for the System CaCO3-CO2-H2O," Geochim. Cosmochim.
  # Acta, 46, 1, 1011–1023.

  # Langelier, W. (1936) "The Analytical Control of Anti-Corrosion Water Treatment,"
  # J. AWWA, 28, 11, 1500–1522.

  a_h = water@kw/water@oh #MWH eq 22-25
  k2co3 = a_h*water@co3 /water@hco3   #MWH eq # 22-24
  log_k2 = -log10(k2co3)
  tempa = water@temp + 273.15
  #mixed solubility constant for CaCO3
  log_kso = 171.9065 + 0.077993*tempa - 2839.319/tempa - 71.595*log10(tempa) #From Plummer and Busenberg (1982), MWH table 22-9

  alk_mol = convert_units(water@alk, "caco3", startunit = "mg/L CaCO3")
  ph_s = log_k2 - log_kso - log10(water@ca) - log10(alk_mol) # from MWH eq 22-30

  ###########################################################################################*
  # LANGELIER ------------------------------
  ###########################################################################################*
  
  if ("langelier" %in% index) {
    water@langelier <- water@ph - ph_s
    
    if(water@langelier == -Inf | water@langelier == Inf){
      water@langelier <- NA_real_
    }
  }
  
  ###########################################################################################*
  # RYZNAR ------------------------------
  ###########################################################################################*
  # Ryznar,T.(1944) "A New Index for Determining the Amount of Calcium Carbonate Scale Formed by a Water," J. AWWA, 36, 4, 472–486.
 
   if ("ryznar" %in% index) {
     water@ryznar <- 2*ph_s - water@ph
     
     if(water@ryznar == -Inf | water@ryznar == Inf){
       water@ryznar <- NA_real_
     }
  }
  
  ###########################################################################################*
  # CCPP ------------------------------
  ###########################################################################################*
  # Merrill, D., and Sanks, R. (1977a) "Corrosion Control by Deposition of CaCO3
  # Films, Part I," J. AWWA, 69, 11, 592–599.
  # Merrill, D., and Sanks, R. (1977b) "Corrosion Control by Deposition of CaCO3
  # Films, Part 2," J. AWWA, 69, 12, 634–640.
  # Merrill, D., and Sanks, R. (1978) "Corrosion Control by Deposition of CaCO3 Films,
  # Part 3," J. AWWA, 70, 1, 12–18.
  
  # using Trussell https://brwncald-my.sharepoint.com/personal/ccorwin_brwncald_com/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fccorwin%5Fbrwncald%5Fcom%2FDocuments%2FMicrosoft%20Teams%20Chat%20Files%2FTrussell%201998%20JAWWA%2Epdf&parent=%2Fpersonal%2Fccorwin%5Fbrwncald%5Fcom%2FDocuments%2FMicrosoft%20Teams%20Chat%20Files&ga=1
  # water <- define_water(ph = 8, temp = 25, alk = 220, ca_hard = 200, tds = 238)
  
  if ("ccpp" %in% index) {
    K_so = 10^-log_kso
    
    solve_x <- function(x, water) {
      water2 <- chemdose_ph(water, caco3 = x)
     K_so/(water2@co3*active_2) - water2@ca*active_2
    }
    
    root_x <- stats::uniroot(solve_x, 
                             water = water, 
                             interval = c(-1000, 1000),
                             lower = -1,
                             upper = 1,
                             extendInt = "yes")
    
    water@ccpp <- -root_x$root
  }
  
  return(water)
}
