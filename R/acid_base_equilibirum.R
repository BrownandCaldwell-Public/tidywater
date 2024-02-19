# Acid/Base Equilibrium Functions
# These functions determine pH and alkalinity after blending and chemical addition

# Author: R. Mulhern, based on spreadsheet calculations by C. Corwin
# Reviewers: Sierra Johnson 1/19/24


#### Function to calculate the pH from a given water quality vector. Not exported in namespace.

solve_ph <- function(water, so4_dose = 0, po4_dose = 0, na_dose = 0, ca_dose = 0, mg_dose = 0, cl_dose = 0) {

  #### SOLVE FOR pH
  solve_h <- function(h, kw, so4_dose, po4_dose, tot_co3, tot_ocl, alk_eq, na_dose, ca_dose, mg_dose, cl_dose) {
    kw / h +
      (2 + h / discons$kso4) * (so4_dose / (h / discons$kso4 + 1)) +
      (h^2 / discons$k2po4 / discons$k3po4 + 2 * h / discons$k3po4 + 3) * (po4_dose / (h^3 / discons$k1po4 / discons$k2po4 / discons$k3po4 + h^2 / discons$k2po4 / discons$k3po4 + h / discons$k3po4 + 1)) +
      (h / discons$k2co3 + 2) * (tot_co3 / (h^2 / discons$k1co3 / discons$k2co3 + h / discons$k2co3 + 1)) +
      tot_ocl / (h / discons$kocl + 1) -
      (h + alk_eq + na_dose + 2*ca_dose + 2*mg_dose - cl_dose)
  }
  root_h <- uniroot(solve_h, interval = c(1e-14, 1e-1),
    kw = water@kw,
    so4_dose = so4_dose,
    po4_dose = po4_dose,
    tot_co3 = water@tot_co3,
    tot_ocl = water@tot_ocl,
    alk_eq = water@alk_eq,
    na_dose = na_dose,
    ca_dose = ca_dose,
    mg_dose = mg_dose,
    cl_dose = cl_dose,
    tol = 1e-14)
  phfinal = -log10(root_h$root)
  return(round(phfinal, 2))
}

#' Chemical Dose Function
#'
#' This function takes chemical doses and a water data frame defined by \code{\link{define_water}} and outputs a new water data frame with updated ion balance and pH.
#' Units of all chemical additions in mg/L as chemical (not as product).
#' Returns "water" class object of dosed water.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param hcl Hydrochloric acid: HCl -> H + Cl
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3po4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param caocl2 Calcium hypochlorite: Ca(OCl)2 -> Ca + 2OCl
#' @param co2 Carbon Dioxide CO2 (gas) + H2O -> H2CO3*
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param fecl3 Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param fe2so43 Ferric sulfate Fe2(SO4)3 + 6HCO3 -> 2Fe(OH)3(am) +3SO4 + 6CO2
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' # Put example code here
#'
#' @export
#'
dose_chemical <- function(water, hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0, na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                          cl2 = 0, naocl = 0, caocl2 = 0, co2 = 0,
                          alum = 0, fecl3 = 0, fe2so43 = 0) {

  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}

  #### CONVERT INDIVIDUAL CHEMICAL ADDITIONS TO MOLAR ####

  # Hydrochloric acid (HCl) dose
  hcl = convert_units(hcl, "hcl")
  # Sulfuric acid (H2SO4) dose
  h2so4 = convert_units(h2so4, "h2so4")

  # Phosphoric acid (H3PO4) dose
  h3po4 = convert_units(h3po4, "h3po4")

  # Caustic soda (NaOH) dose
  naoh = convert_units(naoh, "naoh")

  # Soda ash (Na2CO3) dose
  na2co3 = convert_units(na2co3, "na2co3")

  # Sodium bicarbonate (NaHCO3) dose
  nahco3 = convert_units(nahco3, "nahco3")

  # Lime (Ca(OH)2) dose
  caoh2 = convert_units(caoh2, "caoh2")

  # Magnesium hydroxide (Mg(OH)2) dose
  mgoh2 = convert_units(mgoh2, "mgoh2")

  # Chlorine gas (Cl2)
  cl2 = convert_units(cl2, "cl2")

  # Sodium hypochlorite (NaOCl) as Cl2
  naocl = convert_units(naocl, "cl2")

  # Calcium hypochlorite (Ca(OCl)2) as Cl2
  caocl2 = convert_units(caocl2, "cl2")

  # Carbon dioxide
  co2 = convert_units(co2, "co2")

  # Alum - hydration included
  alum = convert_units(alum, "alum")

  # Ferric chloride
  fecl3 = convert_units(fecl3, "fecl3")

  # Ferric sulfate
  fe2so43 = convert_units(fe2so43, "fe2so43")

  #### CALCULATE NEW ION BALANCE FROM ALL CHEMICAL ADDITIONS ####
dosed_water <- water

  # Total sodium
  na_dose = naoh + 2 * na2co3 + nahco3 + naocl
  dosed_water@na = water@na + na_dose

  # Total calcium
  ca_dose = caoh2 + caocl2 / 2
  dosed_water@ca = water@ca + ca_dose

  # Total magnesium
  mg_dose = mgoh2
  dosed_water@mg = water@mg + mg_dose

  # Total potassium
  k_dose = 0
  dosed_water@k = water@k + k_dose

  # Total chloride
  cl_dose = hcl + cl2 + 3 * fecl3
  dosed_water@cl = water@cl + cl_dose

  # Total sulfate
  so4_dose = h2so4 + 3 * alum + 3 * fe2so43
  dosed_water@so4 = water@so4 + so4_dose

  # Total phosphate
  po4_dose = h3po4
  dosed_water@po4 = water@po4 + po4_dose

  # Total hypochlorite
  ocl_dose = cl2 + naocl + caocl2
  dosed_water@tot_ocl = water@tot_ocl + ocl_dose

  # Total carbonate
  co3_dose = na2co3 + nahco3 + co2
  dosed_water@tot_co3 = water@tot_co3 + co3_dose

  # Calculate new pH, H+ and OH- concentrations
  ph = solve_ph(dosed_water, so4_dose = so4_dose, po4_dose = po4_dose, na_dose = na_dose, ca_dose = ca_dose, mg_dose = mg_dose, cl_dose = cl_dose)
  h = 10^-ph
  oh = dosed_water@kw / h

  # Calculate new carbonate system balance
  alpha1 = calculate_alpha1(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as CO32-
  dosed_water@hco3 = dosed_water@tot_co3 * alpha1
  dosed_water@co3 = dosed_water@tot_co3 * alpha2

  # Calculate new alkalinity
  dosed_water@alk_eq = (dosed_water@hco3 + 2 * dosed_water@co3 + oh - h)
  dosed_water@alk = convert_units(dosed_water@alk_eq, formula = "caco3", startunit = "eq/L", endunit = "mg/L CaCO3")

  # Compile complete dosed water data frame
  dosed_water@ph = ph
  dosed_water@h = h
  dosed_water@oh = oh

  return(dosed_water)
}



#' Target Chemical Dose Function
#'
#' This function calculates the required amount of a chemical to dose based on a target pH and existing water quality.
#' Returns numeric value for dose in mg/L. Uses optimize on the dose_chemical function.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param target_ph The final pH to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include: caustic soda (NaOH), lime (Ca(OH2)), magnesium hydroxide (Mg(OH)2), and CO2.
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' # Put example code here
#'
#' @export
#'
dose_target <- function(water, target_ph, chemical) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}

  if (missing(target_ph)) {
    stop("No target pH defined. Enter a target pH for the chemical dose.")}

  if ((chemical %in% c("naoh", "caoh2", "mgoh2", "co2")) == FALSE) {
    stop("Selected chemical addition not supported.")
  }

  if ((chemical %in% c("naoh", "caoh2", "mgoh2") & target_ph <= water@ph) |
      (chemical == "co2" & (target_ph < 6.5 | target_ph >= water@ph))) {
    stop("Target pH cannot be reached with selected chemical")
  }

  # This is the function to minimize
  match_ph <- function(root_dose, chemical, target_ph, water) {
    naoh <- ifelse(chemical == "naoh", root_dose, 0)
    caoh2 <- ifelse(chemical == "caoh2", root_dose, 0)
    mgoh2 <- ifelse(chemical == "mgoh2", root_dose, 0)
    co2 <- ifelse(chemical == "co2", root_dose, 0)

    waterfin <- dose_chemical(water, naoh = naoh, caoh2 = caoh2, mgoh2 = mgoh2, co2 = co2)
    phfin <- waterfin$ph

    abs(target_ph - phfin)

  }

  chemdose <- optimize(match_ph, interval = c(0, 1000), chemical = chemical, target_ph = target_ph, water = water)
  round(chemdose$minimum, 1)
}


#' Blend water function
#'
#' This function takes up to 4 water data frames defined by \code{\link{define_water}} and outputs a new water data frame with updated ion balance and pH.
#'
#' @param water1 Source water 1 data frame created by \code{\link{define_water}}
#' @param ratio1 Blend ratio of water 1. (Blend ratios must sum to 1)
#' @param water2 Source water 2 data frame created by \code{\link{define_water}}
#' @param ratio2 Blend ratio of water 2. (Blend ratios must sum to 1)
#' @param water3 Source water 3 data frame created by \code{\link{define_water}}
#' @param ratio3 Blend ratio of water 3. (Blend ratios must sum to 1)
#' @param water4 Source water 4 data frame created by \code{\link{define_water}}
#' @param ratio4 Blend ratio of water 4. (Blend ratios must sum to 1)
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' blend_waters(water1, 0.5, water2, .05)
#'
#' @export
#'
blend_waters <- function(waters, ratios) {

  if(length(waters) != length(ratios)) {
    stop("Length of waters vector must equal length of ratios vector.")
  }

  if(sum(ratios) != 1) {
    stop("Blend ratios do not sum up to 1")
  }

  # Initialize empty blended water
  blended_water <- new("water")
  parameters <- slotNames(blended_water)
  not_averaged <- c("ph", "hco3", "co3", "h", "oh", "kw")
  parameters <- setdiff(parameters, not_averaged)

  for(param in parameters) {
    for(i in 1:length(waters)) {
      temp_water <- waters[[i]]
      ratio <- ratios[i]

      if(is.na(slot(blended_water, param))) {
        slot(blended_water, param) = slot(temp_water, param) * ratio
      } else {
        slot(blended_water, param) = slot(temp_water, param) * ratio + slot(blended_water, param)
      }
    }
  }

  # Calculate new pH, H+ and OH- concentrations
  # Calculate kw from temp
  tempa = blended_water@temp + 273.15 # absolute temperature (K)
  pkw = round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1) # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  blended_water@kw = 10^-pkw

  # so4_dose, po4_dose, na_dose are all 0
  #ph_inputs = data.frame(tot_cl, tot_so4, 0, tot_po4, 0, tot_na, 0, tot_ocl, tot_co3, cba, kw)
  ph = solve_ph(blended_water)
  h = 10^-ph
  blended_water@oh = blended_water@kw / h
  blended_water@h = h
  blended_water@ph = ph

  # Calculate new carbonate system balance
  alpha1 = calculate_alpha1(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as CO32-
  blended_water@hco3 = blended_water@tot_co3 * alpha1
  blended_water@co3 = blended_water@tot_co3 * alpha2

  return(blended_water)

}
