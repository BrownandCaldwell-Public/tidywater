# Acid/Base Equilibrium Functions
# These functions determine pH and alkalinity after blending and chemical addition.

#### Function to calculate the pH from a given water quality vector. Not exported in namespace.

solve_ph <- function(water, so4_dose = 0, na_dose = 0, ca_dose = 0, mg_dose = 0, cl_dose = 0) {

  # Correct eq constants
  ks <- correct_k(water)

  #### SOLVE FOR pH
  solve_h <- function(h, kw, so4_dose, tot_po4, tot_co3, tot_ocl, alk_eq, na_dose, ca_dose, mg_dose, cl_dose) {
    kw / h +
      (2 + h / ks$kso4) * (so4_dose / (h / ks$kso4 + 1)) +
      tot_po4 * (calculate_alpha1_phosphate(h, ks) +
        2 * calculate_alpha2_phosphate(h, ks) +
        3 * calculate_alpha3_phosphate(h, ks)) +
      tot_co3 * (calculate_alpha1_carbonate(h, ks) +
        2 * calculate_alpha2_carbonate(h, ks)) +
      tot_ocl * calculate_alpha1_hypochlorite(h, ks) +
      cl_dose -
      (h + na_dose + 2 * ca_dose + 2 * mg_dose) -
      alk_eq
  }
  root_h <- stats::uniroot(solve_h, interval = c(1e-14, 1),
    kw = water@kw,
    so4_dose = so4_dose,
    tot_po4 = water@tot_po4,
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

#' Add chemicals to water and apply acid/base equilibrium and ion mass balance
#'
#' \code{chemdose_ph} calculates the new pH, alkalinity, and ion balance of a water based on different chemical
#' additions. The function takes an object of class "water" created by \code{\link{define_water}} and user-specified
#' chemical additions and returns a new object of class "water" with updated water quality.
#' Units of all chemical additions are in mg/L as chemical (not as product).
#'
#' \code{chemdose_ph} works by evaluating all the user-specified chemical additions and solving for what the new pH
#' must be using \code{uniroot} to satisfy the principle of electroneutrality in pure water while correcting for the existing alkalinity
#' of the water that the chemical is added to. Multiple chemicals can be added simultaneously or each addition can be
#' modeled independently through sequential doses.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param hcl Amount of hydrochloric acid added in mg/L: HCl -> H + Cl
#' @param h2so4 Amount of sulfuric acid added in mg/L: H2SO4 -> 2H + SO4
#' @param h3po4 Amount of phosphoric acid added in mg/L: H3PO4 -> 3H + PO4
#' @param naoh Amount of caustic added in mg/L: NaOH -> Na + OH
#' @param na2co3 Amount of soda ash added in mg/L: Na2CO3 -> 2Na + CO3
#' @param nahco3 Amount of sodium bicarbonate added in mg/L: NaHCO3 -> Na + H + CO3
#' @param caco3 Amount of calcium carbonate added (or removed) in mg/L: CaCO3 -> Ca + CO3
#' @param caoh2 Amount of lime added in mg/L: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Amount of magneisum hydroxide added in mg/L: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Amount of chlorine gas added in mg/L as Cl2: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Amount of sodium hypochlorite added in mg/L as Cl2: NaOCl -> Na + OCl
#' @param caocl2 Amount of calcium hypochlorite added in mg/L as Cl2: Ca(OCl)2 -> Ca + 2OCl
#' @param co2 Amount of carbon dioxide added in mg/L: CO2 (gas) + H2O -> H2CO3*
#' @param alum Amount of hydrated aluminum sulfate added in mg/L: Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param fecl3 Amount of ferric Chloride added in mg/L: FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param fe2so43 Amount of ferric sulfate added in mg/L: Fe2(SO4)3 + 6HCO3 -> 2Fe(OH)3(am) +3SO4 + 6CO2
#' @param softening_correction Set to TRUE to correct post-softening pH (caco3 must be < 0). Default is FALSE. Based on WTP model equation 5-62
#'
#' @seealso \code{\link{define_water}}, \code{\link{convert_units}}
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 10)
#' # Dose 1 mg/L of hydrochloric acid
#' dosed_water <- chemdose_ph(water, hcl = 1)
#' dosed_water@ph
#'
#' # Dose 1 mg/L of hydrochloric acid and 5 mg/L of alum simultaneously
#' dosed_water <- chemdose_ph(water, hcl = 1, alum = 5)
#' dosed_water@ph
#'
#' # Dose 1 mg/L of hydrochloric acid and 5 mg/L of alum sequentially
#' dosed_water1 <- chemdose_ph(water, hcl = 1)
#' dosed_water1@ph
#' dosed_water2 <- chemdose_ph(dosed_water1, alum = 5)
#' dosed_water2@ph
#'
#' # Softening:
#' water2 <- define_water(ph = 7, temp = 25, alk = 100, tot_hard = 350)
#' dosed_water1 <- chemdose_ph(water2, caco3 = -100)
#' dosed_water1@ph
#' dosed_water2 <- chemdose_ph(water2, caco3 = -100, softening_correction = TRUE)
#' dosed_water2@ph
#'
#' @export
#'
chemdose_ph <- function(water, hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0, na2co3 = 0, nahco3 = 0, caco3 = 0, caoh2 = 0, mgoh2 = 0,
                        cl2 = 0, naocl = 0, caocl2 = 0, co2 = 0,
                        alum = 0, fecl3 = 0, fe2so43 = 0,
                        softening_correction = FALSE) {

  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }
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
  # CaCO3
  caco3 = convert_units(caco3, "caco3")

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
  ca_dose = caoh2 + caocl2 / 2 + caco3
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
  dosed_water@tot_po4 = water@tot_po4 + po4_dose

  # Total hypochlorite
  ocl_dose = cl2 + naocl + caocl2
  dosed_water@tot_ocl = water@tot_ocl + ocl_dose

  # Total carbonate
  co3_dose = na2co3 + nahco3 + co2 + caco3
  dosed_water@tot_co3 = water@tot_co3 + co3_dose

  # Calculate dosed TDS/IS/conductivity
  # Assume that all parameters can be determined by calculating new TDS.
  dosed_water@tds <- water@tds + convert_units(na_dose, "na", "M", "mg/L") +
    convert_units(cl_dose, "cl", "M", "mg/L") + convert_units(k_dose, "k", "M", "mg/L") +
    convert_units(ca_dose, "ca", "M", "mg/L") + convert_units(mg_dose, "mg", "M", "mg/L") +
    convert_units(co3_dose, "co3", "M", "mg/L") + convert_units(po4_dose, "po4", "M", "mg/L") +
    convert_units(so4_dose, "so4", "M", "mg/L") + convert_units(ocl_dose, "ocl", "M", "mg/L")
  dosed_water@is <- correlate_ionicstrength(dosed_water@tds, from = "tds")
  dosed_water@cond <- correlate_ionicstrength(dosed_water@tds, from = "tds", to = "cond")

  # Calculate new pH, H+ and OH- concentrations
  ph = solve_ph(dosed_water, so4_dose = so4_dose, na_dose = na_dose, ca_dose = ca_dose, mg_dose = mg_dose, cl_dose = cl_dose)

  if (softening_correction == TRUE & caco3 < 0) {
    ph_corrected = (ph - 1.86) / 0.71 # WTP Model eq 5-62
    ph = ph_corrected
  }

  h = 10^-ph
  oh = dosed_water@kw / h

  # Correct eq constants
  k <- correct_k(dosed_water)

  # Carbonate and phosphate ions and ocl ions
  alpha1 = calculate_alpha1_carbonate(h, k) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2_carbonate(h, k) # proportion of total carbonate as CO32-
  dosed_water@hco3 = dosed_water@tot_co3 * alpha1
  dosed_water@co3 = dosed_water@tot_co3 * alpha2

  alpha1p = calculate_alpha1_phosphate(h, k)
  alpha2p = calculate_alpha2_phosphate(h, k)
  alpha3p = calculate_alpha3_phosphate(h, k)

  dosed_water@h2po4 = water@tot_po4 * alpha1p
  dosed_water@hpo4 = water@tot_po4 * alpha2p
  dosed_water@po4 = water@tot_po4 * alpha3p

  dosed_water@ocl = water@tot_ocl * calculate_alpha1_hypochlorite(h, k)

  # Calculate new alkalinity
  dosed_water@alk_eq = (dosed_water@hco3 + 2 * dosed_water@co3 + oh - h)
  dosed_water@alk = convert_units(dosed_water@alk_eq, formula = "caco3", startunit = "eq/L", endunit = "mg/L CaCO3")

  # Compile complete dosed water data frame
  dosed_water@ph = ph
  dosed_water@h = h
  dosed_water@oh = oh
  dosed_water@treatment <- paste(dosed_water@treatment, "_chemdosed", sep = "")

  # update total hardness
  dosed_water@tot_hard = convert_units(dosed_water@ca + dosed_water@mg, "caco3", "M", "mg/L CaCO3")

  return(dosed_water)
}



#' Calculate a desired chemical dose for a target pH
#'
#' \code{solvedose_ph} calculates the required amount of a chemical to dose based on a target pH and existing water quality.
#' The function takes an object of class "water" created by \code{\link{define_water}}, and user-specified chemical and target pH
#' and returns a numeric value for the required dose in mg/L.
#'
#' \code{solvedose_ph} uses \code{uniroot} on \code{\link{chemdose_ph}} to match the required dose for the requested pH target.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param target_ph The final pH to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include:
#' acids: "hcl", "h2so4", "h3po4", "co2"; bases: "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
#'
#' @seealso \code{\link{define_water}}, \code{\link{chemdose_ph}}
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 10)
#'
#' # Calculate required dose of lime to reach pH 8
#' solvedose_ph(water, target_ph = 8, chemical = "caoh2")
#'
#' @export
#'
solvedose_ph <- function(water, target_ph, chemical) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }
  if (missing(target_ph)) {
    stop("No target pH defined. Enter a target pH for the chemical dose.")}

  if ((target_ph > 14 | target_ph < 1) & !is.na(target_ph)) {
    stop("Target pH should be between 1-14.")
  }

  if (!(chemical %in% c("hcl", "h2so4", "h3po4", "co2",
    "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"))) {
    stop("Selected chemical addition not supported.")
  }

  # This is the function to minimize
  match_ph <- function(root_dose, chemical, target_ph, water) {
    hcl <- ifelse(chemical == "hcl", root_dose, 0)
    h2so4 <- ifelse(chemical == "h2so4", root_dose, 0)
    h3po4 <- ifelse(chemical == "h3po4", root_dose, 0)

    naoh <- ifelse(chemical == "naoh", root_dose, 0)
    na2co3 <- ifelse(chemical == "na2co3", root_dose, 0)
    nahco3 <- ifelse(chemical == "nahco3", root_dose, 0)
    caoh2 <- ifelse(chemical == "caoh2", root_dose, 0)
    mgoh2 <- ifelse(chemical == "mgoh2", root_dose, 0)
    co2 <- ifelse(chemical == "co2", root_dose, 0)

    waterfin <- chemdose_ph(water, hcl = hcl, h2so4 = h2so4, h3po4 = h3po4,
      naoh = naoh, na2co3 = na2co3, nahco3 = nahco3,
      caoh2 = caoh2, mgoh2 = mgoh2, co2 = co2)

    phfin <- waterfin@ph

    (target_ph - phfin)

  }

  # Target pH can't be met
  if ((chemical %in% c("naoh", "na2co3", "nahco3", "caoh2", "mgoh2") &
    round(target_ph, 1) <= round(water@ph, 1)) |
    (chemical == "co2" & (target_ph < 6.5)) |
    (chemical %in% c("hcl", "h2so4", "h3po4", "co2") &
      round(target_ph, 1) >= round(water@ph, 1)) |
    is.na(target_ph)) {
    warning("Target pH cannot be reached with selected chemical. NA returned.")
    return(NA)
  } else {
    chemdose <- stats::uniroot(match_ph, interval = c(0, 1000), chemical = chemical, target_ph = target_ph, water = water)
    round(chemdose$root, 1)
  }

}


#' Calculate a desired chemical dose for a target alkalinity
#'
#' This function calculates the required amount of a chemical to dose based on a target alkalinity and existing water quality.
#' Returns numeric value for dose in mg/L. Uses uniroot on the chemdose_ph function.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param target_alk The final alkalinity in mg/L as CaCO3 to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include:
#' acids: "hcl", "h2so4", "h3po4", "co2", bases: "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' dose_required <- define_water(ph = 7.9, temp = 22, alk = 100, 80, 50) %>%
#'   solvedose_alk(target_alk = 150, "naoh")
#' @export
#'
solvedose_alk <- function(water, target_alk, chemical) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }
  if (missing(target_alk)) {
    stop("No target alkalinity defined. Enter a target alkalinity (mg/L CaCO3) for the chemical dose.")}

  if ((chemical %in% c("hcl", "h2so4", "h3po4", "co2",
    "naoh", "na2co3", "nahco3", "caoh2", "mgoh2")) == FALSE) {
    stop("Selected chemical addition not supported.")
  }

  # This is the function to minimize
  match_alk <- function(root_dose, chemical, target_alk, water) {
    hcl <- ifelse(chemical == "hcl", root_dose, 0)
    h2so4 <- ifelse(chemical == "h2so4", root_dose, 0)
    h3po4 <- ifelse(chemical == "h3po4", root_dose, 0)

    naoh <- ifelse(chemical == "naoh", root_dose, 0)
    na2co3 <- ifelse(chemical == "na2co3", root_dose, 0)
    nahco3 <- ifelse(chemical == "nahco3", root_dose, 0)
    caoh2 <- ifelse(chemical == "caoh2", root_dose, 0)
    mgoh2 <- ifelse(chemical == "mgoh2", root_dose, 0)
    co2 <- ifelse(chemical == "co2", root_dose, 0)

    waterfin <- chemdose_ph(water, hcl = hcl, h2so4 = h2so4, h3po4 = h3po4,
      naoh = naoh, na2co3 = na2co3, nahco3 = nahco3,
      caoh2 = caoh2, mgoh2 = mgoh2, co2 = co2)
    alkfin <- waterfin@alk

    (target_alk - alkfin)

  }

  # Target alkalinity can't be met
  if ((chemical %in% c("naoh", "na2co3", "nahco3", "caoh2", "mgoh2") &
    target_alk <= water@alk) |
    (chemical %in% c("hcl", "h2so4", "h3po4", "co2") &
      target_alk >= water@alk) |
    is.na(target_alk)) {
    warning("Target alkalinity cannot be reached with selected chemical. NA returned.")
    return(NA)
  } else {
    chemdose <- stats::uniroot(match_alk, interval = c(0, 1000), chemical = chemical, target_alk = target_alk, water = water)
    round(chemdose$root, 1)
  }

}


#' Determine blended water quality from multiple waters based on mass balance and acid/base equilibrium
#'
#' This function takes a vector of waters defined by \code{\link{define_water}} and a vector of ratios and outputs a new water object with updated ion balance and pH.
#'
#' @param waters Vector of source waters created by \code{\link{define_water}}
#' @param ratios Vector of ratios in the same order as waters. (Blend ratios must sum to 1)
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' water1 <- define_water(7, 20, 50)
#' water2 <- define_water(7.5, 20, 100)
#' blend_waters(c(water1, water2), c(.4, .6))
#'
#' @export
#'
blend_waters <- function(waters, ratios) {

  if (length(waters) != length(ratios)) {
    stop("Length of waters vector must equal length of ratios vector.")
  }

  if (!is.list(waters)) {
    stop("Waters must be provided as a vector.")
  }

  if (!is.numeric(ratios)) {
    stop("Ratios must provided as a numeric vector.")
  }

  if (round(sum(ratios), 5) != 1.0) {
    stop("Blend ratios do not sum up to 1")
    # print(sum(ratios)) # this is for checking why the function is breaking
  }

  # Initialize empty blended water
  blended_water <- methods::new("water")
  parameters <- methods::slotNames(blended_water)
  not_averaged <- c("ph", "hco3", "co3", "h", "oh", "kw", "treatment", "estimated")
  parameters <- setdiff(parameters, not_averaged)

  for (param in parameters) {
    for (i in 1:length(waters)) {
      temp_water <- waters[[i]]
      if (!methods::is(temp_water, "water")) {
        stop("All input waters must be of class 'water'. Create a water using define_water.")
      }
      ratio <- ratios[i]

      if (is.na(methods::slot(blended_water, param))) {
        methods::slot(blended_water, param) = methods::slot(temp_water, param) * ratio
      } else {
        methods::slot(blended_water, param) = methods::slot(temp_water, param) * ratio + methods::slot(blended_water, param)
      }
    }
  }

  # Track treatments and estimated params
  treatment <- c()
  estimated <- c()

  for (i in 1:length(waters)) {
    # Create character vectors that just add the values from all the waters together
    temp_water <- waters[[i]]
    new_treat <- unlist(strsplit(temp_water@treatment, "_"))
    treatment <- c(treatment, new_treat)
    new_est <- unlist(strsplit(temp_water@estimated, "_"))
    estimated <- c(estimated, new_est)
  }

  # Keep only one of each treatment and estimated and paste back into string for the water.
  blended_water@treatment <- paste(unique(treatment), collapse = "_")
  blended_water@estimated <- paste(unique(estimated), collapse = "_")

  # Calculate new pH, H+ and OH- concentrations
  # Calculate kw from temp
  tempa = blended_water@temp + 273.15 # absolute temperature (K)
  pkw = round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1) # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  blended_water@kw = 10^-pkw

  # so4_dose, po4_dose, na_dose are all 0
  # ph_inputs = data.frame(tot_cl, tot_so4, 0, tot_po4, 0, tot_na, 0, tot_ocl, tot_co3, cba, kw)
  ph = solve_ph(blended_water)
  h = 10^-ph
  blended_water@oh = blended_water@kw / h
  blended_water@h = h
  blended_water@ph = ph

  # Correct eq constants
  k <- correct_k(blended_water)

  # Carbonate and phosphate ions and ocl ions
  alpha1 = calculate_alpha1_carbonate(h, k) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2_carbonate(h, k) # proportion of total carbonate as CO32-
  blended_water@hco3 = blended_water@tot_co3 * alpha1
  blended_water@co3 = blended_water@tot_co3 * alpha2

  alpha1p = calculate_alpha1_phosphate(h, k)
  alpha2p = calculate_alpha2_phosphate(h, k)
  alpha3p = calculate_alpha3_phosphate(h, k)

  blended_water@h2po4 = blended_water@tot_po4 * alpha1p
  blended_water@hpo4 = blended_water@tot_po4 * alpha2p
  blended_water@po4 = blended_water@tot_po4 * alpha3p

  blended_water@ocl = blended_water@tot_ocl * calculate_alpha1_hypochlorite(h, k)
  blended_water@treatment = paste(blended_water@treatment, "_blended", sep = "")


  return(blended_water)

}
