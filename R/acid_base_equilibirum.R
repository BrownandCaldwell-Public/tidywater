# Acid/Base Equilibrium Functions
# These functions determine pH and alkalinity after blending and chemical addition

# Author: R. Mulhern, based on spreadsheet calculations by C. Corwin
# Reviewers:

#' Define water vector
#'
#' This function takes water quality parameters and creates a standard data frame that forms the input and output of all pH functions.
#' Carbonate balance is calculated and units are converted to mol/L
#'
#' @param type Water type = treated drinking water (dw), raw groundwater (gw), raw surface water (sw), or treated wastewater (ww)
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
define_water <- function(ph, temp, alk, tot_hard, c_hard, na, k, cl, so4, tot_ocl = 0, type) {
  # Handle missing arguments with no default water type defined
  if (missing(ph) & missing(type)) {
    stop("Missing value for pH. If not known, specify water type to use default estimated value.")
  }

  if (missing(temp) & missing(type)) {
    stop("Missing value for temperature. If not known, specify water type to use default estimated value.")
  }

  if (missing(alk) & missing(type)) {
    stop("Missing value for alkalinity. If not known, specify water type to use default estimated value.")
  }

  if (missing(tot_hard) & missing(type)) {
    stop("Missing value for total hardness. If not known, specify water type to use default estimated value.")
  }

  if (missing(c_hard) & missing(type)) {
    stop("Missing value for calcium hardness. If not known, specify water type to use default estimated value.")
  }

  if (missing(na) & missing(type)) {
    stop("Missing value for sodium (Na+). If not known, specify water type to use default estimated value.")
  }

  if (missing(k) & missing(type)) {
    stop("Missing value for potassium (K+). If not known, specify water type to use default estimated value.")
  }

  if (missing(cl) & missing(type)) {
    stop("Missing value for chloride (Cl-). If not known, specify water type to use default estimated value.")
  }

  if (missing(so4) & missing(type)) {
    stop("Missing value for sulfate (SO4_2-). If not known, specify water type to use default estimated value.")
  }

  # Handle missing water type when all other parameters are specified or unknown water type
  if (missing(ph) == FALSE & missing(temp) == FALSE & missing(alk) == FALSE & missing(tot_hard) == FALSE & missing(c_hard) == FALSE
  & missing(na) == FALSE & missing(k) == FALSE & missing(cl) == FALSE & missing(so4) == FALSE & missing(type)) {
    type = NA
  } else if ((type %in% wq$water_type) == FALSE) {
    stop("Unknown water type. Options include drinking water (dw), groundwater (gw), surface water (sw), or wastewater (ww).")
  }

  # Restrict data frame of default water quality values to only those for specified water type
  wq = wq %>%
    filter(water_type == type)

  # Handle missing arguments with a valid water type defined (warning only)
  if (missing(ph) & missing(type) == FALSE) {
    ph = wq$ph
    warning("Missing value for pH. Default value will be used based on entered water type.")
  }

  if (missing(temp) & missing(type) == FALSE) {
    temp = wq$temp
    warning("Missing value for temperature. Default value will be used based on entered water type.")
  }

  if (missing(alk) & missing(type) == FALSE) {
    alk = wq$alk
    warning("Missing value for alkalinity. Default value will be used based on entered water type.")
  }

  if (missing(tot_hard) & missing(type) == FALSE) {
    tot_hard = wq$tot_hard
    warning("Missing value for total hardness. Default value will be used based on entered water type.")
  }

  if (missing(c_hard) & missing(type) == FALSE) {
    c_hard = wq$c_hard
    warning("Missing value for calcium hardness. Default value will be used based on entered water type.")
  }

  if (missing(na) & missing(type) == FALSE) {
    na = wq$na
    warning("Missing value for sodium (Na+). Default value will be used based on entered water type.")
  }

  if (missing(k) & missing(type) == FALSE) {
    k = wq$k
    warning("Missing value for potassium (K+). Default value will be used based on entered water type.")
  }

  if (missing(cl) & missing(type) == FALSE) {
    cl = wq$cl
    warning("Missing value for chloride (Cl-). Default value will be used based on entered water type.")
  }

  if (missing(so4) & missing(type) == FALSE) {
    so4 = wq$so4
    warning("Missing value for sulfate (SO4_2-). Default value will be used based on entered water type.")
  }

  # Calculate kw from temp
  tempa = temp + 273.15 # absolute temperature (K)
  pkw = round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1) # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  kw = 10^-pkw

  # convert major ion concentration inputs to mol/L
  na = na / mweights$na / 1000
  ca = c_hard / mweights$caco3 / 1000
  mg = (tot_hard - c_hard) / mweights$caco3 / 1000
  k = k / mweights$k / 1000
  cl = cl / mweights$cl / 1000
  so4 = so4 / mweights$so4 / 1000
  h = 10^-ph
  oh = kw / h

  # calculate carbonate system balance
  alpha1 = (discons$k1co3 * h) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as HCO3-
  alpha2 = (discons$k1co3 * discons$k2co3) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as CO32-
  alk_eq = (alk * 2) / (mweights$caco3 * 1000) # convert alkalinity input to equivalents/L
  tot_co3 = (alk_eq + h - oh) / (alpha1 + 2 * alpha2) # calculate total carbonate concentration
  hco3 = tot_co3 * alpha1
  co3 = tot_co3 * alpha2

  cba = hco3 + 2 * co3 + oh - h # calculate total acid base balance, equivalent to alkalinity in most natural waters

  # Compile complete source water data frame to save to environment
  water_df = data.frame(ph, temp, alk, tot_hard, na, ca, mg, k, cl, so4, hco3, co3, h, oh, tot_ocl, tot_co3, cba, kw, alk_eq)
  return(water_df)
}

#### Function to calculate the pH from a given water quality vector. Not exported in namespace.

solve_ph <- function(water) {
  cba <- water$cba
  kw <- water$kw

  #### COMPILE ACID DISSOCIATION CONSTANTS

  # Carbonate
  # k1co3=10^-6.35 #H2CO3<-->HCO3- + H+
  # k2co3=10^-10.33 #HCO3-<-->CO32- + H+
  tot_co3 = water$tot_co3

  # Sulfate
  # kso4=10^-1.99 #H2SO4<-->2H+ + SO42-
  if (is.null(water$so4_dose)) {
    so4_dose = 0
  } else {so4_dose = water$so4_dose}

  # Phosphate
  # k1po4=10^-2.16 #H3PO4<-->H+ + H2PO4-
  # k2po4=10^-7.20 #H2PO4-<-->H+ + HPO42-
  # k3po4=10^-12.35 #HPO42--<-->H+ + PO43-
  if (is.null(water$po4_dose)) {
    po4_dose = 0
  } else {po4_dose = water$po4_dose}

  # Hypochlorite
  # kocl=10^-7.6 #HOCl<-->H+ + OCl-
  if (is.null(water$tot_ocl)) {
    tot_ocl = 0
  } else {tot_ocl = water$tot_ocl}

  #### SOLVE FOR pH
  solve_h <- function(h, kw, so4_dose, po4_dose, tot_co3, tot_ocl, cba) {
    kw / h +
      (2 + h / discons$kso4) * (so4_dose / (h / discons$kso4 + 1)) +
      (h^2 / discons$k2po4 / discons$k3po4 + 2 * h / discons$k3po4 + 3) * (po4_dose / (h^3 / discons$k1po4 / discons$k2po4 / discons$k3po4 + h^2 / discons$k2po4 / discons$k3po4 + h / discons$k3po4 + 1)) +
      (h / discons$k2co3 + 2) * (tot_co3 / (h^2 / discons$k1co3 / discons$k2co3 + h / discons$k2co3 + 1)) +
      tot_ocl / (h / discons$kocl + 1) -
      h - cba
  }
  root_h <- uniroot(solve_h, interval = c(0, 1),
    kw = kw,
    so4_dose = so4_dose,
    po4_dose = po4_dose,
    tot_co3 = tot_co3,
    tot_ocl = tot_ocl,
    cba = cba,
    tol = 1e-12)
  phfinal = -log10(root_h$root)
  return(round(phfinal, 2))
}

#' Chemical Dose Function
#'
#' This function takes chemical doses and a water data frame defined by \code{\link{define_water}} and outputs a new water data frame with updated ion balance and pH.
#' Units of all chemical additions in mg/L.
#' Returns data frame of dosed water quality.
#'
#' @param water Source water data frame created by \code{\link{define_water}}
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
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' # Put example code here
#'
#' @export
#'
dose_chemical <- function(water, hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0, na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0, cl2 = 0, naocl = 0, caocl2 = 0) {

  if (missing(water)) {
    stop("No source water defined. Create a water quality data frame using the 'define_water' function.")}

  #### CONVERT INDIVIDUAL CHEMICAL ADDITIONS TO MOLAR ####

  # Hydrochloric acid (HCl) dose
  hcl = hcl / mweights$hcl * 10^-3

  # Sulfuric acid (H2SO4) dose
  h2so4 = h2so4 / mweights$h2so4 * 10^-3

  # Phosphoric acid (H3PO4) dose
  h3po4 = h3po4 / mweights$h3po4 * 10^-3

  # Caustic soda (NaOH) dose
  naoh = naoh / mweights$naoh * 10^-3

  # Soda ash (Na2CO3) dose
  na2co3 = na2co3 / mweights$na2co3 * 10^-3

  # Sodium bicarbonate (NaHCO3) dose
  nahco3 = nahco3 / mweights$nahco3 * 10^-3

  # Lime (Ca(OH)2) dose
  caoh2 = caoh2 / mweights$caoh2 * 10^-3

  # Magnesium hydroxide (Mg(OH)2) dose
  mgoh2 = mgoh2 / mweights$mgoh2 * 10^-3

  # Chlorine gas (Cl2)
  cl2 = cl2 / mweights$cl2 * 10^-3

  # Sodium hypochlorite (NaOCl) as Cl2
  naocl = naocl / mweights$cl2 * 10^-3

  # Calcium hypochlorite (Ca(OCl)2) as Cl2
  caocl2 = caocl2 / mweights$cl2 * 10^-3

  #### CALCULATE NEW ION BALANCE FROM ALL CHEMICAL ADDITIONS ####

  # Total sodium
  na_dose = naoh + 2 * na2co3 + nahco3 + naocl
  tot_na = water$na + na_dose

  # Total calcium
  ca_dose = caoh2 + caocl2 / 2
  tot_ca = water$ca + ca_dose

  # Total magnesium
  mg_dose = mgoh2
  tot_mg = water$mg + mg_dose

  # Total potassium
  k_dose = 0
  tot_k = water$k + k_dose

  # Total chloride
  cl_dose = hcl + cl2
  tot_cl = water$cl + cl_dose

  # Total sulfate
  so4_dose = h2so4
  tot_so4 = water$so4 + so4_dose

  # Total phosphate
  po4_dose = h3po4
  tot_po4 = po4_dose

  # Total hypochlorite
  ocl_dose = cl2 + naocl + caocl2
  tot_ocl = water$tot_ocl + ocl_dose

  # Total carbonate
  co3_dose = na2co3 + nahco3
  tot_co3 = water$tot_co3 + co3_dose

  # Update acid/base balance equation with each chemical addition
  cba = water$cba + na_dose + 2 * ca_dose + 2 * mg_dose - cl_dose

  # Calculate new pH, H+ and OH- concentrations
  kw = water$kw
  ph_inputs = data.frame(tot_cl, tot_so4, so4_dose, tot_po4, po4_dose, tot_na, na_dose, tot_ocl, tot_co3, cba, kw)
  ph = solve_ph(ph_inputs)
  h = 10^-ph
  oh = kw / h

  # Calculate new carbonate system balance
  alpha1 = (discons$k1co3 * h) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as HCO3-
  alpha2 = (discons$k1co3 * discons$k2co3) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as CO32-
  hco3 = tot_co3 * alpha1
  co3 = tot_co3 * alpha2

  # Calculate new alkalinity (mg/L as CacO3)
  alk_eq = (hco3 + 2 * co3 + oh - h)
  alk = (alk_eq / 2) * mweights$caco3 * 1000

  # Calculate new hardness (mg/L as CaCO3)
  tot_hard = (tot_ca * mweights$caco3 * 1000) + (tot_mg * mweights$caco3 * 1000)

  # Compile complete dosed water data frame
  dosed_water_df = data.frame(ph,
    temp = water$temp,
    alk,
    tot_hard,
    na = tot_na,
    ca = tot_ca,
    mg = tot_mg,
    k = tot_k,
    cl = tot_cl,
    so4 = tot_so4,
    hco3, co3, h, oh,
    tot_co3,
    tot_ocl,
    cba,
    kw,
    alk_eq)
  return(dosed_water_df)
}

#' Water Summary Table
#'
#' This function takes a water data frame defined by \code{\link{define_water}} and outputs a formatted summary table.
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
summarize_wq <- function(water) {
  # Compile main WQ parameters to print
  params = data.frame(pH = water$ph,
    Temp = water$temp,
    Alkalinity = water$alk,
    Total_Hardness = water$tot_hard)

  params = params %>%
    pivot_longer(c(pH:Total_Hardness), names_to = "param", values_to = "result") %>%
    mutate(units = c("-", "deg C", "mg/L as CaCO3", "mg/L as CaCO3"))

  tab1 = knitr::kable(params,
    format = "simple",
    col.names = c("Key water quality parameters", "Result", "Units"))

  # Compile major ions to print
  ions = data.frame(Na = water$na,
    Ca = water$ca,
    Mg = water$mg,
    K = water$k,
    Cl = water$cl,
    SO4 = water$so4,
    HCO3 = water$hco3,
    CO3 = water$co3,
    H = water$h,
    OH = water$oh)

  ions = ions %>%
    pivot_longer(c(Na:OH), names_to = "ion", values_to = "c_mol")

  tab2 = knitr::kable(ions,
    format = "simple",
    col.names = c("Major ions in source water", "Concentration (mol/L)"),
    format.args = list(scientific = TRUE),
    digits = 10)

  # print(kables(list(tab1,tab2)))
  return(knitr::kables(list(tab1, tab2)))
}

#' Ion Summary Plot
#'
#' This function takes a water data frame defined by \code{\link{define_water}} and outputs an ion balance plot.
#'
#' @param water Source water vector created by link function here
#' @param title Optional plot title
#' @import ggplot2
#'
#'
#' @examples
#' # Put example code here
#'
#' @export
#'
plot_ions <- function(water, title = "") {

  # Compile major ions to plot
  ions = data.frame(Na = water$na,
    Ca = water$ca,
    Mg = water$mg,
    K = water$k,
    Cl = water$cl,
    SO4 = water$so4,
    HCO3 = water$hco3,
    CO3 = water$co3,
    H = water$h,
    OH = water$oh)

  ions %>%
    pivot_longer(c(Na:OH), names_to = "ion", values_to = "concentration") %>%
    mutate(type = case_when(ion %in% c("Na", "Ca", "Mg", "K", "H") == TRUE ~ "Cations",
      TRUE ~ "Anions")) %>%
    ggplot(aes(x = concentration, y = type, fill = ion)) +
    geom_bar(stat = "identity",
      width = 0.5,
      # aes(fill=ion),
      alpha = 0.5,
      color = "black") +
    geom_text(aes(label = ifelse(concentration > 10e-5, ion, ""), fontface = "bold", angle = 90),
      size = 3.5,
      position = position_stack(vjust = 0.5)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    labs(x = "Concentration (mol/L)",
      y = "Major cations and anions",
      title = title,
      subtitle = paste0("pH=", water$ph)) +
    guides(fill = "none")
}

#' Target Chemical Dose Function
#'
#' This function calculates the required amount of a chemical to dose based on a target pH and existing water quality.
#' Returns numeric value for dose in mg/L.
#'
#' @param water Source water data frame created by \code{\link{define_water}}
#' @param target_ph The final pH to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include: caustic soda (NaOH), lime (Ca(OH2)), and magnesium hydroxide (Mg(OH)2).
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
    stop("No source water defined. Create a water quality data frame using the 'define_water' function.")}

  if (missing(target_ph)) {
    stop("No target pH defined. Enter a target pH for the chemical dose.")}

  if ((chemical %in% c("naoh", "caoh2", "mgoh2")) == FALSE) {
    stop("Selected chemical addition not supported.")
  }

  # Calculate H+ and OH- concentrations from target pH
  kw = water$kw
  h = 10^-target_ph
  oh = kw / h

  # Calculate new carbonate system balance from target pH and chemical addition (if relevant)
  if (chemical %in% c("naoh", "caoh2", "mgoh2")) {
    alpha1 = (discons$k1co3 * h) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as HCO3-
    alpha2 = (discons$k1co3 * discons$k2co3) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as CO32-
    hco3 = water$tot_co3 * alpha1
    co3 = water$tot_co3 * alpha2
  }

  if (chemical %in% c("naoh", "caoh2", "mgoh2")) {
    # Solve for new CBA based on target pH
    solve_cba <- function(cba, h, kw, tot_co3) {
      kw / h +
        # (2 + h / discons$kso4) * (so4_dose / (h / discons$kso4 + 1)) +
        # (h ^ 2 / discons$k2po4 / discons$k3po4 + 2 * h / discons$k3po4 + 3) * (po4_dose / (h ^ 3 / discons$k1po4 / discons$k2po4 / discons$k3po4 + h ^ 2 / discons$k2po4 / discons$k3po4 + h / discons$k3po4 + 1)) +
        (h / discons$k2co3 + 2) * (tot_co3 / (h^2 / discons$k1co3 / discons$k2co3 + h / discons$k2co3 + 1)) +
        # tot_ocl / (h / discons$kocl + 1) -
        h - cba
    }
    root_cba <- uniroot(solve_cba, interval = c(-1, 1),
      h = h,
      kw = kw,
      tot_co3 = water$tot_co3,
      # so4_dose=so4_dose,
      # po4_dose=po4_dose,
      # tot_co3=tot_co3,
      # tot_ocl=tot_ocl,
      # cba=cba,
      tol = 1e-5,
      trace = 1)
    cba <- root_cba$root
  }

  # Solve for required chemical dose
  if (chemical == "naoh") {
    solve_dose <- function(na_dose, cba1, cba2) {
      cba1 + na_dose - cba2
    }
    root_dose <- uniroot(solve_dose, interval = c(0, 100),
      cba1 = water$cba,
      cba2 = cba,
      extendInt = "yes",
      tol = 1e-5)
    na_dose <- root_dose$root
    naoh_dose = na_dose * mweights$naoh * 1000
    return(naoh_dose)
  }

  if (chemical == "caoh2") {
    solve_dose <- function(ca_dose, cba1, cba2) {
      cba1 + 2 * ca_dose - cba2
    }
    root_dose <- uniroot(solve_dose, interval = c(0, 100),
      cba1 = water$cba,
      cba2 = cba,
      extendInt = "yes",
      tol = 1e-5)
    ca_dose <- root_dose$root
    caoh2_dose = ca_dose * mweights$caoh2 * 1000
    return(caoh2_dose)
  }

  if (chemical == "mgoh2") {
    solve_dose <- function(mg_dose, cba1, cba2) {
      cba1 + 2 * mg_dose - cba2
    }
    root_dose <- uniroot(solve_dose, interval = c(0, 100),
      cba1 = water$cba,
      cba2 = cba,
      extendInt = "yes",
      tol = 1e-5)
    mg_dose <- root_dose$root
    mgoh2_dose = mg_dose * mweights$mgoh2 * 1000
    return(mgoh2_dose)
  }
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
blend_waters <- function(water1, ratio1, water2, ratio2, water3=data.frame(ph=NA), ratio3=0, water4=data.frame(ph=NA), ratio4=0, ...) {

  if(ratio1 + ratio2 + ratio3 + ratio4 != 1) {
    stop("Blend ratios do not sum up to 1")
  }

  # Mass balance - ions
  water1$ratio = ratio1
  water2$ratio = ratio2
  water3$ratio = ratio3
  water4$ratio = ratio4

  blend_df <- bind_rows(water1, water2, water3, water4) %>%
    mutate(across(!ratio, ~ .x * ratio)) %>%
    summarise(across(!ph, ~ sum(.x, na.rm = TRUE)))

  tot_na = blend_df$na
  tot_ca = blend_df$ca
  tot_mg = blend_df$mg
  tot_k = blend_df$k
  tot_cl = blend_df$cl
  tot_so4 = blend_df$so4
  tot_po4 = 0
  tot_ocl = blend_df$tot_ocl
  tot_co3 = blend_df$tot_co3
  cba = blend_df$cba

  # Calculate new pH, H+ and OH- concentrations
  # Calculate kw from temp
  temp = blend_df$temp
  tempa = temp + 273.15 # absolute temperature (K)
  pkw = round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1) # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  kw = 10^-pkw

  # so4_dose, po4_dose, na_dose are all 0
  ph_inputs = data.frame(tot_cl, tot_so4, 0, tot_po4, 0, tot_na, 0, tot_ocl, tot_co3, cba, kw)
  ph = solve_ph(ph_inputs)
  h = 10^-ph
  oh = kw / h

  # Calculate new carbonate system balance
  alpha1 = (discons$k1co3 * h) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as HCO3-
  alpha2 = (discons$k1co3 * discons$k2co3) / (h^2 + discons$k1co3 * h + discons$k1co3 * discons$k2co3) # proportion of total carbonate as CO32-
  hco3 = tot_co3 * alpha1
  co3 = tot_co3 * alpha2

  # Calculate new alkalinity (mg/L as CacO3)
  alk_eq = (hco3 + 2 * co3 + oh - h)
  alk = (alk_eq / 2) * mweights$caco3 * 1000

  # Calculate new hardness (mg/L as CaCO3)
  tot_hard = (tot_ca * mweights$caco3 * 1000) + (tot_mg * mweights$caco3 * 1000)

  # Compile complete dosed water data frame
  data.frame(ph,
             temp = temp,
             alk,
             tot_hard,
             na = tot_na,
             ca = tot_ca,
             mg = tot_mg,
             k = tot_k,
             cl = tot_cl,
             so4 = tot_so4,
             hco3, co3, h, oh,
             tot_co3,
             tot_ocl,
             cba,
             kw,
             alk_eq)


}
