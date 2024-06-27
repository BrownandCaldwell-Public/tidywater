# General functions
# These functions include formatting and general helper functions


# Create water class
methods::setClass("water",

  representation(
    # General parameters
    ph = "numeric",
    temp = "numeric",
    alk = "numeric",
    tds = "numeric",
    cond = "numeric",
    tot_hard = "numeric",
    kw = "numeric",
    alk_eq = "numeric",

    # Carbon
    toc = "numeric",
    doc = "numeric",
    bdoc = "numeric",
    uv254 = "numeric",
    dic = "numeric",

    # Ions
    na = "numeric",
    ca = "numeric",
    mg = "numeric",
    k = "numeric",
    cl = "numeric",
    so4 = "numeric",
    no3 = "numeric",
    nh3 = "numeric",
    hco3 = "numeric",
    co3 = "numeric",
    h2po4 = "numeric",
    hpo4 = "numeric",
    po4 = "numeric",
    ocl = "numeric",
    h = "numeric",
    oh = "numeric",
    tot_po4 = "numeric",
    tot_ocl = "numeric",
    tot_co3 = "numeric",
    is = "numeric",
    br = "numeric",

    # Corrosion indices
    aggressive = "numeric",
    ryznar = "numeric",
    langelier = "numeric",
    ccpp = "numeric",
    larsonskold = "numeric",
    csmr = "numeric",

    # Miscellaneous
    treatment = "character",
    estimated = "character",

    # DBPs
    chcl3 = "numeric", # chloroform
    chcl2br = "numeric", # bromodichloromethane
    chbr2cl = "numeric", # dibromochloromethane
    chbr3 = "numeric", # bromoform
    tthm = "numeric",

    mcaa = "numeric", # chloroacetic acid
    dcaa = "numeric", # dichloroacetic acid
    tcaa = "numeric", # trichloroeacetic acid
    mbaa = "numeric", # bromoacetic acid
    dbaa = "numeric", # dibromoacetic acid
    haa5 = "numeric",

    bcaa = "numeric", # bromochloroacetic acid

    cdbaa = "numeric", # chlorodibromoacetic acid
    dcbaa = "numeric", # dichlorobromoacetic acid
    tbaa = "numeric" # tribromoacetic acid
  ),

  prototype(
    # General parameters
    ph = NA_real_,
    temp = NA_real_,
    alk = NA_real_,
    tds = NA_real_,
    cond = NA_real_,
    tot_hard = NA_real_,
    kw = NA_real_,
    alk_eq = NA_real_,

    # Carbon
    toc = NA_real_,
    doc = NA_real_,
    bdoc = NA_real_,
    dic = NA_real_,
    uv254 = NA_real_,

    # Ions
    na = 0,
    ca = 0,
    mg = 0,
    k = 0,
    cl = 0,
    so4 = 0,
    no3 = 0,
    nh3 = 0,
    hco3 = NA_real_,
    co3 = NA_real_,
    h2po4 = 0,
    hpo4 = 0,
    po4 = 0,
    ocl = 0,
    h = NA_real_,
    oh = NA_real_,
    tot_po4 = 0,
    tot_ocl = 0,
    tot_co3 = NA_real_,
    is = NA_real_,
    br = NA_real_,

    # Corrosion indices
    aggressive = NA_real_,
    ryznar = NA_real_,
    langelier = NA_real_,
    ccpp = NA_real_,
    larsonskold = NA_real_,
    csmr = NA_real_,

    # Miscellaneous
    treatment = "defined",
    estimated = "",

    # DBPs
    chcl3 = NA_real_, # chloroform
    chcl2br = NA_real_, # bromodichloromethane
    chbr2cl = NA_real_, # dibromochloromethane
    chbr3 = NA_real_, # bromoform
    tthm = NA_real_,

    mcaa = NA_real_, # chloroacetic acid
    dcaa = NA_real_, # dichloroacetic acid
    tcaa = NA_real_, # trichloroeacetic acid
    mbaa = NA_real_, # bromoacetic acid
    dbaa = NA_real_, # dibromoacetic acid
    haa5 = NA_real_,

    bcaa = NA_real_, # bromochloroacetic acid

    cdbaa = NA_real_, # chlorodibromoacetic acid
    dcbaa = NA_real_, # dichlorobromoacetic acid
    tbaa = NA_real_ # tribromoacetic acid

))

methods::setMethod("show",
  "water",
  function(object) {
    # General parameters
    cat("pH: ", object@ph, "\n")
    cat("Temperature (deg C): ", object@temp, "\n")
    cat("Alkalinity (mg/L CaCO3): ", object@alk, "\n")
    cat("Total Dissolved Solids (mg/L): ", object@tds, "\n")
    cat("Electrical conductivity (uS/cm): ", object@cond, "\n")
    cat("Total Hardness (mg/L CaCO3): ", object@tot_hard, "\n")
    cat("Kw: ", object@kw, "\n")
    cat("Alkalinity (eq/L): ", object@alk_eq, "\n")

    # Carbon
    cat("Total organic carbon (mg/L): ", object@toc, "\n")
    cat("Dissolved organic carbon (mg/L): ", object@doc, "\n")
    cat("Biodegradable dissolved organic carbon (mg/L): ", object@bdoc, "\n")
    cat("Dissolved inorganic carbon:", object@dic, "\n")
    cat("UV Absorbance at 254 nm (cm-1): ", object@uv254, "\n")

    # Ions
    cat("Sodium (M): ", object@na, "\n")
    cat("Calcium (M): ", object@ca, "\n")
    cat("Magnesium (M): ", object@mg, "\n")
    cat("Potassium (M): ", object@k, "\n")
    cat("Chloride (M): ", object@cl, "\n")
    cat("Sulfate (M): ", object@so4, "\n")
    cat("Nitrate (M): ", object@no3, "\n")
    cat("Ammonia (M): ", object@nh3, "\n")
    cat("Bicarbonate ion (M): ", object@hco3, "\n")
    cat("Carbonate ion (M): ", object@co3, "\n")
    cat("Dihydrogen phosphate ion - H2PO4 (M): ", object@h2po4, "\n")
    cat("Hydrogen phosphate ion - HPO4 (M): ", object@hpo4, "\n")
    cat("Phosphate ion (M): ", object@po4, "\n")
    cat("Hypochlorite ion (M): ", object@ocl, "\n")
    cat("H+ ion (M): ", object@h, "\n")
    cat("OH- ion (M): ", object@oh, "\n")
    cat("Total phosphate (M)", object@tot_po4, "\n")
    cat("Total OCl (M): ", object@tot_ocl, "\n")
    cat("Total carbonate (M): ", object@tot_co3, "\n")
    cat("Ionic Strength:", object@is, "\n")
    cat("Bromide (ug/L): ", object@br, "\n")

    # Corrosion indices
    cat("Aggressive Index (unitless):", object@aggressive, "\n")
    cat("Ryznar Stability Index (unitless):", object@ryznar, "\n")
    cat("Langelier Saturation Index (unitless):", object@langelier, "\n")
    cat("Calcium carbonate precipitation potential (mg/L CaCO3):", object@ccpp, "\n")
    cat("Larson-Skold Index (unitless):", object@larsonskold, "\n")
    cat("Chloride to sulfate mass ratio (unitless):", object@csmr, "\n")

    # Miscellaneous
    cat("Treatment applied to water class:", object@treatment, "\n")
    cat("List of parameters estimated by tidywater:", object@estimated, "\n")

    # DBPs
    cat("Chloroform (ug/L): ", object@chcl3, "\n")
    cat("Bromodichloromethane (ug/L): ", object@chcl2br, "\n")
    cat("Dibromochloromethane (ug/L): ", object@chbr2cl, "\n")
    cat("Bromoform (ug/L): ", object@chbr3, "\n")
    cat("Total trihalomethanes (ug/L): ", object@tthm, "\n")

    cat("Chloroacetic acid (ug/L): ", object@mcaa, "\n")
    cat("Dichloroacetic acid (ug/L): ", object@dcaa, "\n")
    cat("Trichloroacetic acid (ug/L): ", object@tcaa, "\n")
    cat("Bromoacetic acid (ug/L): ", object@mbaa, "\n")
    cat("Dibromoacetic acid (ug/L): ", object@dbaa, "\n")
    cat("Sum of 5 haloacetic acids (ug/L): ", object@haa5, "\n")

    cat("Bromochloroacetic acid (ug/L): ", object@bcaa, "\n")

    cat("Chlorodibromoacetic acid (ug/L): ", object@cdbaa, "\n")
    cat("Dichlorobromoacetic acid (ug/L): ", object@dcbaa, "\n")
    cat("Tribromoacetic acid (ug/L): ", object@tbaa, "\n")
  })

#' Create a water class object given water quality parameters
#'
#' This function takes user-defined water quality parameters and creates an S4 "water" class object that forms the input and output of all tidywater models.
#' Carbonate balance is calculated and units are converted to mol/L. Ionic strength is determined from ions, TDS, or conductivity. Missing values are handled by defaulting to 0 or
#' NA. Calcium hardness defaults to 65% of the total hardness because that falls within a typical range. For best results
#' manually specify all ions in the define_water arguments. The following equations are used to determine ionic strength:
#' Ionic strength (if TDS provided): Crittenden et al. (2012) equation 5-38
#' Ionic strength (if electrical conductivity provided): Snoeyink & Jenkins (1980)
#' Ionic strength (from ion concentrations): Lewis and Randall (1921), Crittenden et al. (2012) equation 5-37
#' Temperature correction of dielectric constant (relative permittivity): Harned and Owen (1958), Crittenden et al. (2012) equation 5-45.
#'
#' @param ph water pH
#' @param temp Temperature in degree C
#' @param alk Alkalinity in mg/L as CaCO3
#' @param tot_hard Total hardness in mg/L as CaCO3
#' @param ca_hard Calcium hardness in mg/L as CaCO3
#' @param na Sodium in mg/L Na+
#' @param k Potassium in mg/L K+
#' @param cl Chloride in mg/L Cl-
#' @param so4 Sulfate in mg/L SO42-
#' @param tot_ocl Chlorine in mg/L as Cl2. Used when a starting water has a chlorine residual.
#' @param tot_po4 Phosphate in mg/L as PO4 3-. Used when a starting water has a phosphate residual.
#' @param tds Total Dissolved Solids in mg/L (optional if ions are known)
#' @param cond Electrical conductivity in uS/cm (optional if ions are known)
#' @param toc Total organic carbon (TOC) in mg/L
#' @param doc Dissolved organic carbon (DOC) in mg/L
#' @param uv254 UV absorbance at 254 nm (cm-1)
#' @param br Bromide in mg/L Br-
#' @examples
#' water_missingions <- define_water(ph = 7, temp = 15, alk = 100, tds = 10)
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#'
#' @export
#'
define_water <- function(ph, temp = 20, alk, tot_hard, ca_hard, na, k, cl, so4, tot_ocl = 0, tot_po4 = 0, tds, cond,
                         toc, doc, uv254, br) {

  # Initialize string for tracking which parameters were estimated
  estimated <- ""

  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (missing(ph)) {
    ph = NA_real_
    warning("Missing value for pH. Carbonate balance will not be calculated.")
  }

  if (missing(alk)) {
    alk = NA_real_
    warning("Missing value for alkalinity. Carbonate balance will not be calculated.")
  }

  if (missing(tot_hard) & missing(ca_hard)) {
    tot_hard = NA_real_
    ca_hard = NA_real_
  }

  if (missing(tot_hard)) {
    tot_hard = ca_hard / 0.65
    warning("Missing value for total hardness. Default value of 154% of calcium hardness will be used.")
    estimated <- paste(estimated, "tot_hard", sep = "_")
  }

  if (missing(ca_hard)) {
    ca_hard = tot_hard * .65
    warning("Missing value for calcium hardness. Default value of 65% of total hardness will be used.")
    estimated <- paste(estimated, "ca", sep = "_")
  }

  tds = ifelse(missing(tds), NA_real_, tds)
  cond = ifelse(missing(cond), NA_real_, cond)
  br = ifelse(missing(br), NA_real_, br)

  na = ifelse(missing(na), NA_real_, na)
  k = ifelse(missing(k), NA_real_, k)
  cl = ifelse(missing(cl), NA_real_, cl)
  so4 = ifelse(missing(so4), NA_real_, so4)

  if (missing(toc) & missing(doc) & missing(uv254)) {
    toc = NA_real_
    doc = NA_real_
    uv254 = NA_real_
  } else if (missing(toc) & missing(doc)) {
    toc = NA_real_
    doc = NA_real_
  } else if (missing(toc) & !missing(doc)) {
    warning("Missing value for TOC. DOC assumed to be 95% of TOC.")
    toc = doc / 0.95
    estimated <- paste(estimated, "toc", sep = "_")
  } else if (missing(doc) & !missing(toc)) {
    warning("Missing value for DOC. Default value of 95% of TOC will be used.")
    doc = toc * 0.95
    estimated <- paste(estimated, "doc", sep = "_")
  }

  uv254 = ifelse(missing(uv254), NA_real_, uv254)

  # Calculate temperature dependent constants
  tempa = temp + 273.15 # absolute temperature (K)
  # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  pkw = round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1)
  kw = 10^-pkw

  # Convert major ion concentration inputs to mol/L
  na = convert_units(na, "na")
  ca = convert_units(ca_hard, "caco3")
  mg = convert_units(tot_hard - ca_hard, "caco3")
  k = convert_units(k, "k")
  cl = convert_units(cl, "cl")
  so4 = convert_units(so4, "so4")
  tot_po4 = convert_units(tot_po4, "po4")
  tot_ocl = convert_units(tot_ocl, "cl2")
  h = 10^-ph
  oh = kw / h
  # convert alkalinity input to equivalents/L
  carb_alk_eq = convert_units(alk, "caco3", startunit = "mg/L CaCO3", endunit = "eq/L")
  # calculate total carbonate concentration
  # Initial alpha values (not corrected for IS)
  k1co3 = filter(discons, ID == "k1co3") %$%
    K_temp_adjust(deltah, k, temp)
  k2co3 = filter(discons, ID == "k2co3") %$%
    K_temp_adjust(deltah, k, temp)

  alpha1 = calculate_alpha1_carbonate(h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2_carbonate(h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as CO32-
  tot_co3 = (carb_alk_eq + h - oh) / (alpha1 + 2 * alpha2)

  # Initialize water to simplify IS calcs
  water <- methods::new("water",
    ph = ph, temp = temp, alk = alk, tds = tds, cond = cond, tot_hard = tot_hard,
    na = na, ca = ca, mg = mg, k = k, cl = cl, so4 = so4,
    hco3 = tot_co3 * alpha1, co3 = tot_co3 * alpha2, h2po4 = 0, hpo4 = 0, po4 = 0, ocl = 0,
    h = h, oh = oh,
    tot_po4 = tot_po4, tot_ocl = tot_ocl, tot_co3 = tot_co3,
    kw = kw, is = 0, alk_eq = carb_alk_eq,
    doc = doc, toc = toc, uv254 = uv254, br = br)


  # Determine ionic strength

  if (!is.na(tds)) {
    water@is = correlate_ionicstrength(tds, from = "tds")
    water@cond = correlate_ionicstrength(tds, from = "tds", to = "cond")
    estimated <- paste(estimated, "cond", sep = "_")
  } else if (!is.na(cond)) {
    water@is = correlate_ionicstrength(cond, from = "cond")
    water@tds = correlate_ionicstrength(cond, from = "cond", to = "tds")
    estimated <- paste(estimated, "tds", sep = "_")
  } else if (is.na(tds) & is.na(cond) & ((!is.na(ca) | !is.na(na)) & (!is.na(cl) | !is.na(so4)) & alk > 0) & !is.na(ph)) {
    water@is = calculate_ionicstrength(water)
    water@tds = correlate_ionicstrength(water@is, from = "is", to = "tds")
    estimated <- paste(estimated, "tds", sep = "_")
    water@cond = correlate_ionicstrength(water@is, from = "is", to = "cond")
    estimated <- paste(estimated, "cond", sep = "_")
  } else {
    warning("Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.")
    water@is = NA_real_
  }

  # Eq constants
  ks <- correct_k(water)

  # Carbonate and phosphate ions and ocl ions
  alpha1 = calculate_alpha1_carbonate(h, ks) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2_carbonate(h, ks) # proportion of total carbonate as CO32-
  water@tot_co3 = (carb_alk_eq + h - oh) / (alpha1 + 2 * alpha2)
  water@hco3 = water@tot_co3 * alpha1
  water@co3 = water@tot_co3 * alpha2

  alpha1p = calculate_alpha1_phosphate(h, ks)
  alpha2p = calculate_alpha2_phosphate(h, ks)
  alpha3p = calculate_alpha3_phosphate(h, ks)

  water@h2po4 = tot_po4 * alpha1p
  water@hpo4 = tot_po4 * alpha2p
  water@po4 = tot_po4 * alpha3p

  water@ocl = tot_ocl * calculate_alpha1_hypochlorite(h, ks)

  # Calculate total alkalinity (set equal to carbonate alkalinity for now)
  water@alk_eq = carb_alk_eq

  # Add all estimated values to water slot
  water@estimated = estimated

  return(water)
}

#' Create summary table from water class
#'
#' This function takes a water data frame defined by \code{\link{define_water}} and outputs a formatted summary table of
#' general water quality parameters and major ions.
#'
#' @param water Source water vector created by \code{\link{define_water}}.
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#' summarise_wq(water_defined)
#'
#' @export
#'
summarise_wq <- function(water) {
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }
  # Compile main WQ parameters to print
  params = data.frame(pH = water@ph,
    Temp = water@temp,
    Alkalinity = water@alk,
    Total_Hardness = calculate_hardness(water@ca, water@mg, startunit = "M"))

  params = params %>%
    pivot_longer(c(pH:Total_Hardness), names_to = "param", values_to = "result") %>%
    mutate(units = c("-", "deg C", "mg/L as CaCO3", "mg/L as CaCO3"))

  tab1 = knitr::kable(params,
    format = "simple",
    col.names = c("Key water quality parameters", "Result", "Units"))

  # Compile major ions to print
  ions = data.frame(Na = convert_units(water@na, "na", "M", "mg/L"),
    Ca = convert_units(water@ca, "ca", "M", "mg/L"),
    Mg = convert_units(water@mg, "mg", "M", "mg/L"),
    K = convert_units(water@k, "k", "M", "mg/L"),
    Cl = convert_units(water@cl, "cl", "M", "mg/L"),
    SO4 = convert_units(water@so4, "so4", "M", "mg/L"),
    HCO3 = convert_units(water@hco3, "hco3", "M", "mg/L"),
    CO3 = convert_units(water@co3, "co3", "M", "mg/L"))

  ions = ions %>%
    pivot_longer(c(Na:CO3), names_to = "ion", values_to = "c_mg")

  tab2 = knitr::kable(ions,
    format = "simple",
    col.names = c("Major ions", "Concentration (mg/L)"),
    # format.args = list(scientific = TRUE),
    digits = 2)

  # print(kables(list(tab1,tab2)))
  return(knitr::kables(list(tab1, tab2)))
}

#' Create summary plot of ions from water class
#'
#' This function takes a water data frame defined by \code{\link{define_water}} and outputs an ion balance plot.
#'
#' @param water Source water vector created by link function here
#' @import ggplot2
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#' plot_ions(water_defined)
#'
#' @export
#'
plot_ions <- function(water) {
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }
  # Compile major ions to plot
  ions = data.frame(Na = water@na,
    Ca = water@ca * 2,
    Mg = water@mg * 2,
    K = water@k,
    Cl = water@cl,
    SO4 = water@so4 * 2,
    HCO3 = water@hco3,
    CO3 = water@co3 * 2,
    H2PO4 = water@h2po4,
    HPO4 = water@hpo4 * 2,
    PO4 = water@po4 * 3,
    OCl = water@ocl,
    H = water@h,
    OH = water@oh)

  ions %>%
    pivot_longer(c(Na:OH), names_to = "ion", values_to = "concentration") %>%
    mutate(type = case_when(ion %in% c("Na", "Ca", "Mg", "K", "H") ~ "Cations", TRUE ~ "Anions")) %>%
    arrange(type, concentration) %>%
    mutate(label_pos = cumsum(concentration) - concentration / 2, .by = type,
      label_y = case_when(type == "Cations" ~ 2 - .2, TRUE ~ 1 - .2)) %>%

    ggplot(aes(x = concentration, y = type, fill = reorder(ion, -concentration))) +
    geom_bar(stat = "identity",
      width = 0.5,
      alpha = 0.5,
      color = "black") +
    geom_text(aes(x = label_pos, label = ifelse(concentration > 10e-5, ion, ""), fontface = "bold", angle = 90),
      size = 3.5) +
    ggrepel::geom_text_repel(aes(x = label_pos, y = label_y,
      label = ifelse(concentration <= 10e-5 & concentration > 0, ion, ""),
      fontface = "bold"),
    size = 3.5,
    nudge_y = -.2,
    seed = 555) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    labs(x = "Concentration (eq/L)",
      y = "Major cations and anions",
      subtitle = paste0("pH=", water@ph, "\nAlkalinity=", water@alk)) +
    guides(fill = "none")
}

#' Create DBP summary table from water class
#'
#' This function takes a water data frame defined by \code{\link{chemdose_dbp}} and and outputs a formatted summary table of
#' modeled DBP concentrations.
#'
#' @param water Source water vector created by \code{\link{chemdose_dbp}}.
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#' summarise_dbp(water_defined)
#'
#' @export
#'
summarise_dbp <- function(water) {
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water. Model DBP formation using chemdose_dbp")
  }
  # Compile main WQ parameters to print
  thms = data.frame(Chloroform = ifelse(length(water@chcl3) == 0, NA, water@chcl3),
    Bromodichloromethane = ifelse(length(water@chcl2br) == 0, NA, water@chcl2br),
    Dibromochloromethane = ifelse(length(water@chbr2cl) == 0, NA, water@chbr2cl),
    Bromoform = ifelse(length(water@chbr3) == 0, NA, water@chbr3),
    Total_trihalomethanes = ifelse(length(water@tthm) == 0, NA, water@tthm))


  haas = data.frame(Chloroacetic_acid = ifelse(length(water@mcaa) == 0, NA, water@mcaa),
    Dichloroacetic_acid = ifelse(length(water@dcaa) == 0, NA, water@dcaa),
    Trichloroacetic_acid = ifelse(length(water@tcaa) == 0, NA, water@tcaa),
    Bromoacetic_acid = ifelse(length(water@mbaa) == 0, NA, water@mbaa),
    Dibromoacetic_acid = ifelse(length(water@dbaa) == 0, NA, water@dbaa),
    Sum_5_haloacetic_acids = ifelse(length(water@haa5) == 0, NA, water@haa5))
  # Bromochloroacetic_acid = ifelse(length(water@bcaa)==0, NA, water@bcaa),
  # Sum_6_haloacetic_acids = ifelse(length(water@haa6)==0, NA, water@haa6),
  # Chlorodibromoacetic_acid = ifelse(length(water@cdbaa)==0, NA, water@cdbaa),
  # Dichlorobromoacetic_acid = ifelse(length(water@dcbaa)==0, NA, water@dcbaa),
  # Tribromoacetic_acid = ifelse(length(water@tbaa)==0, NA, water@tbaa),
  # Sum_9_haloacetic_acids = ifelse(length(water@haa9)==0, NA, water@haa9))


  thms = thms %>%
    pivot_longer(c(Chloroform:Total_trihalomethanes), names_to = "param", values_to = "result") %>%
    mutate(result = round(result, 2))

  haas = haas %>%
    pivot_longer(c(Chloroacetic_acid:Sum_5_haloacetic_acids), names_to = "param", values_to = "result") %>%

    mutate(result = round(result, 2))

  thms = knitr::kable(thms,
    format = "simple",
    col.names = c("THMs", "Modeled concentration (ug/L)"))

  haas = knitr::kable(haas,
    format = "simple",
    col.names = c("HAAs", "Modeled concentration (ug/L)"))

  return(knitr::kables(list(thms, haas)))
}

#' Calculate unit conversions for common compounds
#'
#' This function takes a value and converts units based on compound name.
#'
#' @param value Value to be converted
#' @param formula Chemical formula of compound. Accepts compounds in mweights for conversions between g and mol or eq
#' @param startunit Units of current value, currently accepts g/L; g/L CaCO3; M; eq/L; and the same units with "m", "u", "n" prefixes
#' @param endunit Desired units, currently accepts same as start units
#'
#' @examples
#' convert_units(50, "ca") # converts from mg/L to M by default
#' convert_units(50, "ca", "mg/L", "mg/L CaCO3")
#' convert_units(50, "ca", startunit = "mg/L", endunit = "eq/L")
#'
#' @export
#'
convert_units <- function(value, formula, startunit = "mg/L", endunit = "M") {

  milli_list <- c("mg/L", "mg/L CaCO3", "mM", "meq/L")
  mcro_list <- c("ug/L", "ug/L CaCO3", "uM", "ueq/L")
  nano_list <- c("ng/L", "ng/L CaCO3", "nM", "neq/L")
  stand_list <- c("g/L", "g/L CaCO3", "M", "eq/L")

  gram_list <- c("ng/L", "ug/L", "mg/L", "g/L", "mg/L CaCO3", "g/L CaCO3")
  mole_list <- c("M", "mM", "uM", "nM")
  eqvl_list <- c("neq/L", "ueq/L", "meq/L", "eq/L")

  caco_list <- c("mg/L CaCO3", "g/L CaCO3", "ug/L CaCO3", "ng/L CaCO3")

  # Determine multiplier for order of magnitude conversion
  # In the same list, no multiplier needed
  if ((startunit %in% milli_list & endunit %in% milli_list) |
    (startunit %in% stand_list & endunit %in% stand_list) |
    (startunit %in% nano_list & endunit %in% nano_list) |
    (startunit %in% mcro_list & endunit %in% mcro_list)) {
    multiplier <- 1
    # m - standard, n-u, u-n
  } else if ((startunit %in% milli_list & endunit %in% stand_list) |
    (startunit %in% mcro_list & endunit %in% milli_list) |
    (startunit %in% nano_list & endunit %in% mcro_list)) {
    multiplier <- 1e-3
  } else if ((startunit %in% stand_list & endunit %in% milli_list) |
    (startunit %in% milli_list & endunit %in% mcro_list) |
    (startunit %in% mcro_list & endunit %in% nano_list)) {
    multiplier <- 1e3
    # u - standard
  } else if ((startunit %in% mcro_list & endunit %in% stand_list) |
    (startunit %in% nano_list & endunit %in% milli_list)) {
    multiplier <- 1e-6
  } else if ((startunit %in% stand_list & endunit %in% mcro_list) |
    (startunit %in% milli_list & endunit %in% nano_list)) {
    multiplier <- 1e6
    # n - standard
  } else if (startunit %in% nano_list & endunit %in% stand_list) {
    multiplier <- 1e-9
  } else if (startunit %in% stand_list & endunit %in% nano_list) {
    multiplier <- 1e9
  } else {
    stop("Units not supported")
  }

  # Need molar mass of CaCO3
  caco3_mw <- as.numeric(mweights["caco3"])

  # Determine relevant molar weight
  if (formula %in% colnames(mweights)) {
    if ((startunit %in% caco_list & endunit %in% c(mole_list, eqvl_list)) |
      (endunit %in% caco_list & startunit %in% c(mole_list, eqvl_list))) {
      molar_weight <- as.numeric(mweights["caco3"])
    } else {
      molar_weight <- as.numeric(mweights[formula])
    }
  } else if (!(startunit %in% gram_list) & !(endunit %in% gram_list)) {
    molar_weight <- 0
  } else {
    stop("Chemical formula not supported")
  }

  # Determine charge for equivalents
  if (formula %in% c("na", "k", "cl", "hcl", "naoh", "nahco3", "na")) {
    charge <- 1
  } else if (formula %in% c("so4", "caco3", "h2so4", "na2co3", "caoh2", "mgoh2", "mg", "ca", "pb", "cacl2")) {
    charge <- 2
  } else if (formula %in% c("h3po4", "al", "fe", "alum", "fecl3", "fe2so43", "po4")) {
    charge <- 3
  } else if (!(startunit %in% eqvl_list) & !(endunit %in% eqvl_list)) {
    # This is included so that charge can be in equations later without impacting results
    charge <- 1
  } else {
    stop("Unable to find charge for equivalent conversion")
  }

  # Unit conversion
  # g - mol
  if (startunit %in% gram_list & endunit %in% mole_list) {
    value / molar_weight * multiplier
  } else if (startunit %in% mole_list & endunit %in% gram_list) {
    value * molar_weight * multiplier
    # g - eq
  } else if (startunit %in% eqvl_list & endunit %in% gram_list) {
    value / charge * molar_weight * multiplier
  } else if (startunit %in% gram_list & endunit %in% eqvl_list) {
    value / molar_weight * charge * multiplier
    # mol - eq
  } else if (startunit %in% mole_list & endunit %in% eqvl_list) {
    value * charge * multiplier
  } else if (startunit %in% eqvl_list & endunit %in% mole_list) {
    value / charge * multiplier
    # g CaCO3 - g
  } else if (startunit %in% caco_list & endunit %in% gram_list & !(endunit %in% caco_list)) {
    value / caco3_mw * molar_weight
  } else if (endunit %in% caco_list & startunit %in% gram_list & !(startunit %in% caco_list)) {
    value / molar_weight * caco3_mw
    # same lists
  } else if ((startunit %in% gram_list & endunit %in% gram_list) |
    (startunit %in% mole_list & endunit %in% mole_list) |
    (startunit %in% eqvl_list & endunit %in% eqvl_list)) {
    value * multiplier
  } else {
    stop("Units not supported")
  }


}


#' Calculate hardness from calcium and magnesium
#'
#' This function takes Ca and Mg in mg/L and returns hardness in mg/L as CaCO3
#'
#' @param ca Calcium concentration in mg/L as Ca
#' @param mg Magnesium concentration in mg/L as Mg
#' @param type "total" returns total hardness, "ca" returns calcium hardness. Defaults to "total"
#' @param startunit Units of Ca and Mg. Defaults to mg/L
#'
#' @examples
#' calculate_hardness(50, 10)
#'
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#' calculate_hardness(water_defined@ca, water_defined@mg, "total", "M")
#'
#' @export
#'
calculate_hardness <- function(ca, mg, type = "total", startunit = "mg/L") {
  ca <- convert_units(ca, "ca", startunit, "mg/L CaCO3")
  mg <- convert_units(mg, "mg", startunit, "mg/L CaCO3")
  tot_hard <- ca + mg
  ca_hard <- ca

  if (type == "total") {
    tot_hard
  } else if (type == "ca") {
    ca_hard
  } else {
    stop("Unsupported type. Specify 'total' or 'ca'")
  }

}

#' Add Na, K, Cl, or SO4 to balance overall charge in a water
#'
#' This function takes a water defined by \code{\link{define_water}} and balances charge. If more cations are needed, sodium
#' will be added, unless a number for sodium is already provided and potassium is 0, then it will add potassium. Similarly,
#' anions are added using chloride, unless sulfate is 0. If calcium and magnesium are not specified when defining a water with
#' \code{\link{define_water}}, they will default to 0 and not be changed by this function.  This function is purely mathematical.
#' User should always check the outputs to make sure values are reasonable for the input source water.
#'
#' @param water Water created with define_water, which may have some ions set to 0 when unknown
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1) %>%
#'   balance_ions()
#'
#' @export
#'
balance_ions <- function(water) {
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }
  # Set up ions to be changed
  na_new <- water@na
  k_new <- water@k
  cl_new <- water@cl
  so4_new <- water@so4

  # calculate charge
  cations <- sum(water@na, 2 * water@ca, 2 * water@mg, water@k, water@h, na.rm = TRUE)
  anions <- sum(water@cl, 2 * water@so4, water@hco3, 2 * water@co3, water@h2po4, 2 * water@hpo4, 3 * water@po4,
    water@oh, water@ocl, na.rm = TRUE)

  if (is.na(cations) | is.na(anions)) {
    stop("Missing cations or anions for balance. Make sure pH and alkalinity are specified when define_water is called.")
  }

  # Initialize these objects so they can be used later.
  add_na <- 0
  add_k <- 0
  add_cl <- 0
  add_so4 <- 0
  # Add either sodium or potassium if cations are needed
  # Sodium is preferred because it's often present and not measured.
  # Potassium is usually low, but if it's the only cation not measured, it can be added.
  # No defaut behavior to add Ca or Mg because those are frequently measured.
  if (cations < anions) {
    add_cat <- anions - cations
    if (is.na(water@na)) {
      add_na <- add_cat
      na_new <- add_na
      water@estimated <- paste(water@estimated, "na", sep = "_")
    } else if (is.na(water@k)) {
      add_k <- add_cat
      k_new <- add_k
      water@estimated <- paste(water@estimated, "k", sep = "_")
    } else {
      add_na <- add_cat
      na_new <- water@na + add_na
      water@estimated <- paste(water@estimated, "na", sep = "_")
    }
    # add chloride or sulfate if anions are needed
    # Similar logic to cations, although sulfate is typically at higher concentrations than potassium.
    # Pretty standard to add Na and Cl because those are just regular salt. It does affect CSMR, but almost nothing else.
  } else if (anions < cations) {
    add_ani <- cations - anions
    if (is.na(water@cl)) {
      add_cl <- add_ani
      cl_new <- add_cl
      water@estimated <- paste(water@estimated, "cl", sep = "_")
    } else if (is.na(water@so4)) {
      add_so4 <- add_ani / 2
      so4_new <- add_so4
      water@estimated <- paste(water@estimated, "so4", sep = "_")
    } else {
      add_cl <- add_ani
      cl_new <- water@cl + add_cl
      water@estimated <- paste(water@estimated, "cl", sep = "_")
    }
  }

  water@na <- na_new
  water@k <- k_new
  water@cl <- cl_new
  water@so4 <- so4_new
  water@treatment <- paste(water@treatment, "_balanced", sep = "")

  # Update TDS/cond/IS if needed.
  if (grepl("tds", water@estimated) & grepl("cond", water@estimated)) {
    # Update TDS and cond if they were estimated from IS. Otherwise, assume initial values were measured.
    water@tds = water@tds + convert_units(add_na, "na", "M", "mg/L") + convert_units(add_k, "k", "M", "mg/L") +
      convert_units(add_cl, "cl", "M", "mg/L") + convert_units(add_so4, "so4", "M", "mg/L")
    water@cond <- correlate_ionicstrength(water@tds, from = "tds", to = "cond")
    # Similarly, IS should only update from the ion balance if TDS and cond were estimates.
    water@is = calculate_ionicstrength(water)

  }

  return(water)
}

# Non-exported functions
# View reference list at https://github.com/BrownandCaldwell/tidywater/wiki/References

# Functions to determine alpha from H+ and dissociation constants for carbonate
calculate_alpha1_carbonate <- function(h, k) {
  k1 = k$k1co3
  k2 = k$k2co3
  (k1 * h) / (h^2 + k1 * h + k1 * k2)
}

calculate_alpha2_carbonate <- function(h, k) {
  k1 = k$k1co3
  k2 = k$k2co3
  (k1 * k2) / (h^2 + k1 * h + k1 * k2)
}

# Equations from Benjamin (2014) Table 5.3b
calculate_alpha0_phosphate <- function(h, k) {
  k1 = k$k1po4
  k2 = k$k2po4
  k3 = k$k3po4
  1 / (1 + (k1 / h) + (k1 * k2 / h^2) + (k1 * k2 * k3 / h^3))
}

calculate_alpha1_phosphate <- function(h, k) { # H2PO4
  k1 = k$k1po4
  k2 = k$k2po4
  k3 = k$k3po4
  calculate_alpha0_phosphate(h, k) * k1 / h
}

calculate_alpha2_phosphate <- function(h, k) { # HPO4
  k1 = k$k1po4
  k2 = k$k2po4
  k3 = k$k3po4
  calculate_alpha0_phosphate(h, k) * (k1 * k2 / h^2)
}

calculate_alpha3_phosphate <- function(h, k) { # PO4
  k1 = k$k1po4
  k2 = k$k2po4
  k3 = k$k3po4
  calculate_alpha0_phosphate(h, k) * (k1 * k2 * k3 / h^3)
}

calculate_alpha1_hypochlorite <- function(h, k) { # OCl
  k1 = k$kocl
  1 / (1 + h / k1)
}

# General temperature correction for equilibrium constants
# Temperature in deg C
# van't Hoff equation, from Crittenden et al. (2012) equation 5-68 and Benjamin (2010) equation 2-17
# Assumes delta H for a reaction doesn't change with temperature, which is valid for ~0-30 deg C

K_temp_adjust <- function(deltah, ka, temp) {
  R <- 8.314 # J/mol * K
  tempa <- temp + 273.15
  lnK <- log(ka)
  exp((deltah / R * (1 / 298.15 - 1 / tempa)) + lnK)
}


# Ionic strength calculation
# Crittenden et al (2012) equation 5-37

calculate_ionicstrength <- function(water) {
  # From all ions: IS = 0.5 * sum(M * z^2)
  0.5 * (sum(water@na, water@cl, water@k, water@hco3, water@h2po4, water@h, water@oh, water@tot_ocl, na.rm = TRUE) * 1^2 +
    sum(water@ca, water@mg, water@so4, water@co3, water@hpo4, na.rm = TRUE) * 2^2 +
    (water@po4) * 3^2)

}

correlate_ionicstrength <- function(result, from = "cond", to = "is") {
  if (from == "cond" & to == "is") {
    # Snoeyink & Jenkins (1980)
    1.6 * 10^-5 * result
  } else if (from == "tds" & to == "is") {
    # Crittenden et al. (2012) equation 5-38
    2.5 * 10^-5 * result
  } else if (from == "is" & to == "tds") {
    result / (2.5 * 10^-5)
  } else if (from == "is" & to == "cond") {
    result / (1.6 * 10^-5)
  } else if (from == "tds" & to == "cond") {
    result * (2.5 * 10^-5) / (1.6 * 10^-5)
  } else if (from == "cond" & to == "tds") {
    result * (1.6 * 10^-5) / (2.5 * 10^-5)
  } else {
    stop("from and to arguments must be one of 'is', 'tds', or 'cond'.")
  }

}

# Calculate activity coefficients
# Activity coefficients: Davies (1967), Crittenden et al. (2012) equation 5-43
# Activity coefficient constant A: Stumm and Morgan (1996), Trussell (1998), Crittenden et al. (2012) equation 5-44

calculate_activity <- function(z, is, temp) {
  tempa = temp + 273.15 # absolute temperature (K)

  # dielectric constant (relative permittivity) based on temperature from Harned and Owen (1958), Crittenden et al. (2012) equation 5-45
  de = 78.54 * (1 - (0.004579 * (tempa - 298)) + 11.9E-6 * (tempa - 298)^2 + 28E-9 * (tempa - 298)^3)

  # constant for use in calculating activity coefficients from Stumm and Morgan (1996), Trussell (1998), Crittenden et al. (2012) equation 5-44
  a = 1.29E6 * (sqrt(2) / ((de * tempa)^1.5))

  # Davies equation, Davies (1967), Crittenden et al. (2012) equation 5-43
  10^(-a * z^2 * ((is^0.5 / (1 + is^0.5)) - 0.3 * is))
}


# Correct acid dissociation constants for temperature and ionic strength
# Dissociation constants corrected for non-ideal solutions following Benjamin (2010) example 3.14.
# See k_temp_adjust for temperature correction equation.
correct_k <- function(water) {

  # Determine activity coefficients
  if (is.na(water@is)) {
    activity_z1 = 1
    activity_z2 = 1
    activity_z3 = 1
  } else {
    activity_z1 = calculate_activity(1, water@is, water@temp)
    activity_z2 = calculate_activity(2, water@is, water@temp)
    activity_z3 = calculate_activity(3, water@is, water@temp)
  }

  temp = water@temp

  # Eq constants
  k1co3 = filter(discons, ID == "k1co3") %$% # k1co3 = {h+}{hco3-}/{h2co3}
    K_temp_adjust(deltah, k, temp) / activity_z1^2
  k2co3 = filter(discons, ID == "k2co3") %$% # k2co3 = {h+}{co32-}/{hco3-}
    K_temp_adjust(deltah, k, temp) / activity_z2
  k1po4 = filter(discons, ID == "k1po4") %$% # k1po4 = {h+}{h2po4-}/{h3po4}
    K_temp_adjust(deltah, k, temp) / activity_z1^2
  k2po4 = filter(discons, ID == "k2po4") %$% # k2po4 = {h+}{hpo42-}/{h2po4-}
    K_temp_adjust(deltah, k, temp) / activity_z2
  k3po4 = filter(discons, ID == "k3po4") %$% # k3po4 = {h+}{po43-}/{hpo42-}
    K_temp_adjust(deltah, k, temp) * activity_z2 / (activity_z1 * activity_z3)
  kocl = filter(discons, ID == "kocl") %$% # kocl = {h+}{ocl-}/{hocl}
    K_temp_adjust(deltah, k, temp) / activity_z1^2
  kso4 = filter(discons, ID == "kso4") %$% # kso4 = {h+}{so42-}/{hso4-} Only one relevant dissociation for sulfuric acid in natural waters.
    K_temp_adjust(deltah, k, water@temp) / activity_z2

  return(data.frame("k1co3" = k1co3, "k2co3" = k2co3,
    "k1po4" = k1po4, "k2po4" = k2po4, "k3po4" = k3po4,
    "kocl" = kocl, "kso4" = kso4))

}
