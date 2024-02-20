# General functions
# These functions include formatting and general helper functions


# Create water class
setClass("water",
  representation(ph = "numeric",
    temp = "numeric",
    alk = "numeric",
    # tot_hard = "numeric",
    na = "numeric",
    ca = "numeric",
    mg = "numeric",
    k = "numeric",
    cl = "numeric",
    so4 = "numeric",
    po4 = "numeric",
    hco3 = "numeric",
    co3 = "numeric",
    h = "numeric",
    oh = "numeric",
    tot_ocl = "numeric",
    tot_co3 = "numeric",
    kw = "numeric",
    alk_eq = "numeric"),
  prototype(ph = NA_real_,
    temp = NA_real_,
    alk = NA_real_,
    # tot_hard = NA_real_,
    na = 0,
    ca = 0,
    mg = 0,
    k = 0,
    cl = 0,
    so4 = 0,
    po4 = 0,
    hco3 = NA_real_,
    co3 = NA_real_,
    h = NA_real_,
    oh = NA_real_,
    tot_ocl = 0,
    tot_co3 = NA_real_,
    kw = NA_real_,
    alk_eq = NA_real_))

setMethod("show",
  "water",
  function(object) {
    cat("pH: ", object@ph, "\n")
    cat("Temperature (deg C): ", object@temp, "\n")
    cat("Alkalinity (mg/L CaCO3): ", object@alk, "\n")
    # cat("Hardness (mg/L CaCO3): ", object@tot_hard, "\n")
    cat("Sodium (M): ", object@na, "\n")
    cat("Calcium (M): ", object@ca, "\n")
    cat("Magnesium (M): ", object@mg, "\n")
    cat("Potassium (M): ", object@k, "\n")
    cat("Chloride (M): ", object@cl, "\n")
    cat("Sulfate (M): ", object@so4, "\n")
    cat("Phosphate (M)", object@po4, "\n")
    cat("Bicarbonate ion (M): ", object@hco3, "\n")
    cat("Carbonate ion (M): ", object@co3, "\n")
    cat("H+ ion (M): ", object@h, "\n")
    cat("OH- ion (M): ", object@oh, "\n")
    cat("Total OCl (M): ", object@tot_ocl, "\n")
    cat("Total carbonate (M): ", object@tot_co3, "\n")
    cat("Kw: ", object@kw, "\n")
    cat("Alkalinity (eq/L):", object@alk_eq)

  })


#' Define water vector
#'
#' This function takes water quality parameters and creates a "water" object that forms the input and output of all pH functions.
#' Carbonate balance is calculated and units are converted to mol/L
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
#' @param tot_ocl Chlorine in mg/L as ??
#' @param po4 Phosphate in mg/L as PO4
#'
#' @examples
#' # Put example code here
#'
#' @export
#'
define_water <- function(ph, temp, alk, tot_hard, ca_hard, na, k, cl, so4, tot_ocl = 0, po4 = 0) {

  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (missing(ph)) {
    ph = NA_real_
    warning("Missing value for pH. Carbonate balance will not be calculated.")
  }

  if (missing(temp)) {
    temp = 20
    warning("Missing value for temperature. Default of 20C will be used.")
  }

  if (missing(alk)) {
    alk = NA_real_
    warning("Missing value for alkalinity. Carbonate balance will not be calculated.")
  }

  if (missing(tot_hard)) {
    tot_hard = 0
    warning("Missing value for total hardness. Default value of 0 will be used.")
  }

  if (missing(ca_hard)) {
    ca_hard = tot_hard * .65
    warning("Missing value for calcium hardness. Default value of 65% of total will be used.")
  }

  if (missing(na) | missing(k)) {
    na = ifelse(missing(na), 0, na)
    k = ifelse(missing(k), 0, k)
    cl = ifelse(missing(cl), 0, cl)
    so4 = ifelse(missing(so4), 0, so4)
    warning("Missing value for cations and/or anions. Default values of 0 will be used. Use balance_ions to correct.")
  }

  # Calculate kw from temp
  tempa = temp + 273.15 # absolute temperature (K)
  pkw = round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1) # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  kw = 10^-pkw

  # convert major ion concentration inputs to mol/L
  na = convert_units(na, "na")
  ca = convert_units(ca_hard, "caco3")
  mg = convert_units(tot_hard - ca_hard, "caco3")
  k = convert_units(k, "k")
  cl = convert_units(cl, "cl")
  so4 = convert_units(so4, "so4")
  po4 = convert_units(po4, "po4")
  h = 10^-ph
  oh = kw / h

  # calculate carbonate system balance
  alpha1 = calculate_alpha1_carbonate(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2_carbonate(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as CO32-
  alk_eq = convert_units(alk, "caco3", startunit = "mg/L CaCO3", endunit = "eq/L") # convert alkalinity input to equivalents/L
  # convert_units(alk, "caco3", startunit = "mg/L CaCO3", endunit = "eq/L")
  tot_co3 = (alk_eq + h - oh) / (alpha1 + 2 * alpha2) # calculate total carbonate concentration
  hco3 = tot_co3 * alpha1
  co3 = tot_co3 * alpha2

  # Compile complete source water data frame to save to environment
  water_class <- new("water",
    ph = ph, temp = temp, alk = alk, # tot_hard = tot_hard,
    na = na, ca = ca, mg = mg, k = k, cl = cl, so4 = so4, po4 = po4,
    hco3 = hco3, co3 = co3, h = h, oh = oh,
    tot_ocl = tot_ocl, tot_co3 = tot_co3, kw = kw, alk_eq = alk_eq)

  return(water_class)
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
  if(class(water) != "water") {
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
  if(class(water) != "water") {
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
    CO3 = water@co3,
    H = water@h,
    OH = water@oh)

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
    labs(x = "Concentration (eq/L)",
      y = "Major cations and anions",
      title = title,
      subtitle = paste0("pH=", water@ph)) +
    guides(fill = "none")
}


#' Unit Conversions
#'
#' This function takes a value and converts units based on compound name.
#'
#' @param value Value to be converted
#' @param formula Chemical formula of compound. Accepts compounds in \code{\link{mweights}} for conversions between g and mol or eq
#' @param startunit Units of current value, currently accepts g/L; g/L CaCO3; M; eq/L; and the same units with "m", "u", "n" prefixes
#' @param endunit Desired units, currently accepts same as start units
#'
#' @examples
#' # Put example code here
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
  } else if (formula %in% c("so4", "caco3", "h2so4", "na2co3", "caoh2", "mgoh2", "mg", "ca")) {
    charge <- 2
  } else if (formula %in% c("h3po4", "al", "fe", "alum", "fecl3", "fe2so43")) {
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


#' Hardness calculation
#'
#' This function takes Ca and Mg in mg/L and returns hardness in mg/L as CaCO3
#'
#' @param ca Calcium concentration in mg/L as Ca
#' @param mg Magnesium concentration in mg/L as Mg
#' @param type "total" returns total hardness, "ca" returns calcium hardness
#' @param startunit Units of Ca and Mg. Defaults to mg/L
#'
#' @examples
#' # Put example code here
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

#' Ion balance a water
#'
#' This function takes a water defined by \code{\link{define_water}} and balances charge. If more cations are needed, sodium
#' will be added, unless a number for sodium is already provided and potassium is 0, then it will add potassium. Similarly,
#' anions are added using chloride, unless sulfate is 0.
#'
#' @param water Water created with define_water, which may have some ions set to 0 when unknown
#'
#' @examples
#' # Put example code here
#'
#' @export
#'
balance_ions <- function(water) {
  if(class(water) != "water") {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }
  # Set up ions to be changed
  na_new <- water@na
  k_new <- water@k
  cl_new <- water@cl
  so4_new <- water@so4

  # calculate charge
  cations <- water@na + 2 * water@ca + 2 * water@mg + water@k + water@h
  anions <- water@cl + 2 * water@so4 + water@hco3 + 2 * water@co3 + water@oh + water@tot_ocl

  if (is.na(cations) | is.na(anions)) {
    stop("Missing cations or anions for balance. Make sure pH and alkalinity are specified when define_water is called.")
  }

  # Add either sodium or potassium if cations are needed
  if (cations < anions) {
    add_cat <- anions - cations
    if (water@na == 0) {
      na_new <- add_cat
    } else if (water@k == 0) {
      k_new <- add_cat
    } else {
      na_new <- water@na + add_cat
    }
    # add chloride or sulfate if anions are needed
  } else if (anions < cations) {
    add_ani <- cations - anions
    if (water@cl == 0) {
      cl_new <- add_ani
    } else if (water@so4 == 0) {
      so4_new <- add_ani / 2
    } else {
      cl_new <- water@cl + add_ani
    }
  }

  water@na <- na_new
  water@k <- k_new
  water@cl <- cl_new
  water@so4 <- so4_new

  return(water)

}



# Functions to determine alpha from H+ and dissociation constants for carbonate
# Not exported
calculate_alpha1_carbonate <- function(h, k1, k2) {
  (k1 * h) / (h^2 + k1 * h + k1 * k2)
}

calculate_alpha2_carbonate <- function(h, k1, k2) {
  (k1 * k2) / (h^2 + k1 * h + k1 * k2)
}

# General temperature correction for equilibrium constants
# Temperature in deg C
# Eqn 5-9 WTP Model Manual (changed using Meyer masters thesis to include the correct temp correction)
pK_temp_adjust <- function(delta_h, k_a, temp) {
  R <- 8.314
  tempa <- temp + 273.15
  lnK <- log(k_a)
  -log10(exp((delta_h / R * (1 / 298.15 - 1 / tempa)) + lnK))
}

# discons$k1co3 delta_h = 7700
# discons$k2co3 delta_h = 14900

# Ionic strength calc

calculate_ionicstrength <- function(water) {

  # From all ions: IS = 0.5 * sum(M * z^2)
  0.5 * ((water@na + water@cl + water@k + water@hco3 + water@h + water@oh + water@tot_ocl) * 1^2 +
    (water@ca + water@mg + water@so4 + water@co3) * 2^2 +
    (water@po4) * 3^2)

}

correlate_ionicstrength <- function(water, from = "cond") {
  if (from == "cond") {
    # Snoeyink & Jenkins 1980
    1.6 * 10^-5 * water@cond
  } else if (from == "tds") {
    # MWH 2012 (5-38)
    2.5 * 10^-5 * water@tds
  } else {
    stop("Specify correlation from 'cond' or 'tds'.")
  }

}
