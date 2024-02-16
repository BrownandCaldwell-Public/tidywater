
# General functions
# These functions include formatting and general helper functions

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
#' @param ca_hard Calcium hardness in mg/L as CaCO3
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
define_water <- function(ph, temp, alk, tot_hard, ca_hard, na, k, cl, so4, tot_ocl = 0, type) {
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
  
  if (missing(ca_hard) & missing(type)) {
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
  if (missing(ph) == FALSE & missing(temp) == FALSE & missing(alk) == FALSE & missing(tot_hard) == FALSE & missing(ca_hard) == FALSE
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
  
  if (missing(ca_hard) & missing(type) == FALSE) {
    ca_hard = wq$ca_hard
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
  na = convert_units(na, "na")
  ca = convert_units(ca, "caco3")
  mg = convert_units(tot_hard - ca_hard, "caco3")
  k = convert_units(k, "k")
  cl = convert_units(cl, "cl")
  so4 = convert_units(so4, "so4")
  h = 10^-ph
  oh = kw / h
  
  # calculate carbonate system balance
  alpha1 = calculate_alpha1(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as HCO3-
  alpha2 = calculate_alpha2(h, discons$k1co3, discons$k2co3) # proportion of total carbonate as CO32-
  alk_eq = convert_units(alk, "caco3", startunit = "mg/L CaCO3", endunit = "eq/L") # convert alkalinity input to equivalents/L
  #convert_units(alk, "caco3", startunit = "mg/L CaCO3", endunit = "eq/L")
  tot_co3 = (alk_eq + h - oh) / (alpha1 + 2 * alpha2) # calculate total carbonate concentration
  hco3 = tot_co3 * alpha1
  co3 = tot_co3 * alpha2
  
  # Compile complete source water data frame to save to environment
  water_df = data.frame(ph, temp, alk, tot_hard, na, ca, mg, k, cl, so4, hco3, co3, h, oh, tot_ocl, tot_co3, kw, alk_eq)
  return(water_df)
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


#' Unit Converstions
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
  if((startunit %in% milli_list & endunit %in% milli_list) | 
     (startunit %in% stand_list & endunit %in% stand_list) |
     (startunit %in% nano_list & endunit %in% nano_list) |
     (startunit %in% mcro_list & endunit %in% mcro_list)) {
    multiplier <- 1
    # m - standard, n-u, u-n
  } else if((startunit %in% milli_list & endunit %in% stand_list) |
            (startunit %in% mcro_list & endunit %in% milli_list) |
            (startunit %in% nano_list & endunit %in% mcro_list)) {
    multiplier <- 1e-3
  } else if((startunit %in% stand_list & endunit %in% milli_list) |
            (startunit %in% milli_list & endunit %in% mcro_list) |
            (startunit %in% mcro_list & endunit %in% nano_list)) {
    multiplier <- 1e3
    # u - standard
  } else if((startunit %in% mcro_list & endunit %in% stand_list) |
            (startunit %in% nano_list & endunit %in% milli_list)) {
    multiplier <- 1e-6
  } else if((startunit %in% stand_list & endunit %in% mcro_list) |
            (startunit %in% milli_list & endunit %in% nano_list)) {
    multiplier <- 1e6
    # n - standard
  } else if(startunit %in% nano_list & endunit %in% stand_list) {
    multiplier <- 1e-9
  } else if(startunit %in% stand_list & endunit %in% nano_list) {
    multiplier <- 1e9
  } else {
    stop("Units not supported")
  }
  
  # Need molar mass of CaCO3
  caco3_mw <- mweights["caco3"]
  
  # Determine relevant molar weight
  if(formula %in% colnames(mweights)) {
    if((startunit %in% caco_list & endunit %in% c(mole_list, eqvl_list)) |
       (endunit %in% caco_list & startunit %in% c(mole_list, eqvl_list))) {
      molar_weight <- mweights["caco3"]
    } else {
      molar_weight <- mweights[formula]
    }
  } else if(!(startunit %in% gram_list) & !(endunit %in% gram_list)) {
    molar_weight <- 0
  } else {
    stop("Chemical formula not supported")
  }
  
  # Determine charge for equivalents
  if(formula %in% c("na", "k", "cl", "hcl", "naoh", "nahco3", "na")) {
    charge <- 1
  } else if(formula %in% c("so4", "caco3", "h2so4", "na2co3", "caoh2", "mgoh2", "mg", "ca")) {
    charge <- 2
  } else if(formula %in% c("h3po4", "al", "fe", "alum", "fecl3", "fe2so43")) {
    charge <- 3
  } else if(!(startunit %in% eqvl_list) & !(endunit %in% eqvl_list)) {
    # This is included so that charge can be in equations later without impacting results
    charge <- 1
  } else {
    stop("Unable to find charge for equivalent conversion")
  }

  # Unit conversion
    # g - mol
  if(startunit %in% gram_list & endunit %in% mole_list) {
    value / molar_weight * multiplier
  } else if(startunit %in% mole_list & endunit %in% gram_list) {
    value * molar_weight * multiplier
    # g - eq
  } else if(startunit %in% eqvl_list & endunit %in% gram_list) {
    value / charge * molar_weight * multiplier
  } else if(startunit %in% gram_list & endunit %in% eqvl_list) {
    value / molar_weight * charge * multiplier
    # mol - eq
  } else if(startunit %in% mole_list & endunit %in% eqvl_list) {
    value * charge * multiplier
  } else if(startunit %in% eqvl_list & endunit %in% mole_list) {
    value / charge * multiplier
    # g CaCO3 - g
  } else if(startunit %in% caco_list & endunit %in% gram_list & !(endunit %in% caco_list)) {
    value / caco3_mw * molar_weight
  } else if(endunit %in% caco_list & startunit %in% gram_list & !(startunit %in% caco_list)) {
    value / molar_weight * caco3_mw
    # same lists
  } else if((startunit %in% gram_list & endunit %in% gram_list) |
            (startunit %in% mole_list & endunit %in% mole_list) |
            (startunit %in% eqvl_list & endunit %in% eqvl_list)) {
    value * multiplier
  } else {
    stop("Units not supported")
  }
  
   
}


# Functions to determine alpha from H+ and disassociation constants
# Not exported
calculate_alpha1 <- function(h, k1, k2) {
  (k1 * h) / (h^2 + k1 * h + k1 * k2)
}

calculate_alpha2 <- function(h, k1, k2) {
  (k1 * k2) / (h^2 + k1 * h + k1 * k2)
}

