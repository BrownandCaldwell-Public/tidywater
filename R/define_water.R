#' @title Create a water class object given water quality parameters
#'
#' @description This function takes user-defined water quality parameters and creates an S4 "water" class object that forms the input and output of all tidywater models.
#'
#' @details Carbonate balance is calculated and units are converted to mol/L. Ionic strength is determined from ions, TDS, or conductivity. Missing values are handled by defaulting to 0 or
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
#' @param ca Calcium in mg/L Ca2+
#' @param mg Magnesium in mg/L Mg2+
#' @param na Sodium in mg/L Na+
#' @param k Potassium in mg/L K+
#' @param cl Chloride in mg/L Cl-
#' @param so4 Sulfate in mg/L SO42-
#' @param tot_ocl Chlorine in mg/L as Cl2. Used when a starting water has a chlorine residual.
#' @param tot_po4 Phosphate in mg/L as PO4 3-. Used when a starting water has a phosphate residual.
#' @param tot_nh3 Total ammonia in mg/L as N
#' @param tds Total Dissolved Solids in mg/L (optional if ions are known)
#' @param cond Electrical conductivity in uS/cm (optional if ions are known)
#' @param toc Total organic carbon (TOC) in mg/L
#' @param doc Dissolved organic carbon (DOC) in mg/L
#' @param uv254 UV absorbance at 254 nm (cm-1)
#' @param br Bromide in ug/L Br-
#' @param f Fluoride in mg/L F-
#' @param fe Iron in mg/L Fe3+
#' @param al Aluminum in mg/L Al3+
#' @param mn Manganese in ug/L Mn2+
#'
#' @examples
#' water_missingions <- define_water(ph = 7, temp = 15, alk = 100, tds = 10)
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#'
#' @export
#'
#' @returns A water class object where slots are filled or calculated based on input parameters.

define_water <- function(ph, temp = 25, alk, tot_hard, ca, mg, na, k, cl, so4,
                         tot_ocl = 0, tot_po4 = 0, tot_nh3 = 0, tds, cond,
                         toc, doc, uv254, br, f, fe, al, mn) {
  # Initialize string for tracking which parameters were estimated
  estimated <- ""

  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (missing(ph)) {
    ph <- NA_real_
    warning("Missing value for pH. Carbonate balance will not be calculated.")
  }

  if (missing(alk)) {
    alk <- NA_real_
    warning("Missing value for alkalinity. Carbonate balance will not be calculated.")
  }

  tot_hard <- ifelse(missing(tot_hard), NA_real_, tot_hard)
  ca <- ifelse(missing(ca), NA_real_, ca)
  mg <- ifelse(missing(mg), NA_real_, mg)

  if ((!is.na(tot_hard) & !is.na(ca) & !is.na(mg)) & (tot_hard != 0 & ca != 0 & mg != 0)) {
    check_tot_hard <- abs(tot_hard - calculate_hardness(ca, mg)) / mean(c(tot_hard, calculate_hardness(ca, mg)))
    if (check_tot_hard > 0.10) {
      warning("User entered total hardness is >10% different than calculated hardness.")
    }
  }

  if (!is.na(tot_hard) & is.na(ca) & !is.na(mg)) {
    ca <- convert_units(tot_hard - convert_units(mg, "mg", "mg/L", "mg/L CaCO3"), "ca", "mg/L CaCO3", "mg/L")
    warning("Missing value for calcium. Value estimated from total hardness and magnesium.")
    estimated <- paste(estimated, "ca", sep = "_")
  }

  if (!is.na(tot_hard) & is.na(mg) & !is.na(ca)) {
    mg <- convert_units(tot_hard - convert_units(ca, "ca", "mg/L", "mg/L CaCO3"), "mg", "mg/L CaCO3", "mg/L")
    warning("Missing value for magnesium. Value estimated from total hardness and calcium.")
    estimated <- paste(estimated, "mg", sep = "_")
  }

  if (!is.na(tot_hard) & is.na(mg) & is.na(ca)) {
    ca <- convert_units(tot_hard * 0.65, "ca", "mg/L CaCO3", "mg/L")
    mg <- convert_units(tot_hard * 0.35, "mg", "mg/L CaCO3", "mg/L")
    warning("Missing values for calcium and magnesium but total hardness supplied. Default ratio of 65% Ca2+ and 35% Mg2+ will be used.")
    estimated <- paste(estimated, "ca", sep = "_")
    estimated <- paste(estimated, "mg", sep = "_")
  }

  if (is.na(tot_hard) & !is.na(ca) & is.na(mg)) {
    tot_hard <- calculate_hardness(ca, 0) / .65
    mg <- convert_units(tot_hard - convert_units(ca, "ca", "mg/L", "mg/L CaCO3"), "mg", "mg/L CaCO3", "mg/L")
    warning("Missing values for magnesium and total hardness but calcium supplied. Default ratio of 65% Ca2+ and 35% Mg2+ will be used.")
    estimated <- paste(estimated, "tothard", sep = "_")
    estimated <- paste(estimated, "mg", sep = "_")
  } else if (is.na(tot_hard) & !is.na(ca) & !is.na(mg)) {
    tot_hard <- calculate_hardness(ca, mg)
  }

  tds <- ifelse(missing(tds), NA_real_, tds)
  cond <- ifelse(missing(cond), NA_real_, cond)

  # Convert ion concentration inputs to mol/L and fill missing arguments with NA
  ca <- convert_units(ca, "ca")
  mg <- convert_units(mg, "mg")
  na <- ifelse(missing(na), NA_real_, convert_units(na, "na"))
  k <- ifelse(missing(k), NA_real_, convert_units(k, "k"))
  cl <- ifelse(missing(cl), NA_real_, convert_units(cl, "cl"))
  so4 <- ifelse(missing(so4), NA_real_, convert_units(so4, "so4"))
  tot_po4 <- convert_units(tot_po4, "po4")
  tot_ocl <- convert_units(tot_ocl, "cl2")
  tot_nh3 <- convert_units(tot_nh3, "n")

  br <- ifelse(missing(br), NA_real_, convert_units(br, "br", "ug/L", "M"))
  f <- ifelse(missing(f), NA_real_, convert_units(f, "f"))
  fe <- ifelse(missing(fe), NA_real_, convert_units(fe, "fe"))
  al <- ifelse(missing(al), NA_real_, convert_units(al, "al"))
  mn <- ifelse(missing(mn), NA_real_, convert_units(mn, "mn", "ug/L", "M"))

  if (missing(toc) & missing(doc) & missing(uv254)) {
    toc <- NA_real_
    doc <- NA_real_
    uv254 <- NA_real_
  } else if (missing(toc) & missing(doc)) {
    toc <- NA_real_
    doc <- NA_real_
  } else if (missing(toc) & !missing(doc)) {
    warning("Missing value for TOC. DOC assumed to be 95% of TOC.")
    toc <- doc / 0.95
    estimated <- paste(estimated, "toc", sep = "_")
  } else if (missing(doc) & !missing(toc)) {
    warning("Missing value for DOC. Default value of 95% of TOC will be used.")
    doc <- toc * 0.95
    estimated <- paste(estimated, "doc", sep = "_")
  }

  uv254 <- ifelse(missing(uv254), NA_real_, uv254)

  # Calculate temperature dependent constants
  tempa <- temp + 273.15 # absolute temperature (K)
  # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  pkw <- round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1)
  kw <- 10^-pkw

  h <- 10^-ph
  oh <- kw / h

  # convert alkalinity input to equivalents/L
  carb_alk_eq <- convert_units(alk, "caco3", startunit = "mg/L CaCO3", endunit = "eq/L")
  # calculate total carbonate concentration
  # Initial alpha values (not corrected for IS)
  discons <- tidywater::discons
  k1co3 <- K_temp_adjust(discons["k1co3", ]$deltah, discons["k1co3", ]$k, temp)
  k2co3 <- K_temp_adjust(discons["k2co3", ]$deltah, discons["k2co3", ]$k, temp)

  alpha1 <- calculate_alpha1_carbonate(h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as HCO3-
  alpha2 <- calculate_alpha2_carbonate(h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as CO32-
  tot_co3 <- (carb_alk_eq + h - oh) / (alpha1 + 2 * alpha2)

  # Initialize water to simplify IS calcs
  water <- methods::new("water",
    ph = ph, temp = temp, alk = alk, tds = tds, cond = cond, tot_hard = tot_hard,
    na = na, ca = ca, mg = mg, k = k, cl = cl, so4 = so4,
    hco3 = tot_co3 * alpha1, co3 = tot_co3 * alpha2, h2po4 = 0, hpo4 = 0, po4 = 0, ocl = 0, nh4 = 0,
    h = h, oh = oh,
    tot_po4 = tot_po4, tot_ocl = tot_ocl, tot_nh3 = tot_nh3, tot_co3 = tot_co3,
    kw = kw, is = 0, alk_eq = carb_alk_eq,
    doc = doc, toc = toc, uv254 = uv254,
    br = br, f = f, fe = fe, al = al, mn = mn
  )

  # Determine ionic strength

  if (!is.na(tds)) {
    water@is <- correlate_ionicstrength(tds, from = "tds")
    water@cond <- correlate_ionicstrength(tds, from = "tds", to = "cond")
    estimated <- paste(estimated, "cond", sep = "_")
  } else if (!is.na(cond)) {
    water@is <- correlate_ionicstrength(cond, from = "cond")
    water@tds <- correlate_ionicstrength(cond, from = "cond", to = "tds")
    estimated <- paste(estimated, "tds", sep = "_")
  } else if (is.na(tds) & is.na(cond) & ((!is.na(ca) | !is.na(na)) & (!is.na(cl) | !is.na(so4)) & alk > 0) & !is.na(ph)) {
    water@is <- calculate_ionicstrength(water)
    water@tds <- correlate_ionicstrength(water@is, from = "is", to = "tds")
    estimated <- paste(estimated, "tds", sep = "_")
    water@cond <- correlate_ionicstrength(water@is, from = "is", to = "cond")
    estimated <- paste(estimated, "cond", sep = "_")
  } else {
    warning("Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.")
    water@is <- NA_real_
  }

  # Eq constants
  ks <- correct_k(water)

  # Carbonate and phosphate ions and ocl ions
  alpha1 <- calculate_alpha1_carbonate(h, ks) # proportion of total carbonate as HCO3-
  alpha2 <- calculate_alpha2_carbonate(h, ks) # proportion of total carbonate as CO32-
  water@tot_co3 <- (carb_alk_eq + h - oh) / (alpha1 + 2 * alpha2)
  water@hco3 <- water@tot_co3 * alpha1
  water@co3 <- water@tot_co3 * alpha2

  alpha1p <- calculate_alpha1_phosphate(h, ks)
  alpha2p <- calculate_alpha2_phosphate(h, ks)
  alpha3p <- calculate_alpha3_phosphate(h, ks)

  water@h2po4 <- tot_po4 * alpha1p
  water@hpo4 <- tot_po4 * alpha2p
  water@po4 <- tot_po4 * alpha3p

  water@ocl <- tot_ocl * calculate_alpha1_hypochlorite(h, ks)
  water@nh4 <- tot_nh3 * calculate_alpha1_ammonia(h, ks)

  # Calculate total alkalinity (set equal to carbonate alkalinity for now)
  water@alk_eq <- carb_alk_eq

  # Add all estimated values to water slot
  water@estimated <- estimated

  return(water)
}

#' Apply `define_water` and output a dataframe
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame.
#' It outputs all carbonate calculations and other parameters in a data frame.
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>% define_water_once()
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>% define_water_once()
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export
#' @returns A data frame containing columns that were filled or calculated based on define_water.

define_water_once <- function(df) {
  defined_df <- defined_water <- NULL # Quiet RCMD check global variable note
  df %>%
    define_water_chain() %>%
    mutate(defined_df = furrr::future_map(defined_water, convert_water)) %>%
    unnest_wider(defined_df) %>%
    select(-defined_water) %>%
    as.data.frame()
}

#' Apply `define_water` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be chained with "downstream" tidywater functions.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "defined_water".
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_once()
#'
#' example_df <- water_df %>%
#'   define_water_chain(output_water = "This is a column of water") %>%
#'   balance_ions_once(input_water = "This is a column of water")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_once()
#'
#' #' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export
#' @returns A data frame containing a water class column.

define_water_chain <- function(df, output_water = "defined_water") {
  define_water_args <- c(
    "ph", "temp", "alk", "tot_hard", "ca", "mg", "na", "k", "cl", "so4", "tot_ocl", "tot_po4", "tot_nh4",
    "tds", "cond",
    "toc", "doc", "uv254", "br", "f", "fe", "al", "mn"
  )

  extras <- df %>%
    select(!any_of(define_water_args))

  output <- df %>%
    select(any_of(define_water_args)) %>%
    mutate(!!output_water := furrr::future_pmap(., define_water)) %>%
    select(!any_of(define_water_args)) %>%
    cbind(extras)
}
