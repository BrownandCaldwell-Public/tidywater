# General functions
# These functions include formatting and general helper functions

#' @title Create summary table from water class
#'
#' @description This function takes a water data frame defined by \code{\link{define_water}} and outputs a formatted summary table of
#' specified water quality parameters.
#'
#' \code{summarise_wq()} and \code{summarize_wq()} are synonyms.
#'
#' @details Use \code{\link{calculate_corrosion}} for corrosivity indicators and \code{\link{chemdose_dbp}} for modeled DBP concentrations.
#'
#' @param water Source water vector created by \code{\link{define_water}}.
#' @param params List of water quality parameters to be summarized. Options include "general", "ions", "corrosion", and "dbps". Defaults to "general" only.
#'
#' @examples
#' # Summarize general parameters
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#' summarize_wq(water_defined)
#'
#' # Summarize major cations and anions
#' summarize_wq(water_defined, params = list("ions"))
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @export
#' @returns A knitr_kable table of specified water quality parameters.
#'
summarize_wq <- function(water, params = c("general")) {
  pH <- TOC <- Na <- CO3 <- result <- NULL # Quiet RCMD check global variable note
  if (!methods::is(water, "water")) {
    stop("Input must be of class 'water'. Create a water using define_water.")
  }
  if (any(!params %in% c("general", "ions", "corrosion", "dbps"))) {
    stop("params must be one or more of c('general', 'ions', 'corrosion', 'dbps')")
  }

  # Compile general WQ parameters
  general <- data.frame(
    pH = water@ph,
    Temp = water@temp,
    Alkalinity = water@alk,
    Total_Hardness = calculate_hardness(water@ca, water@mg, startunit = "M"),
    TDS = water@tds,
    Conductivity = water@cond,
    TOC = water@toc
  )

  general <- general %>%
    pivot_longer(c(pH:TOC), names_to = "param", values_to = "result") %>%
    mutate(units = c(
      "-", "deg C", "mg/L as CaCO3", "mg/L as CaCO3",
      "mg/L", "uS/cm", "mg/L"
    ))

  gen_tab <- knitr::kable(general,
                          format = "simple",
                          col.names = c("General water quality parameters", "Result", "Units")
  )

  # Compile major ions
  ions <- data.frame(
    Na = convert_units(water@na, "na", "M", "mg/L"),
    Ca = convert_units(water@ca, "ca", "M", "mg/L"),
    Mg = convert_units(water@mg, "mg", "M", "mg/L"),
    K = convert_units(water@k, "k", "M", "mg/L"),
    Cl = convert_units(water@cl, "cl", "M", "mg/L"),
    SO4 = convert_units(water@so4, "so4", "M", "mg/L"),
    HCO3 = convert_units(water@hco3, "hco3", "M", "mg/L"),
    CO3 = convert_units(water@co3, "co3", "M", "mg/L")
  )

  ions <- ions %>%
    pivot_longer(c(Na:CO3), names_to = "ion", values_to = "c_mg")

  ions_tab <- knitr::kable(ions,
                           format = "simple",
                           col.names = c("Major ions", "Concentration (mg/L)"),
                           # format.args = list(scientific = TRUE),
                           digits = 2
  )

  # Compile corrosion indices
  corrosion <- data.frame(
    `Aggressive Index` = water@aggressive,
    `Ryznar Stability Index` = water@ryznar,
    `Langelier Saturation Index (LSI)` = water@langelier,
    `Larson Skold Index` = water@larsonskold,
    `Chloride to sulfate mass ratio (CSMR)` = water@csmr,
    `Calcium carbonate precipitation potential (CCPP)` = water@ccpp
  )

  corrosion <- corrosion %>%
    pivot_longer(everything(), names_to = "param", values_to = "result") %>%
    mutate(result = round(result, 2)) %>%
    mutate(
      units = c(rep("unitless", 5), "mg/L CaCO3"),
      Recommended = c(">12", "6.5 - 7.0", ">0", "<0.8", "<0.2", "4 - 10")
    )

  corr_tab <- knitr::kable(corrosion,
                           format = "simple",
                           col.names = c("Corrosion Indices", "Result", "Units", "Recommended")
  )

  # Compile DBPs
  tthm <- data.frame(
    Chloroform = ifelse(length(water@chcl3) == 0, NA, water@chcl3),
    Bromodichloromethane = ifelse(length(water@chcl2br) == 0, NA, water@chcl2br),
    Dibromochloromethane = ifelse(length(water@chbr2cl) == 0, NA, water@chbr2cl),
    Bromoform = ifelse(length(water@chbr3) == 0, NA, water@chbr3),
    `Total trihalomethanes` = ifelse(length(water@tthm) == 0, NA, water@tthm)
  )


  haa5 <- data.frame(
    `Chloroacetic acid` = ifelse(length(water@mcaa) == 0, NA, water@mcaa),
    `Dichloroacetic acid` = ifelse(length(water@dcaa) == 0, NA, water@dcaa),
    `Trichloroacetic acid` = ifelse(length(water@tcaa) == 0, NA, water@tcaa),
    `Bromoacetic acid` = ifelse(length(water@mbaa) == 0, NA, water@mbaa),
    `Dibromoacetic acid` = ifelse(length(water@dbaa) == 0, NA, water@dbaa),
    `Sum 5 haloacetic acids` = ifelse(length(water@haa5) == 0, NA, water@haa5)
  )
  # Bromochloroacetic_acid = ifelse(length(water@bcaa)==0, NA, water@bcaa),
  # Sum_6_haloacetic_acids = ifelse(length(water@haa6)==0, NA, water@haa6),
  # Chlorodibromoacetic_acid = ifelse(length(water@cdbaa)==0, NA, water@cdbaa),
  # Dichlorobromoacetic_acid = ifelse(length(water@dcbaa)==0, NA, water@dcbaa),
  # Tribromoacetic_acid = ifelse(length(water@tbaa)==0, NA, water@tbaa),
  # Sum_9_haloacetic_acids = ifelse(length(water@haa9)==0, NA, water@haa9))

  tthm <- tthm %>%
    pivot_longer(everything(), names_to = "param", values_to = "result") %>%
    mutate(result = round(result, 2))

  haa5 <- haa5 %>%
    pivot_longer(everything(), names_to = "param", values_to = "result") %>%
    mutate(result = round(result, 2))

  thm_tab <- knitr::kable(tthm,
                          format = "simple",
                          col.names = c("THMs", "Modeled concentration (ug/L)")
  )

  haa_tab <- knitr::kable(haa5,
                          format = "simple",
                          col.names = c("HAAs", "Modeled concentration (ug/L)")
  )

  # Print tables
  tables_list <- list()
  if ("general" %in% params) {
    tables_list[[length(tables_list) + 1]] <- gen_tab
  }
  if ("ions" %in% params) {
    tables_list[[length(tables_list) + 1]] <- ions_tab
  }
  if ("corrosion" %in% params) {
    tables_list[[length(tables_list) + 1]] <- corr_tab
  }
  if ("dbps" %in% params) {
    tables_list[[length(tables_list) + 1]] <- thm_tab
    tables_list[[length(tables_list) + 1]] <- haa_tab
  }

  return(knitr::kables(tables_list))
}

#' @rdname summarize_wq
#' @export
summarise_wq <- summarize_wq

#' Create summary plot of ions from water class
#'
#' This function takes a water data frame defined by \code{\link{define_water}} and outputs an ion balance plot.
#'
#' @param water Source water vector created by link function here
#' @import ggplot2
#'
#' @examples
#' water<- define_water(7, 20, 50, 100, 20, 10, 10, 10, 10, tot_po4 = 1)
#' plot_ions(water)
#'
#' @export
#'
#' @returns A ggplot object displaying the water's ion balance.
#'
plot_ions <- function(water) {
  type <- concentration <- label_pos <- ion <- label_y <- label <- repel_label <- Na <- OH <- NULL # Quiet RCMD check global variable note
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }

  # Compile major ions to plot
  ions <- data.frame(
    Na = water@na,
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
    NH4 = water@nh4,
    H = water@h,
    OH = water@oh
  )

  plot <- ions %>%
    tidyr::pivot_longer(c(Na:OH), names_to = "ion", values_to = "concentration") %>%
    dplyr::mutate(type = case_when(ion %in% c("Na", "Ca", "Mg", "K", "NH4", "H") ~ "Cations", TRUE ~ "Anions"),
                  ion = factor(ion, levels = c("Ca", "Mg", "Na", "K", "NH4", "H",
                                               "HCO3", "CO3", "SO4", "Cl", "H2PO4", "HPO4", "PO4", "OCl", "OH")),
                  concentration = case_when(is.na(concentration) ~ 0, TRUE ~ concentration)) %>%
    dplyr::arrange(ion) %>%
    dplyr::mutate(label_pos = cumsum(concentration) - concentration / 2, .by = type,
                  label_y = case_when(type == "Cations" ~ 2 - .2, TRUE ~ 1 - .2)
    ) %>%
    dplyr::filter(!is.na(concentration),
                  concentration > 0) %>%
    dplyr::mutate(label = case_when(concentration > 10e-5 ~ ion, TRUE ~ ""),
                  repel_label = case_when(concentration <= 10e-5 & concentration > 10e-7 ~ ion, TRUE ~ "")) %>%
    dplyr::mutate(ion = forcats::fct_rev(ion))

  plot %>%
    ggplot(aes(x = concentration, y = type, fill = ion)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5, color = "black") +
    geom_text(aes(label = label, fontface = "bold", angle = 90),
              size = 3.5, position = position_stack(vjust = 0.5)) +
    ggrepel::geom_text_repel(
      aes(
        x = label_pos, y = label_y,
        label = repel_label,
        fontface = "bold"
      ),
      size = 3.5,
      nudge_y = -.2,
      seed = 555) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          legend.position = "none") +
    labs(x = "Concentration (eq/L)", y = "Major Cations and Anions",
         subtitle = paste0("pH=", water@ph, "\nAlkalinity=", water@alk))
}

#' @title Calculate unit conversions for common compounds
#'
#' @description This function takes a value and converts units based on compound name.
#'
#' @param value Value to be converted
#' @param formula Chemical formula of compound. Accepts compounds in mweights for conversions between g and mol or eq
#' @param startunit Units of current value, currently accepts g/L; g/L CaCO3; g/L N; M; eq/L;
#' and the same units with "m", "u", "n" prefixes
#' @param endunit Desired units, currently accepts same as start units
#'
#' @examples
#' convert_units(50, "ca") # converts from mg/L to M by default
#' convert_units(50, "ca", "mg/L", "mg/L CaCO3")
#' convert_units(50, "ca", startunit = "mg/L", endunit = "eq/L")
#'
#' @export
#'
#' @returns A numeric value for the converted parameter.
#'
convert_units <- function(value, formula, startunit = "mg/L", endunit = "M") {
  milli_list <- c("mg/L", "mg/L CaCO3", "mg/L N", "mM", "meq/L")
  mcro_list <- c("ug/L", "ug/L CaCO3", "ug/L N", "uM", "ueq/L")
  nano_list <- c("ng/L", "ng/L CaCO3", "ng/L N", "nM", "neq/L")
  stand_list <- c("g/L", "g/L CaCO3", "g/L N", "M", "eq/L")

  gram_list <- c(
    "ng/L", "ug/L", "mg/L", "g/L",
    "ng/L CaCO3", "ug/L CaCO3", "mg/L CaCO3", "g/L CaCO3",
    "ng/L N", "ug/L N", "mg/L N", "g/L N"
  )
  mole_list <- c("M", "mM", "uM", "nM")
  eqvl_list <- c("neq/L", "ueq/L", "meq/L", "eq/L")

  caco_list <- c("mg/L CaCO3", "g/L CaCO3", "ug/L CaCO3", "ng/L CaCO3")
  n_list <- c("mg/L N", "g/L N", "ug/L N", "ng/L N")

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

  # Need molar mass of CaCO3 and N
  caco3_mw <- as.numeric(tidywater::mweights["caco3"])
  n_mw <- as.numeric(tidywater::mweights["n"])

  # Determine relevant molar weight
  if (formula %in% colnames(tidywater::mweights)) {
    if ((startunit %in% caco_list & endunit %in% c(mole_list, eqvl_list)) |
        (endunit %in% caco_list & startunit %in% c(mole_list, eqvl_list))) {
      molar_weight <- caco3_mw
    } else if ((startunit %in% n_list & endunit %in% c(mole_list, eqvl_list)) |
               (endunit %in% n_list & startunit %in% c(mole_list, eqvl_list))) {
      molar_weight <- n_mw
    } else {
      molar_weight <- as.numeric(tidywater::mweights[formula])
    }
  } else if (!(startunit %in% gram_list) & !(endunit %in% gram_list)) {
    molar_weight <- 0
  } else {
    stop(paste("Chemical formula", formula, "not supported"))
  }

  # Determine charge for equivalents
  if (formula %in% c("na", "k", "cl", "hcl", "naoh", "nahco3", "na", "nh4", "nh3", "f", "br", "bro3")) {
    charge <- 1
  } else if (formula %in% c("so4", "caco3", "h2so4", "na2co3", "caoh2", "mgoh2", "mg", "ca", "pb", "cacl2", "mn")) {
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
    # g N - g
  } else if (startunit %in% n_list & endunit %in% gram_list & !(endunit %in% n_list)) {
    value / n_mw * molar_weight
  } else if (endunit %in% n_list & startunit %in% gram_list & !(startunit %in% n_list)) {
    value / molar_weight * n_mw
    # same lists
  } else if ((startunit %in% gram_list & endunit %in% gram_list) |
             (startunit %in% mole_list & endunit %in% mole_list) |
             (startunit %in% eqvl_list & endunit %in% eqvl_list)) {
    value * multiplier
  } else {
    stop("Units not supported")
  }
}


#' @title Calculate hardness from calcium and magnesium
#'
#' @description This function takes Ca and Mg in mg/L and returns hardness in mg/L as CaCO3
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
#' @returns A numeric value for the total hardness in mg/L as CaCO3.
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

#' Calculate dissolved inorganic carbon (DIC) from total carbonate
#'
#' This function takes a water class object defined by \code{\link{define_water}}
#' and outputs a DIC (mg/L).
#'
#' @param water a water class object containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' example_dic <- define_water(8, 15, 200) %>%
#'   calculate_dic()
#'
#' @export
#' @returns A numeric value for the calculated DIC.
#'

calculate_dic <- function(water) {
  dic <- water@tot_co3 * tidywater::mweights$dic * 1000

  return(dic)
}

# Non-exported functions -----

validate_water <- function(water, slots) {
  # Make sure a water is present.
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }

  # Check if any slots are NA
  if (any(sapply(slots, function(sl) is.na(methods::slot(water, sl))))) {
    # Paste all missing slots together.
    missing <- gsub(" +", ", ", trimws(paste(
      sapply(slots, function(sl) ifelse(is.na(methods::slot(water, sl)), sl, "")),
      collapse = " "
    )))

    stop("Water is missing the following modeling parameter(s): ", missing, ". Specify in 'define_water'.")
  }
}

validate_water_helpers <- function(df, input_water) {
  # Make sure input_water column is in the dataframe and is a water class.

  if (!(input_water %in% colnames(df))) {
    stop("Specified input_water column not found. Check spelling or create a water class column using define_water_chain().")
  }
  if (!all(sapply(df[[input_water]], function(x) methods::is(x, "water")))) {
    stop("Specified input_water does not contain water class objects. Use define_water_chain() or specify a different column.")
  }
}

validate_args <- function(num_args = list(), str_args = list(), log_args = list(), misc_args = list()) {
  all_args <- c(num_args, str_args, log_args, misc_args)
  for (arg in names(all_args)) {
    if (is.null(all_args[[arg]])) {
      stop("argument '", arg, "' is missing, with no default")
    }
  }
  for (arg in names(num_args)) {
    if (!is.numeric(num_args[[arg]])) {
      stop("argument '", arg, "' must be numeric.")
    }
  }
  for (arg in names(str_args)) {
    if (!is.character(str_args[[arg]])) {
      stop("argument '", arg, "' must be specified as a string.")
    }
  }
  for (arg in names(log_args)) {
    if (!is.logical(log_args[[arg]])) {
      stop("argument '", arg, "' must be either TRUE or FALSE.")
    }
  }
}

construct_helper <- function(df, all_args) {
  # Get the names of each argument type
  all_arguments <- names(all_args)
  from_df <- names(all_args[all_args == "use_col"])

  from_new <- all_args[all_args != "use_col"]
  if (length(from_new) > 0) {
    from_columns <- from_new[sapply(from_new, function(x) any(inherits(x, "quosure")))]
  } else {
    from_columns <- list()
  }

  from_inputs <- setdiff(from_new, from_columns)

  inputs_arg <- do.call(expand.grid, list(from_inputs, stringsAsFactors = FALSE))


  if (any(colnames(df) %in% colnames(inputs_arg))) {
    stop("Argument was applied as a function argument, but the column already exists in the data frame. Remove argument or rename dataframe column.")
  }

  # Get the new names for relevant columns
  final_names <- stats::setNames(as.list(all_arguments), all_arguments)
  for (arg in names(from_columns)) {
    final_names[[arg]] <- rlang::as_name(from_columns[[arg]])
  }

  return(list(
    "new_cols" = as.list(inputs_arg),
    "final_names" = as.list(final_names)
  ))
}


# View reference list at https://github.com/BrownandCaldwell/tidywater/wiki/References

# Functions to determine alpha from H+ and dissociation constants for carbonate
calculate_alpha1_carbonate <- function(h, k) {
  k1 <- k$k1co3
  k2 <- k$k2co3
  (k1 * h) / (h^2 + k1 * h + k1 * k2)
}

calculate_alpha2_carbonate <- function(h, k) {
  k1 <- k$k1co3
  k2 <- k$k2co3
  (k1 * k2) / (h^2 + k1 * h + k1 * k2)
}

# Equations from Benjamin (2014) Table 5.3b
calculate_alpha0_phosphate <- function(h, k) {
  k1 <- k$k1po4
  k2 <- k$k2po4
  k3 <- k$k3po4
  1 / (1 + (k1 / h) + (k1 * k2 / h^2) + (k1 * k2 * k3 / h^3))
}

calculate_alpha1_phosphate <- function(h, k) { # H2PO4
  k1 <- k$k1po4
  k2 <- k$k2po4
  k3 <- k$k3po4
  calculate_alpha0_phosphate(h, k) * k1 / h
}

calculate_alpha2_phosphate <- function(h, k) { # HPO4
  k1 <- k$k1po4
  k2 <- k$k2po4
  k3 <- k$k3po4
  calculate_alpha0_phosphate(h, k) * (k1 * k2 / h^2)
}

calculate_alpha3_phosphate <- function(h, k) { # PO4
  k1 <- k$k1po4
  k2 <- k$k2po4
  k3 <- k$k3po4
  calculate_alpha0_phosphate(h, k) * (k1 * k2 * k3 / h^3)
}

calculate_alpha1_hypochlorite <- function(h, k) { # OCl-
  k1 <- k$kocl
  1 / (1 + h / k1) # calculating how much is in the deprotonated form with -1 charge
}

calculate_alpha1_ammonia <- function(h, k) { # NH4+
  k1 <- k$knh4
  1 / (1 + k1 / h) # calculating how much is in the protonated form with +1 charge
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
  0.5 * (sum(water@na, water@cl, water@k, water@hco3, water@h2po4, water@h, water@oh, water@ocl,
             water@f, water@br, water@bro3, water@nh4,
             na.rm = TRUE
  ) * 1^2 +
    sum(water@ca, water@mg, water@so4, water@co3, water@hpo4, water@mn, na.rm = TRUE) * 2^2 +
    sum(water@po4, water@fe, water@al, na.rm = TRUE) * 3^2)
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
  if (!is.na(is)) {
    tempa <- temp + 273.15 # absolute temperature (K)

    # dielectric constant (relative permittivity) based on temperature from Harned and Owen (1958), Crittenden et al. (2012) equation 5-45
    de <- 78.54 * (1 - (0.004579 * (tempa - 298)) + 11.9E-6 * (tempa - 298)^2 + 28E-9 * (tempa - 298)^3)

    # constant for use in calculating activity coefficients from Stumm and Morgan (1996), Trussell (1998), Crittenden et al. (2012) equation 5-44
    a <- 1.29E6 * (sqrt(2) / ((de * tempa)^1.5))

    # Davies equation, Davies (1967), Crittenden et al. (2012) equation 5-43
    activity <- 10^(-a * z^2 * ((is^0.5 / (1 + is^0.5)) - 0.3 * is))
  } else {
    activity <- 1
  }
  return(activity)
}


# Correct acid dissociation constants for temperature and ionic strength
# Dissociation constants corrected for non-ideal solutions following Benjamin (2010) example 3.14.
# See k_temp_adjust for temperature correction equation.
correct_k <- function(water) {
  # Determine activity coefficients
  if (is.na(water@is)) {
    activity_z1 <- 1
    activity_z2 <- 1
    activity_z3 <- 1
  } else {
    activity_z1 <- calculate_activity(1, water@is, water@temp)
    activity_z2 <- calculate_activity(2, water@is, water@temp)
    activity_z3 <- calculate_activity(3, water@is, water@temp)
  }

  temp <- water@temp
  discons <- tidywater::discons
  # Eq constants
  # k1co3 = {h+}{hco3-}/{h2co3}
  k1co3 <- K_temp_adjust(discons["k1co3", ]$deltah, discons["k1co3", ]$k, temp) / activity_z1^2
  # k2co3 = {h+}{co32-}/{hco3-}
  k2co3 <- K_temp_adjust(discons["k2co3", ]$deltah, discons["k2co3", ]$k, temp) / activity_z2
  # kso4 = {h+}{so42-}/{hso4-} Only one relevant dissociation for sulfuric acid in natural waters.
  kso4 <- K_temp_adjust(discons["kso4", ]$deltah, discons["kso4", ]$k, temp) / activity_z2
  # k1po4 = {h+}{h2po4-}/{h3po4}
  k1po4 <- K_temp_adjust(discons["k1po4", ]$deltah, discons["k1po4", ]$k, temp) / activity_z1^2
  # k2po4 = {h+}{hpo42-}/{h2po4-}
  k2po4 <- K_temp_adjust(discons["k2po4", ]$deltah, discons["k2po4", ]$k, temp) / activity_z2
  # k3po4 = {h+}{po43-}/{hpo42-}
  k3po4 <- K_temp_adjust(discons["k3po4", ]$deltah, discons["k3po4", ]$k, temp) * activity_z2 / (activity_z1 * activity_z3)
  # kocl = {h+}{ocl-}/{hocl}
  kocl <- K_temp_adjust(discons["kocl", ]$deltah, discons["kocl", ]$k, temp) / activity_z1^2
  # knh4 = {h+}{nh3}/{nh4+}
  knh4 <- K_temp_adjust(discons["knh4", ]$deltah, discons["knh4", ]$k, temp) / activity_z1^2

  return(data.frame(
    "k1co3" = k1co3, "k2co3" = k2co3,
    "k1po4" = k1po4, "k2po4" = k2po4, "k3po4" = k3po4,
    "kocl" = kocl, "knh4" = knh4, "kso4" = kso4
  ))
}


# SUVA calc
calc_suva <- function(doc, uv254) {
  uv254 / doc * 100
}
