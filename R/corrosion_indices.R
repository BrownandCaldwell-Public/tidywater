# Corrosion and scaling indices
# This function calculates standard corrosion and scaling indices

#' @title Calculate six corrosion and scaling indices (AI, RI, LSI, LI, CSMR, CCPP)
#'
#' @description \code{calculate_corrosion} takes an object of class "water" created by \code{\link{define_water}} and calculates
#' corrosion and scaling indices.
#'
#' @details Aggressive Index (AI) - the corrosive tendency of water and its effect on asbestos cement pipe
#'
#' Ryznar Index(RI) - a measure of scaling potential
#'
#' Langelier Saturation Index (LSI) - describes the potential for calcium carbonate scale formation. Equations use empirical calcium carbonate solubilities from Plummer and Busenberg (1982) and Crittenden et al. (2012) rather than calculated from the concentrations of calcium and carbonate in the water.
#'
#' Larson-skold Index (LI) - describes the corrosivity towards mild steel
#'
#' Chloride-to-sulfate mass ratio (CSMR) - indicator of galvanic corrosion for lead solder pipe joints
#'
#' Calcium carbonate precipitation potential (CCPP) - a prediction of the mass of calcium carbonate that will precipitate at equilibrium. A positive CCPP value indicates the amount of CaCO3 (mg/L as CaCO3) that will precipitate. A negative CCPP indicates how much CaCO3 can be dissolved in the water.
#'
#' @source AWWA (1977)
#' @source Crittenden et al. (2012)
#' @source Langelier (1936)
#' @source Plummer and Busenberg (1982)
#' @source U.S. EPA (1980)
#' @source Schock (1984)
#' @source Merrill and Sanks (1977a)
#' @source Merrill and Sanks (1977b)
#' @source Merrill and Sanks (1978)
#' @source Trussell (1998)
#' @source Ryznar (1946)
#' @source See reference list at \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param index The indices to be calculated.
#'  Default calculates all six indices: "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"
#'  CCPP may not be able to be calculated sometimes, so it may be advantageous to leave this out of the function to avoid errors
#' @param form Form of calcium carbonate mineral to use for modelling solubility: "calcite" (default), "aragonite", or "vaterite"
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' water <- define_water(ph = 8, temp = 25, alk = 200, ca_hard = 200, tds = 576, cl = 150, so4 = 200) %>%
#'   calculate_corrosion()
#'
#' water <- define_water(ph = 8, temp = 25, alk = 100, ca_hard = 50, tds = 200) %>%
#'   calculate_corrosion(index = c("aggressive", "ccpp"))
#'
#' @export
#'
calculate_corrosion <- function(water, index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"), form = "calcite") {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}
  if (is.na(water@ca) & ("aggressive" %in% index | "ryznar" %in% index | "langelier" %in% index | "ccpp" %in% index)) {
    warning("Calcium or total hardness not specified. Aggressive, Ryznar, Langelier, and CCPP indices will not be calculated.")}
  if ((is.na(water@cl) | is.na(water@so4)) & ("larsonskold" %in% index | "csmr" %in% index)) {
    warning("Chloride or sulfate not specified. Larson-Skold index and CSMR will not be calculated.")}
  if (any(!index %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))) {
    stop("Index must be one or more of c('aggressive', 'ryznar', 'langelier', 'ccpp', 'larsonskold', 'csmr')") }

  ###########################################################################################*
  # AGGRESSIVE ------------------------------
  ###########################################################################################*
  # AWWA (1977)

  if ("aggressive" %in% index) {
    if (grepl("ca", water@estimated)) {
      warning("Calcium estimated by previous tidywater function, aggressive index calcuation approximate.")
      water@estimated <- paste0(water@estimated, "_aggressive")
    }
    ca_hard <- convert_units(water@ca, "ca", "M", "mg/L CaCO3")
    water@aggressive <- water@ph + log10(water@alk * ca_hard)

    if (is.infinite(water@aggressive)) {
      water@aggressive <- NA_real_
    }

  }

  ###########################################################################################*
  # CSMR ------------------------------
  ###########################################################################################*

  if ("csmr" %in% index) {
    if (grepl("cl", water@estimated) | grepl("so4", water@estimated)) {
      warning("Chloride or sulfate estimated by previous tidywater function, CSMR calcuation approximate.")
      water@estimated <- paste0(water@estimated, "_csmr")
    }
    cl <- convert_units(water@cl, "cl", "M", "mg/L")
    so4 <- convert_units(water@so4, "so4", "M", "mg/L")
    water@csmr <- cl / so4

    if (is.nan(water@csmr) | is.infinite(water@csmr)) {
      water@csmr <- NA_real_
    }
  }

  ###########################################################################################*
  # LARSONSKOLD ------------------------------
  ###########################################################################################*

  if ("larsonskold" %in% index) {
    if (grepl("cl", water@estimated) | grepl("so4", water@estimated)) {
      warning("Chloride or sulfate estimated by previous tidywater function, Larson-Skold index calcuation approximate.")
      water@estimated <- paste0(water@estimated, "_csmr")
    }
    # epm = equivalents per million
    # (epm Cl + epm SO4)/ (epm HCO3 + epm CO3)
    cl_meq <- convert_units(water@cl, "cl", "M", "meq/L")
    so4_meq <- convert_units(water@so4, "so4", "M", "meq/L")
    alk_meq <- water@alk_eq * 1000

    water@larsonskold <- (cl_meq + so4_meq) / (alk_meq)
  }

  ###########################################################################################*
  # CALCULATE pH OF SATURATION (ph_s) ----
  # Crittenden et al. (2012), equation 22-30
  # Plummer and Busenberg (1982)
  # Langelier (1936)
  # Schock (1984), equation 9
  # U.S. EPA (1980), equation 4a
  if ("langelier" %in% index | "ryznar" %in% index) {
    ks <- correct_k(water)
    pk2co3 = -log10(ks$k2co3)
    gamma1 <- ifelse(!is.na(water@is), calculate_activity(1, water@is, water@temp), 1)
    gamma2 <- ifelse(!is.na(water@is), calculate_activity(2, water@is, water@temp), 1)
    tempa = water@temp + 273.15

    # Empirical calcium carbonate solubilities From Plummer and Busenberg (1982)
    if (form == "calcite") {
      pkso = 171.9065 + 0.077993 * tempa - 2839.319 / tempa - 71.595 * log10(tempa) # calcite
    } else if (form == "aragonite") {
      pkso = 171.9773 + 0.077993 * tempa - 2903.293 / tempa - 71.595 * log10(tempa) # aragonite
    } else if (form == "vaterite") {
      pkso = 172.1295 + 0.077993 * tempa - 3074.688 / tempa - 71.595 * log10(tempa) # vaterite
    }

    # pH of saturation
    ph_s = pk2co3 - pkso - log10(gamma2 * water@ca) - log10(water@alk_eq) # Crittenden et al. (2012), eqn. 22-30

    if (ph_s <= 9.3) {
      ph_s = ph_s
    } else if (ph_s > 9.3) {
      ph_s = pk2co3 - pkso - log10(gamma2 * water@ca) - log10(gamma1 * water@hco3) # Use bicarbonate alkalinity only if initial pH_s > 9.3 (U.S. EPA, 1980)
    }

    ###########################################################################################*
    # LANGELIER ------------------------------
    ###########################################################################################*

    if ("langelier" %in% index) {
      water@langelier <- water@ph - ph_s

      if (is.infinite(water@langelier)) {
        water@langelier <- NA_real_
      }
    }

  }

  ###########################################################################################*
  # RYZNAR ------------------------------
  ###########################################################################################*
  # Ryznar (1944)

  if ("ryznar" %in% index) {
    water@ryznar <- 2 * ph_s - water@ph

    if (is.infinite(water@ryznar)) {
      water@ryznar <- NA_real_
    }
  }

  ###########################################################################################*
  # CCPP ------------------------------
  ###########################################################################################*
  # Merrill and Sanks (1977a)
  # Merrill and Sanks (1977b)
  # Merrill and Sanks (1978)
  # Trussell (1998)

  if ("ccpp" %in% index) {
    tempa = water@temp + 273.15
    pkso = 171.9065 + 0.077993 * tempa - 2839.319 / tempa - 71.595 * log10(tempa) # calcite
    K_so = 10^-pkso
    gamma2 <- ifelse(!is.na(water@is), calculate_activity(2, water@is, water@temp), 1)

    solve_x <- function(x, water) {
      water2 <- chemdose_ph(water, caco3 = x)
      K_so / (water2@co3 * gamma2) - water2@ca * gamma2
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

#' @title Create summary table of corrosion indices from a water class
#'
#' @description This function takes a water data frame defined by \code{\link{define_water}} and outputs a formatted summary table of
#' corrosion indices calculated by \code{\link{calculate_corrosion}}.
#'
#' @param water Source water vector created by \code{\link{define_water}}.
#'
#' @examples
#' water <- define_water(ph = 8, temp = 25, alk = 200, ca_hard = 200, tds = 576) %>%
#'   calculate_corrosion()
#' summarise_corrosion(water)
#'
#' @export
#'
summarise_corrosion <- function(water) {
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }
  # Compile main WQ parameters to print AI, RI, LSI, LI, CSMR, CCPP)
  params = data.frame(`Aggressive Index` = water@aggressive,
    `Ryznar Stability Index` = water@ryznar,
    `Langelier Saturation Index` = water@langelier,
    `Larson Skold Index` = water@larsonskold,
    `Chloride to Sulfate Mass Ratio` = water@csmr,
    `Calcium carbonate precipitation potential` = water@ccpp)

  params = params %>%
    pivot_longer(c(Aggressive.Index:Calcium.carbonate.precipitation.potential), names_to = "param", values_to = "result") %>%
    mutate(units = c(rep("unitless", 5), "mg/L CaCO3"),
      Recommended = c(">12", "6.5 - 7.0", ">0", "<0.8", "<0.2", "4 - 10"))

  tab = knitr::kable(params,
    format = "simple",
    col.names = c("Index", "Result", "Units", "Recommended"))

  return(tab)
}
