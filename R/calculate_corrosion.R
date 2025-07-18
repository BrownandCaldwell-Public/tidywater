# Corrosion and scaling indices
# This function calculates standard corrosion and scaling indices

#' @title Calculate six corrosion and scaling indices (AI, RI, LSI, LI, CSMR, CCPP)
#'
#' @description This function takes an object created by [define_water] and calculates
#' corrosion and scaling indices. For a single water, use `calculate_corrosion`; to apply the calculations to a
#' dataframe, use `calculate_corrosion_once`.
#'
#' @details Aggressiveness Index (AI), unitless - the corrosive tendency of water and its effect on asbestos cement pipe.
#'
#' Ryznar Index (RI), unitless - a measure of scaling potential.
#'
#' Langelier Saturation Index (LSI), unitless - describes the potential for calcium carbonate scale formation.
#' Equations use empirical calcium carbonate solubilities from Plummer and Busenberg (1982) and Crittenden et al. (2012)
#' rather than calculated from the concentrations of calcium and carbonate in the water.
#'
#' Larson-skold Index (LI), unitless - describes the corrosivity towards mild steel.
#'
#' Chloride-to-sulfate mass ratio (CSMR), mg Cl/mg SO4 - indicator of galvanic corrosion for lead solder pipe joints.
#'
#' Calcium carbonate precipitation potential (CCPP), mg/L as CaCO3 - a prediction of the mass of calcium carbonate that will precipitate at equilibrium.
#' A positive CCPP value indicates the amount of CaCO3 (mg/L as CaCO3) that will precipitate.
#' A negative CCPP indicates how much CaCO3 can be dissolved in the water.
#'
#' @source AWWA (1977)
#' @source Crittenden et al. (2012)
#' @source Langelier (1936)
#' @source Larson and Skold (1958)
#' @source Merrill and Sanks (1977a)
#' @source Merrill and Sanks (1977b)
#' @source Merrill and Sanks (1978)
#' @source Nguyen et al. (2011)
#' @source Plummer and Busenberg (1982)
#' @source Ryznar (1944)
#' @source Schock (1984)
#' @source Trussell (1998)
#' @source U.S. EPA (1980)
#' @source See reference list at \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#'
#' @param water Source water of class "water" created by [define_water]
#' @param index The indices to be calculated.
#'  Default calculates all six indices: "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"
#'  CCPP may not be able to be calculated sometimes, so it may be advantageous to leave this out of the function to avoid errors
#' @param form Form of calcium carbonate mineral to use for modelling solubility: "calcite" (default), "aragonite", or "vaterite"
#'
#' @examples
#' water <- define_water(
#'   ph = 8, temp = 25, alk = 200, tot_hard = 200,
#'   tds = 576, cl = 150, so4 = 200
#' )
#' corrosion_indices <- calculate_corrosion(water)
#'
#' water <- define_water(ph = 8, temp = 25, alk = 100, tot_hard = 50, tds = 200)
#' corrosion_indices <- calculate_corrosion(water, index = c("aggressive", "ccpp"))
#'
#' @export
#'
#' @returns `calculate_corrosion` returns a data frame with corrosion and scaling indices as individual columns.
#'
calculate_corrosion <- function(water, index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"), form = "calcite") {
  if (is.na(water@ca) & ("aggressive" %in% index | "ryznar" %in% index | "langelier" %in% index | "ccpp" %in% index)) {
    warning("Calcium or total hardness not specified. Aggressiveness, Ryznar, Langelier, and CCPP indices will not be calculated.")
  }
  if ((is.na(water@cl) | is.na(water@so4)) & ("larsonskold" %in% index | "csmr" %in% index)) {
    warning("Chloride or sulfate not specified. Larson-Skold index and CSMR will not be calculated.")
  }
  if (any(!index %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))) {
    stop("Index must be one or more of c('aggressive', 'ryznar', 'langelier', 'ccpp', 'larsonskold', 'csmr')")
  }

  # Create the output data frame corrosion_indices
  corrosion_indices <- data.frame(
    aggressive = NA_real_,
    ryznar = NA_real_,
    langelier = NA_real_,
    larsonskold = NA_real_,
    csmr = NA_real_,
    ccpp = NA_real_
  )

  ###########################################################################################*
  # AGGRESSIVE ------------------------------
  ###########################################################################################*
  # AWWA (1977)

  if ("aggressive" %in% index) {
    validate_water(water, c("ca", "ph", "alk"))
    if (grepl("ca", water@estimated)) {
      warning("Calcium estimated by previous tidywater function, aggressiveness index calculation approximate.")
      water@estimated <- paste0(water@estimated, "_aggressive")
    }
    ca_hard <- convert_units(water@ca, "ca", "M", "mg/L CaCO3")
    aggressive <- water@ph + log10(water@alk * ca_hard)

    if (is.infinite(aggressive)) {
      aggressive <- NA_real_
    }

    corrosion_indices$aggressive <- aggressive
  }

  ###########################################################################################*
  # CSMR ------------------------------
  ###########################################################################################*
  # Nguyen et al. (2011)

  if ("csmr" %in% index) {
    validate_water(water, c("cl", "so4"))
    if (grepl("cl", water@estimated) | grepl("so4", water@estimated)) {
      warning("Chloride or sulfate estimated by previous tidywater function, CSMR calculation approximate.")
      water@estimated <- paste0(water@estimated, "_csmr")
    }
    cl <- convert_units(water@cl, "cl", "M", "mg/L")
    so4 <- convert_units(water@so4, "so4", "M", "mg/L")
    cl_sulfate <- cl / so4

    if (is.nan(cl_sulfate) | is.infinite(cl_sulfate)) {
      cl_sulfate <- NA_real_
    }

    corrosion_indices$csmr <- cl_sulfate
  }

  ###########################################################################################*
  # LARSONSKOLD ------------------------------
  ###########################################################################################*
  # Larson and Skold (1958)

  if ("larsonskold" %in% index) {
    validate_water(water, c("cl", "so4", "alk_eq"))
    if (grepl("cl", water@estimated) | grepl("so4", water@estimated)) {
      warning("Chloride or sulfate estimated by previous tidywater function, Larson-Skold index calculation approximate.")
      water@estimated <- paste0(water@estimated, "_csmr")
    }
    # epm = equivalents per million
    # (epm Cl + epm SO4)/ (epm HCO3 + epm CO3)
    cl_meq <- convert_units(water@cl, "cl", "M", "meq/L")
    so4_meq <- convert_units(water@so4, "so4", "M", "meq/L")
    alk_meq <- water@alk_eq * 1000

    larsonskold <- (cl_meq + so4_meq) / (alk_meq)

    corrosion_indices$larsonskold <- larsonskold
  }

  ###########################################################################################*
  # CALCULATE pH OF SATURATION (ph_s) ----
  # Crittenden et al. (2012), equation 22-30
  # Plummer and Busenberg (1982)
  # Schock (1984), equation 9
  # U.S. EPA (1980), equation 4a

  if ("langelier" %in% index | "ryznar" %in% index) {
    validate_water(water, c("temp", "ca", "alk_eq", "hco3", "ph"))
    ks <- correct_k(water)
    pk2co3 <- -log10(ks$k2co3)
    gamma1 <- ifelse(!is.na(water@is), calculate_activity(1, water@is, water@temp), 1)
    gamma2 <- ifelse(!is.na(water@is), calculate_activity(2, water@is, water@temp), 1)
    tempa <- water@temp + 273.15

    # Empirical calcium carbonate solubilities From Plummer and Busenberg (1982)
    if (form == "calcite") {
      pkso <- 171.9065 + 0.077993 * tempa - 2839.319 / tempa - 71.595 * log10(tempa) # calcite
    } else if (form == "aragonite") {
      pkso <- 171.9773 + 0.077993 * tempa - 2903.293 / tempa - 71.595 * log10(tempa) # aragonite
    } else if (form == "vaterite") {
      pkso <- 172.1295 + 0.077993 * tempa - 3074.688 / tempa - 71.595 * log10(tempa) # vaterite
    }

    # pH of saturation
    ph_s <- pk2co3 - pkso - log10(gamma2 * water@ca) - log10(water@alk_eq) # Crittenden et al. (2012), eqn. 22-30

    if (ph_s <= 9.3) {
      ph_s <- ph_s
    } else if (ph_s > 9.3) {
      ph_s <- pk2co3 - pkso - log10(gamma2 * water@ca) - log10(gamma1 * water@hco3) # Use bicarbonate alkalinity only if initial pH_s > 9.3 (U.S. EPA, 1980)
    }

    ###########################################################################################*
    # LANGELIER ------------------------------
    ###########################################################################################*
    # Langelier (1936)

    if ("langelier" %in% index) {
      langelier <- water@ph - ph_s

      if (is.infinite(langelier)) {
        langelier <- NA_real_
      }

      corrosion_indices$langelier <- langelier
    }
  }

  ###########################################################################################*
  # RYZNAR ------------------------------
  ###########################################################################################*
  # Ryznar (1944)

  if ("ryznar" %in% index) {
    ryznar <- 2 * ph_s - water@ph

    if (is.infinite(ryznar)) {
      ryznar <- NA_real_
    }

    corrosion_indices$ryznar <- ryznar
  }

  ###########################################################################################*
  # CCPP ------------------------------
  ###########################################################################################*
  # Merrill and Sanks (1977a)
  # Merrill and Sanks (1977b)
  # Merrill and Sanks (1978)
  # Trussell (1998)

  if ("ccpp" %in% index) {
    validate_water(water, c("temp", "alk_eq", "ca", "co3"))
    tempa <- water@temp + 273.15
    pkso <- 171.9065 + 0.077993 * tempa - 2839.319 / tempa - 71.595 * log10(tempa) # calcite
    K_so <- 10^-pkso
    gamma2 <- ifelse(!is.na(water@is), calculate_activity(2, water@is, water@temp), 1)

    solve_x <- function(x, water) {
      water2 <- suppressWarnings(chemdose_ph(water, caco3 = x))
      K_so / (water2@co3 * gamma2) - water2@ca * gamma2
    }

    # Nesting here to allow broader search without causing errors in the solve_ph uniroot.
    # A programming expert could probably clean this up somewhat
    root_x <- tryCatch(
      {
        # First try with a restricted interval
        # cat("\nFirst Solver\n")
        stats::uniroot(solve_x,
          water = water,
          interval = c(-50, 50)
        )
      },
      error = function(e) {
        tryCatch(
          {
            # cat("Big Scan\n")
            # Initial check for search interval
            x_range <- seq(-500, 500, 10)
            vals <- sapply(x_range, function(x) solve_x(x, water))
            # Find all sign changes
            signs <- sign(vals)
            interval_min <- which(diff(signs) != 0)
            # Smallest difference between values indicates more stability
            best <- which.min(abs(vals[interval_min] - vals[interval_min + 1]))
            lower <- x_range[interval_min[best]]
            upper <- x_range[interval_min[best] + 1]

            # Run uniroot on idenfied interval
            stats::uniroot(solve_x,
              water = water,
              interval = c(lower, upper)
            )
          },
          error = function(e) {
            tryCatch(
              {
                # cat("Extend int down\n")
                stats::uniroot(solve_x,
                  water = water,
                  interval = c(-1300, -100),
                  extendInt = "downX"
                )
              },
              error = function(e) {
                tryCatch(
                  {
                    # cat("Extend int up\n")
                    stats::uniroot(solve_x,
                      water = water,
                      interval = c(-1, 500),
                      extendInt = "upX"
                    )
                  },
                  error = function(e) {
                    stop("Water outside range for CCPP solver.")
                  }
                )
              }
            )
          }
        )
      }
    )


    caco3_precip <- -root_x$root

    corrosion_indices$ccpp <- caco3_precip
  }

  output <- corrosion_indices[, names(corrosion_indices) %in% index, drop = FALSE]

  return(output)
}

#' @rdname calculate_corrosion
#' @param df a data frame containing a water class column, created using [define_water]
#' @param input_water name of the column of water class data to be used as the input. Default is "defined".
#' @param water_prefix append water name to beginning of output columns. Defaults to TRUE
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   calculate_corrosion_chain(index = c("aggressive", "ccpp"))
#'
#' @export
#'
#' @returns `calculate_corrosion_chain` returns the a data frame containing specified corrosion and scaling indices as columns.

calculate_corrosion_chain <- function(df, input_water = "defined", water_prefix = TRUE,
                                     index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
                                     form = "calcite") {

  if (any(!index %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))) {
    stop("Index must be one or more of c('aggressive', 'ryznar', 'langelier', 'ccpp', 'larsonskold', 'csmr')")
  }

  validate_water_helpers(df, input_water)

  indices_df <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    calculate_corrosion(
        water = df[[input_water]][[i]],
        index = index,
        form = form
      )
  }))

  if(water_prefix) {
    names(indices_df) <- paste0(input_water, "_", names(indices_df))
  }

  output <- cbind(df, indices_df)
  return(output)

}
