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
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
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
#' @source Ryznar (1946)
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
#' ) %>%
#'   calculate_corrosion()
#'
#' water <- define_water(ph = 8, temp = 25, alk = 100, tot_hard = 50, tds = 200) %>%
#'   calculate_corrosion(index = c("aggressive", "ccpp"))
#'
#' @export
#'
#' @returns `calculate_corrosion` returns a single water class object with updated corrosion and scaling index slots.
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
    water@aggressive <- water@ph + log10(water@alk * ca_hard)

    if (is.infinite(water@aggressive)) {
      water@aggressive <- NA_real_
    }
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
    water@csmr <- cl / so4

    if (is.nan(water@csmr) | is.infinite(water@csmr)) {
      water@csmr <- NA_real_
    }
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

    water@larsonskold <- (cl_meq + so4_meq) / (alk_meq)
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
    validate_water(water, c("temp", "alk_eq", "ca", "co3"))
    tempa <- water@temp + 273.15
    pkso <- 171.9065 + 0.077993 * tempa - 2839.319 / tempa - 71.595 * log10(tempa) # calcite
    K_so <- 10^-pkso
    gamma2 <- ifelse(!is.na(water@is), calculate_activity(2, water@is, water@temp), 1)

    solve_x <- function(x, water) {
      water2 <- chemdose_ph(water, caco3 = x)
      K_so / (water2@co3 * gamma2) - water2@ca * gamma2
    }

    # Crazy nesting here to allow broader search without causing errors in the solve_ph uniroot.
    root_x <- tryCatch(
      {
        # First try with a restricted interval
        stats::uniroot(solve_x,
          water = water,
          interval = c(-50, 50)
        )
      },
      error = function(e) {
        tryCatch(
          {
            stats::uniroot(solve_x,
              water = water,
              interval = c(-1, 1),
              extendInt = "downX"
            )
          },
          error = function(e) {
            tryCatch(
              {
                stats::uniroot(solve_x,
                  water = water,
                  interval = c(-1, 1),
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


    water@ccpp <- -root_x$root
  }

  return(water)
}

#' @rdname calculate_corrosion
#' @param df a data frame containing a water class column, created using [define_water]
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#'
#' @examples
#'
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   slice_head(n = 2) %>% # used to make example run faster
#'   define_water_chain() %>%
#'   calculate_corrosion_once(index = c("aggressive", "ccpp"))
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns `calculate_corrosion_once` returns a data frame containing specified corrosion and scaling indices as columns.

calculate_corrosion_once <- function(df, input_water = "defined_water", index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
                                     form = "calcite") {
  corrosion_indices <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    calculate_corrosion_chain(input_water = input_water, index = index, form = form) %>%
    mutate(index = furrr::future_map(corrosion_indices, convert_water)) %>%
    unnest(index) %>%
    select(-corrosion_indices) %>%
    select_if(~ any(!is.na(.)))
}

#' @rdname calculate_corrosion
#' @param output_water name of output column storing updated indices with the class, water. Default is "corrosion_indices".
#'
#' @examples
#' \donttest{
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   calculate_corrosion_chain()
#'
#' # Initialize parallel processing
#' library(furrr)
#' # plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   calculate_corrosion_chain(index = c("aggressive", "ccpp"))
#'
#' # Optional: explicitly close multisession processing
#' # plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns `calculate_corrosion_chain` returns a data frame containing a water class column with updated corrosion and scaling index slots.

calculate_corrosion_chain <- function(df, input_water = "defined_water", output_water = "corrosion_indices",
                                      index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
                                      form = "calcite") {
  if (any(!index %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))) {
    stop("Index must be one or more of c('aggressive', 'ryznar', 'langelier', 'ccpp', 'larsonskold', 'csmr')")
  }

  validate_water_helpers(df, input_water)

  index <- list(index)

  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        index = index,
        form = form
      ),
      calculate_corrosion
    ))
}
