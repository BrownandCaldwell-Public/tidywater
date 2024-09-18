#' @title Determine blended water quality from multiple waters based on mass balance and acid/base equilibrium
#'
#' @description This function takes a vector of waters defined by \code{\link{define_water}} and a vector of ratios and outputs a new water object with updated ion balance and pH.
#'
#' @param waters Vector of source waters created by \code{\link{define_water}}
#' @param ratios Vector of ratios in the same order as waters. (Blend ratios must sum to 1)
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' water1 <- define_water(7, 20, 50)
#' water2 <- define_water(7.5, 20, 100, tot_nh3 = 2)
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

  # Identify slots that are not NA for blending
  s4todata <- function(water) {
    names <- methods::slotNames(water)
    lt <- lapply(names, function(names) methods::slot(water, names))
    as.list(stats::setNames(lt, names))
  }

  parameters <- s4todata(waters[[1]])
  parameters <- names(parameters[!is.na(parameters)])
  otherparams <- c()
  if (length(waters) > 1) {
    for (i in 2:length(waters)) {
      tempparams <- s4todata(waters[[i]])
      tempparams <- names(tempparams[!is.na(tempparams)])
      otherparams <- c(otherparams, tempparams)
    }
    missingn <- setdiff(parameters, otherparams)
    missing1 <- setdiff(otherparams, parameters)
    if (!purrr::is_empty(missingn) | !purrr::is_empty(missing1)) {
      missing <- paste0(c(missingn, missing1), collapse = ", ")
      warning(paste0(
        "The following parameters are missing in some of the waters and will be set to NA in the blend:\n   ", missing,
        "\nTo fix this, make sure all waters provided have the same parameters specified."
      ))
    }
  }

  not_averaged <- c(
    "ph", "hco3", "co3", "po4", "hpo4", "h2po4", "ocl", "nh4",
    "h", "oh", "kw", "applied_treatment", "estimated"
  )
  parameters <- setdiff(parameters, not_averaged)

  # Initialize empty blended water
  blended_water <- methods::new("water")
  # Loop through all slots that have a number and blend.
  for (param in parameters) {
    for (i in 1:length(waters)) {
      temp_water <- waters[[i]]
      if (!methods::is(temp_water, "water")) {
        stop("All input waters must be of class 'water'. Create a water using define_water.")
      }
      ratio <- ratios[i]

      if (is.na(methods::slot(blended_water, param))) {
        methods::slot(blended_water, param) <- methods::slot(temp_water, param) * ratio
      } else {
        methods::slot(blended_water, param) <- methods::slot(temp_water, param) * ratio + methods::slot(blended_water, param)
      }
    }
  }

  # Track treatments and estimated params
  applied_treatment <- c()
  estimated <- c()

  for (i in 1:length(waters)) {
    # Create character vectors that just add the values from all the waters together
    temp_water <- waters[[i]]
    new_treat <- unlist(strsplit(temp_water@applied_treatment, "_"))
    applied_treatment <- c(applied_treatment, new_treat)
    new_est <- unlist(strsplit(temp_water@estimated, "_"))
    estimated <- c(estimated, new_est)
  }

  # Keep only one of each treatment and estimated and paste back into string for the water.
  blended_water@applied_treatment <- paste(unique(applied_treatment), collapse = "_")
  blended_water@estimated <- paste(unique(estimated), collapse = "_")

  # Calculate new pH, H+ and OH- concentrations
  # Calculate kw from temp
  tempa <- blended_water@temp + 273.15 # absolute temperature (K)
  pkw <- round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1) # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  blended_water@kw <- 10^-pkw

  # so4_dose, po4_dose, na_dose, ca_dose, mg_dose, cl_dose are all 0
  # kw calculated above. tot_po4, tot_co3, tot_ocl, tot_nh3, alk_eq part of mass balance.
  # need po4_i, hpo4_i, h2po4_i, ocl_i, nh4_i. Instead, use the total charge from each water for those ions.
  if (blended_water@tot_po4 > 0 | blended_water@tot_ocl > 0 | blended_water@tot_nh3 > 0) {
    charge_delta <- 0
    for (i in 1:length(waters)) {
      temp_water <- waters[[i]]
      temp_water@nh4 <- ifelse(is.na(temp_water@nh4), 0, temp_water@nh4)
      charge <- temp_water@nh4 - sum(3 * temp_water@po4, 2 * temp_water@hpo4, temp_water@h2po4, temp_water@ocl, na.rm = TRUE)
      charge_weight <- ratios[i] * charge
      charge_delta <- charge_delta + charge_weight
    }
  } else {
    charge_delta <- 0
  }

  # Replace NAs for those ions in the blended_water for pH solving.
  blended_water@po4 <- 0
  blended_water@hpo4 <- 0
  blended_water@h2po4 <- 0
  blended_water@ocl <- 0
  # Replace nh4 with the charge so that it's added to the end during solve pH
  blended_water@nh4 <- charge_delta

  ph <- solve_ph(blended_water)
  h <- 10^-ph
  blended_water@oh <- blended_water@kw / h
  blended_water@h <- h
  blended_water@ph <- ph

  # Correct eq constants
  k <- correct_k(blended_water)

  # Carbonate, phosphate, ocl, and nh4 ions
  alpha1 <- calculate_alpha1_carbonate(h, k) # proportion of total carbonate as HCO3-
  alpha2 <- calculate_alpha2_carbonate(h, k) # proportion of total carbonate as CO32-
  blended_water@hco3 <- blended_water@tot_co3 * alpha1
  blended_water@co3 <- blended_water@tot_co3 * alpha2

  alpha1p <- calculate_alpha1_phosphate(h, k)
  alpha2p <- calculate_alpha2_phosphate(h, k)
  alpha3p <- calculate_alpha3_phosphate(h, k)

  blended_water@h2po4 <- blended_water@tot_po4 * alpha1p
  blended_water@hpo4 <- blended_water@tot_po4 * alpha2p
  blended_water@po4 <- blended_water@tot_po4 * alpha3p

  blended_water@ocl <- blended_water@tot_ocl * calculate_alpha1_hypochlorite(h, k)
  blended_water@nh4 <- blended_water@tot_nh3 * calculate_alpha1_ammonia(h, k)
  blended_water@applied_treatment <- paste(blended_water@applied_treatment, "_blended", sep = "")

  return(blended_water)
}

#' Apply `blend_waters` to a dataframe and output `water` slots as a dataframe
#'
#' This function allows \code{\link{blend_waters}} to be added to a piped data frame.
#' Its output is a data frame with updated ions and pH.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' The `water` class columns to use in the function are specified as function arguments. Ratios may be input
#' as columns with varied ratios (in this case, input column names in the function arguments), OR input as numbers directly.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}
#' @param waters List of column names containing a water class to be blended
#' @param ratios List of column names or vector of blend ratios in the same order as waters. (Blend ratios must sum to 1)
#'
#' @seealso \code{\link{blend_waters}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   mutate(
#'     ratios1 = .4,
#'     ratios2 = .6
#'   ) %>%
#'   blend_waters_once(waters = c("defined_water", "dosed"), ratios = c("ratios1", "ratios2"))
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_once(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_once(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export


blend_waters_once <- function(df, waters, ratios) {
  blend_df <- blended <- NULL # Quiet RCMD check global variable note
  df_subset <- df %>% select(all_of(waters))

  for (row in 1:length(df_subset[[1]])) {
    water_vectors <- c()
    blend_ratios <- c()

    for (cols in 1:length(df_subset)) {
      water_save <- df_subset[[cols]][row]
      water_vectors <- c(water_vectors, water_save)

      if (is.character(ratios)) {
        df_ratio <- df %>% select(all_of(ratios))
        ratio_save <- df_ratio[[cols]][row]
        blend_ratios <- c(blend_ratios, ratio_save)
      } else {
        ratio_save <- ratios[[cols]]
        blend_ratios <- c(blend_ratios, ratio_save)
      }
    }

    suppressWarnings(df$blended[row] <- list(blend_waters(water_vectors, blend_ratios)))
  }

  output <- df %>%
    mutate(blend_df = furrr::future_map(blended, convert_water)) %>%
    unnest_wider(blend_df) %>%
    select(-blended)
}

#' Apply `blend_waters` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{blend_waters}} to be added to a piped data frame.
#' Its output is a `water` class with updated ions and pH.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' The `water` class columns to use in the function are specified as function arguments. Ratios may be input
#' as columns with varied ratios (in this case, input column names in the function arguments), OR input as numbers directly.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already
#' been computed using \code{\link{define_water_chain}},
#' @param waters List of column names containing a water class to be blended
#' @param ratios List of column names or vector of blend ratios in the same order as waters. (Blend ratios must sum to 1)
#' @param output_water name of output column storing updated parameters with the class, water. Default is "blended_water".
#'
#' @seealso \code{\link{blend_waters}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22) %>%
#'   mutate(
#'     ratios1 = .4,
#'     ratios2 = .6
#'   ) %>%
#'   blend_waters_chain(
#'     waters = c("defined_water", "dosed_chem_water"),
#'     ratios = c("ratios1", "ratios2"), output_water = "Blending_after_chemicals"
#'   )
#'
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_chain(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_chain(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export


blend_waters_chain <- function(df, waters, ratios, output_water = "blended_water") {
  output <- df %>%
    rowwise() %>%
    mutate(
      waters = furrr::future_pmap(across(all_of(waters)), list),
      ratios = ifelse(
        is.numeric(ratios),
        list(ratios),
        (list(c_across(all_of(ratios))))
      )
    ) %>%
    ungroup() %>%
    mutate(!!output_water := furrr::future_pmap(list(waters = waters, ratios = ratios), blend_waters)) %>%
    select(-c(waters, ratios))
}
