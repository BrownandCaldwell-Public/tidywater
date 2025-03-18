#' @title Add Na, K, Cl, or SO4 to balance overall charge in a water
#'
#' @description This function takes a water defined by [define_water] and balances charge.
#' For a single water use `balance_ions`; for a dataframe where you want to output a water for continued modeling use
#' `balance_ions_chain`; for a dataframe where you want to output water parameters as columns use `balance_ions_once`
#' (note subsequent tidywater modeling functions will only work if `_chain` is used because a `water` is required).
#'
#' @details If more cations are needed, sodium will be added, unless a number for sodium is already provided and potassium is 0, then it will add potassium. Similarly,
#' anions are added using chloride, unless sulfate is 0. If calcium and magnesium are not specified when defining a water with
#' [define_water], they will default to 0 and not be changed by this function.  This function is purely mathematical.
#' User should always check the outputs to make sure values are reasonable for the input source water.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param water Water created with [define_water], which may have some ions set to 0 when unknown
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10) %>%
#'   balance_ions()
#'
#' @export
#'
#' @returns `balance_ions` returns a single water class object with updated ions to balance water charge.
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
  anions <- sum(
    water@cl,
    2 * water@so4,
    water@hco3,
    2 * water@co3,
    water@h2po4,
    2 * water@hpo4,
    3 * water@po4,
    water@oh,
    water@ocl,
    na.rm = TRUE
  )

  if (is.na(cations) | is.na(anions)) {
    stop(
      "Missing cations or anions for balance. Make sure pH and alkalinity are specified when define_water is called."
    )
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
  water@applied_treatment <- paste(water@applied_treatment, "_balanced", sep = "")

  # Update TDS/cond/IS if needed.
  if (grepl("tds", water@estimated) & grepl("cond", water@estimated)) {
    # Update TDS and cond if they were estimated from IS. Otherwise, assume initial values were measured.
    water@tds <- water@tds +
      convert_units(add_na, "na", "M", "mg/L") +
      convert_units(add_k, "k", "M", "mg/L") +
      convert_units(add_cl, "cl", "M", "mg/L") +
      convert_units(add_so4, "so4", "M", "mg/L")
    water@cond <- correlate_ionicstrength(water@tds, from = "tds", to = "cond")
    # Similarly, IS should only update from the ion balance if TDS and cond were estimates.
    water@is <- calculate_ionicstrength(water)
  }

  return(water)
}

#' @rdname balance_ions
#'
#' @param df a data frame containing a water class column, which has already been computed using \code{\link{define_water_chain}}
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#'
#' @examples
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
#'   define_water_chain(output_water = "Different_defined_water_column") %>%
#'   balance_ions_once(input_water = "Different_defined_water_column")
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export
#' @returns `balance_ions_once` returns a dataframe with updated ions to balance water charge

balance_ions_once <- function(df, input_water = "defined_water") {
  balance_df <- balanced_water <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    mutate(balanced_water = furrr::future_pmap(list(water = !!as.name(input_water)), balance_ions)) %>%
    mutate(balance_df = furrr::future_map(balanced_water, convert_water)) %>%
    unnest_wider(balance_df) %>%
    select(-balanced_water)
}


#' @rdname balance_ions
#' @param output_water name of the output column storing updated water classes. Default is "balanced_water".
#'
#' @examples
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 5)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain(output_water = "balanced ions, balanced life") %>%
#'   chemdose_ph_chain(input_water = "balanced ions, balanced life", naoh = 5)
#'
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 5)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export
#' @returns `balance_ions_chain` returns a dataframe with a new column with the ion balanced water

balance_ions_chain <- function(df, input_water = "defined_water", output_water = "balanced_water") {
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(list(water = !!as.name(input_water)), balance_ions))
}
