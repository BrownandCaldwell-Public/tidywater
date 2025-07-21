#' @title Add an ion to balance overall charge in a water
#'
#' @description This function takes a water defined by [define_water] and balances charge.
#' For a single water use `balance_ions`; for a dataframe use `balance_ions_chain`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#'
#' @details If more cations are needed, sodium will be added. User may specify which cation ("na", "k", "ca", or "mg") to use for balancing.
#' If calcium and magnesium are not specified when defining a water with
#' [define_water], they will default to 0 and not be changed by this function unless specified in the cation argument.
#' Anions are added by default with chloride. User may specify which anion ("cl", "so4") to use for balancing.
#' This function is purely mathematical.
#' User should always check the outputs to make sure values are reasonable for the input source water.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param water Water created with [define_water], which may have some ions set to 0 when unknown
#' @param anion Selected anion to use to for ion balance when more cations are present. Defaults to "cl". Choose one of c("cl", "so4").
#' @param cation Selected cation to use to for ion balance when more anions are present. Defaults to "na". Choose one of c("na", "k", "ca", or "mg").
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10) %>%
#'   balance_ions()
#'
#' water_defined <- define_water(7, 20, 50, tot_hard = 150) %>%
#'   balance_ions(anion = "so4")
#'
#' @export
#'
#' @returns `balance_ions` returns a single water class object with updated ions to balance water charge.
#'
balance_ions <- function(water, anion = "cl", cation = "na") {
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }

  # check for acceptable cations/anions
  if (!(anion %in% c("cl", "so4"))) {
    stop("Anion must be one of 'cl' or 'so4'. No other ions supported at this time.")
  }
  if (!(cation %in% c("na", "k", "ca", "mg"))) {
    stop("Cation must be one of 'na', 'k', 'ca', or 'mg'. No other ions supported at this time.")
  }

  # Set up ions to be changed
  na_new <- water@na
  k_new <- water@k
  ca_new <- water@ca
  mg_new <- water@mg

  cl_new <- water@cl
  so4_new <- water@so4

  # calculate charge
  cations <- sum(water@na, 2 * water@ca, 2 * water@mg, water@k, water@h, na.rm = TRUE)
  anions <- sum(water@cl, 2 * water@so4, water@hco3, 2 * water@co3, water@h2po4, 2 * water@hpo4, 3 * water@po4,
    water@oh, water@ocl,
    na.rm = TRUE
  )

  if (is.na(cations) | is.na(anions)) {
    stop("Missing cations or anions for balance. Make sure pH and alkalinity are specified when define_water is called.")
  }

  # Initialize these objects so they can be used later.
  add_na <- 0
  add_k <- 0
  add_ca <- 0
  add_mg <- 0

  add_cl <- 0
  add_so4 <- 0

  # Add either sodium or potassium if cations are needed
  # Sodium is preferred because it's often present and not measured.
  # Potassium is usually low, but if it's the only cation not measured, it can be added.
  # Added Ca and Mg as options
  if (cations < anions) {
    add_cat <- anions - cations
    if (cation == "na") {
      add_na <- add_cat
      na_new <- ifelse(is.na(water@na), add_na, water@na + add_na)
      water@estimated <- paste(water@estimated, "na", sep = "_")
    } else if (cation == "k") {
      add_k <- add_cat
      k_new <- ifelse(is.na(water@k), add_k, water@k + add_k)
      water@estimated <- paste(water@estimated, "k", sep = "_")
    } else if (cation == "ca") {
      add_ca <- add_cat / 2
      ca_new <- ifelse(is.na(water@ca), add_ca, water@ca + add_ca)
      water@estimated <- paste(water@estimated, "ca", sep = "_")
    } else if (cation == "mg") {
      add_mg <- add_cat / 2
      mg_new <- ifelse(is.na(water@mg), add_mg, water@mg + add_mg)
      water@estimated <- paste(water@estimated, "mg", sep = "_")
    }
    # add chloride or sulfate if anions are needed
    # Similar logic to cations, although sulfate is typically at higher concentrations than potassium.
    # Pretty standard to add Na and Cl because those are just regular salt. It does affect CSMR, but almost nothing else.
  } else if (anions < cations) {
    add_ani <- cations - anions
    if (anion == "cl") {
      add_cl <- add_ani
      cl_new <- ifelse(is.na(water@cl), add_cl, water@cl + add_cl)
      water@estimated <- paste(water@estimated, "cl", sep = "_")
    } else if (anion == "so4") {
      add_so4 <- add_ani / 2
      so4_new <- ifelse(is.na(water@so4), add_so4, water@so4 + add_so4)
      water@estimated <- paste(water@estimated, "so4", sep = "_")
    }
  }

  water@na <- na_new
  water@k <- k_new
  water@ca <- ca_new
  water@mg <- mg_new

  water@cl <- cl_new
  water@so4 <- so4_new

  # Update TDS/cond/IS if needed.
  if (grepl("tds", water@estimated) & grepl("cond", water@estimated)) {
    # Update TDS and cond if they were estimated from IS. Otherwise, assume initial values were measured.
    water@tds <- water@tds + convert_units(add_na, "na", "M", "mg/L") + convert_units(add_k, "k", "M", "mg/L") +
      convert_units(add_ca, "ca", "M", "mg/L") + convert_units(add_mg, "mg", "M", "mg/L") +

      convert_units(add_cl, "cl", "M", "mg/L") + convert_units(add_so4, "so4", "M", "mg/L")

    water@cond <- correlate_ionicstrength(water@tds, from = "tds", to = "cond")
    # Similarly, IS should only update from the ion balance if TDS and cond were estimates.
    water@is <- calculate_ionicstrength(water)
  }

  return(water)
}

#' @rdname balance_ions
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_chain]
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated water classes. Default is "balanced_water".
#'
#' @examples
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain(anion = "so4", cation = "ca")
#'
#' \donttest{
#' # Initialize parallel processing
#' library(furrr)
#' # plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain()
#'
#' # Optional: explicitly close multisession processing
#' # plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#' @returns `balance_ions_chain` returns a dataframe with a new column with the ion balanced water

balance_ions_chain <- function(df, input_water = "defined", output_water = "balanced_water",
                               anion = "cl", cation = "na") {
  validate_water_helpers(df, input_water)
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        anion = anion,
        cation = cation
      ), balance_ions
    ))
}
