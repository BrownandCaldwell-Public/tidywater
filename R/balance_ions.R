#' @title Add an ion to balance overall charge in a water
#'
#' @description This function takes a water defined by \code{\link{define_water}} and balances charge.
#'
#' @details If more cations are needed, a default of sodium will be added. User may specify which cation ("na", "k", "ca", or "mg") to use for balancing.
#' If calcium and magnesium are not specified when defining a water with
#' \code{\link{define_water}}, they will default to 0 and not be changed by this function unless specified in the cation argument.
#' Anions are added by default with chloride. User may specify which anion ("cl", "so4") to use for balancing. This function is purely mathematical.
#' User should always check the outputs to make sure values are reasonable for the input source water.
#'
#' @param water Water created with define_water, which may have some ions set to 0 when unknown
#' @param anion Selected anion to use to for ion balance when more cations are present. Defaults to "cl". Choose one of c("cl", "so4").
#' @param cation Selected cation to use to for ion balance when more anions are present. Defaults to "na". Choose one of c("na", "k", "ca", or "mg").
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1) %>%
#'   balance_ions()
#'
#' water_defined <- define_water(7, 20, 50, tot_hard = 150) %>%
#'   balance_ions(anion = "so4")
#'
#' @export
#'
#' @returns A water class object with updated ions to balance water charge.
#'
balance_ions <- function(water, anion = "cl", cation = "na") {
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
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
    }else if (cation == "ca") {
      add_ca <- add_cat / 2
      ca_new <- ifelse(is.na(water@ca), add_ca, water@ca + add_ca)
      water@estimated <- paste(water@estimated, "ca", sep = "_")
    }else if (cation == "mg") {
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

  water@applied_treatment <- paste(water@applied_treatment, "_balanced", sep = "")

  # Update TDS/cond/IS if needed.
  if (grepl("tds", water@estimated) & grepl("cond", water@estimated)) {
    # Update TDS and cond if they were estimated from IS. Otherwise, assume initial values were measured.
    water@tds <- water@tds + convert_units(add_na, "na", "M", "mg/L") + convert_units(add_k, "k", "M", "mg/L") +
      convert_units(add_ca, "ca", "M", "mg/L") +  convert_units(add_mg, "mg", "M", "mg/L") +

      convert_units(add_cl, "cl", "M", "mg/L") + convert_units(add_so4, "so4", "M", "mg/L")

    water@cond <- correlate_ionicstrength(water@tds, from = "tds", to = "cond")
    # Similarly, IS should only update from the ion balance if TDS and cond were estimates.
    water@is <- calculate_ionicstrength(water)
  }

  return(water)
}

#' Apply `balance_ions` function and output a dataframe
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame.
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using \code{\link{define_water_chain}}
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param anion Selected anion to use to for ion balance when more cations are present. Defaults to "cl". Choose one of c("cl", "so4").
#' @param cation Selected cation to use to for ion balance when more anions are present. Defaults to "na". Choose one of c("na", "k", "ca", or "mg").
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_once(anion = "so4", cation = "mg")
#'
#' example_df <- water_df %>%
#'   define_water_chain(output_water = "Different_defined_water_column") %>%
#'   balance_ions_once(input_water = "Different_defined_water_column")
#'
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_once()
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export
#' @returns A dataframe with updated ions to balance water charge

balance_ions_once <- function(df, input_water = "defined_water",
                              anion = "cl", cation = "na") {
  balance_df <- balanced_water <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    mutate(balanced_water = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        anion = anion,
        cation = cation
        ), balance_ions)
      ) %>%
    mutate(balance_df = furrr::future_map(balanced_water, convert_water)) %>%
    unnest_wider(balance_df) %>%
    select(-balanced_water)
}

#' Apply `balance_ions` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using \code{\link{define_water_chain}}
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "balanced_water".
#' @param anion Selected anion to use to for ion balance when more cations are present. Defaults to "cl". Choose one of c("cl", "so4").
#' @param cation Selected cation to use to for ion balance when more anions are present. Defaults to "na". Choose one of c("na", "k", "ca", or "mg").
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain(anion = "so4", cation = "ca") %>%
#'   select(-defined_water, -balanced_water)
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
#' @returns A data frame containing a water class column with updated ions to balance water charge.

balance_ions_chain <- function(df, input_water = "defined_water", output_water = "balanced_water",
                               anion = "cl", cation = "na") {
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        anion = anion,
        cation = cation
      ), balance_ions)
    )
}
