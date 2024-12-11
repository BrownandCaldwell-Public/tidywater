# Helper Functions
# These directly interact with a water class.
# _chain and _once functions should be in the R script with their respective models.

#' Convert `water` class object to a dataframe
#'
#' This converts a `water` class to a dataframe with individual columns for each slot (water quality parameter) in the `water`.
#' This is useful for one-off checks and is applied in all `fn_once` tidywater functions. For typical applications,
#' there may be a `fn_once` tidywater function that provides a more efficient solution.
#'
#'
#' @param water A water class object
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#'
#' # Generates 1 row dataframe
#' example_df <- define_water(ph = 7, temp = 20, alk = 100) %>%
#'   convert_water()
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(to_dataframe = map(defined_water, convert_water)) %>%
#'   unnest(to_dataframe) %>%
#'   select(-defined_water)
#'
#' @import dplyr
#' @export
#'
#' @returns A data frame containing columns for all non-NA water slots.

convert_water <- function(water) {
  nms <- methods::slotNames(water)
  lst <- lapply(nms, function(nm) methods::slot(water, nm))
  as.data.frame(stats::setNames(lst, nms)) %>%
    select(where(~ any(!is.na(.))))
}

#' @title Convert a `water` class object to a dataframe with ions in mg/L or ug/L
#'
#' @description This function is the same as \code{\link{convert_water}} except it converts the units of following slots from
#' M to mg/L: na, ca, mg, k, cl, so4, hco3, co3, h2po4, hpo4, po4, ocl, bro3, f, fe, al.  These slots are converted to
#' ug/L: br, mn.  All other values remain unchanged.
#'
#' @param water A water class object
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1) %>%
#'   convert_watermg()
#'
#' @export
#'
#' @returns A data frame containing columns for all non-NA water slots with ions in mg/L.

convert_watermg <- function(water) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }

  water@na <- convert_units(water@na, "na", "M", "mg/L")
  water@ca <- convert_units(water@ca, "ca", "M", "mg/L")
  water@mg <- convert_units(water@mg, "mg", "M", "mg/L")
  water@k <- convert_units(water@k, "k", "M", "mg/L")
  water@cl <- convert_units(water@cl, "cl", "M", "mg/L")
  water@so4 <- convert_units(water@so4, "so4", "M", "mg/L")
  water@hco3 <- convert_units(water@hco3, "hco3", "M", "mg/L")
  water@co3 <- convert_units(water@co3, "co3", "M", "mg/L")
  water@h2po4 <- convert_units(water@h2po4, "h2po4", "M", "mg/L")
  water@hpo4 <- convert_units(water@hpo4, "hpo4", "M", "mg/L")
  water@po4 <- convert_units(water@po4, "po4", "M", "mg/L")
  water@ocl <- convert_units(water@ocl, "ocl", "M", "mg/L")
  water@tot_nh3 <- convert_units(water@nh4, "nh4", "M", "mg/L N")

  water@bro3 <- convert_units(water@bro3, "bro3", "M", "mg/L")
  water@f <- convert_units(water@f, "f", "M", "mg/L")
  water@fe <- convert_units(water@fe, "fe", "M", "mg/L")
  water@al <- convert_units(water@al, "al", "M", "mg/L")

  # These get converted to ug/L instead.
  water@br <- convert_units(water@br, "br", "M", "ug/L")
  water@mn <- convert_units(water@mn, "mn", "M", "ug/L")

  convert_water(water)
}

#' Pluck out a single parameter from a `water` class object
#'
#' This function plucks one or more selected parameters from selected columns of `water` class objects.
#' The names of the output columns will follow the form `water_parameter`
#' To view all slots as columns, please use one of the `fn_once` functions or \code{\link{convert_water}}.
#'
#' @param df a data frame containing a water class column, which has already
#' been computed using \code{\link{define_water}}
#' @param input_waters vector of names of the columns of water class data to be used as the input for this function.
#' @param parameter vector of water class parameters to view outside the water column
#'
#' @seealso \code{\link{convert_water}}
#'
#' @examples
#'
#' library(dplyr)
#' library(furrr)
#' library(purrr)
#' library(tidyr)
#'
#' pluck_example <- water_df %>%
#'   define_water_chain() %>%
#'   pluck_water(parameter = "tot_co3")
#'
#' pluck_example <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   pluck_water(input_waters = c("defined_water", "balanced_water"), parameter = c("na", "cl"))
#'
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' pluck_example <- water_df %>%
#'   define_water_chain() %>%
#'   pluck_water(parameter = c("ph", "alk"))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' @import dplyr
#' @export
#' @returns A data frame containing columns of selected parameters from a list of water class objects.

pluck_water <- function(df, input_waters = c("defined_water"), parameter) {
  if (missing(parameter)) {
    stop("Parameter not specified to pluck.")
  }
  if (!any(parameter %in% methods::slotNames("water"))) {
    stop("One or more parameters doesn't exist in water class.")
  }
  if (!any(input_waters %in% colnames(df))) {
    stop("One or more specified waters doesn't exist in dataframe. Check column names.")
  }


  plucked <- data.frame(row.names = seq(1, nrow(df)))
  for (water in input_waters) {
    if (!methods::is(df[[water]][[1]], "water")) {
      stop("All waters must be of class 'water'.")
    }
    output_column <- paste0(water, "_", parameter)
    temp <- furrr::future_map2(parameter, output_column, ~ {
      df %>%
        mutate(!!as.name(.y) := furrr::future_map_dbl(!!as.name(water), pluck, .x)) %>%
        select(!!as.name(.y))
    }) %>%
      purrr::list_cbind()

    plucked <- bind_cols(plucked, temp)
  }

  bind_cols(df, plucked)
}
