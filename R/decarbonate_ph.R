#' @title Apply decarbonation to a water
#'
#' @description Calculates the required amount of a chemical to dose based on a target pH and existing water quality.
#' The function takes an object of class "water", and user-specified chemical and target pH
#' and returns a numeric value for the required dose in mg/L.
#' For a single water, use `decarbonate_ph`; to apply the model to a dataframe, use `decarbonate_ph_chain`.
#' For most arguments, the `_chain` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details
#'
#' `decarbonate_ph` uses [water@h2co3] to determine the existing CO2 in water, then applies [chemdose_ph] to match the CO2 removal.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param co2_removed Fraction of CO2 removed
#'
#' @seealso [chemdose_ph]
#'
#' @examples
#' water <- define_water(ph = 4, temp = 25, alk = 5) %>%
#'   decarbonate_ph(co2_removed = .95)
#'
#' @export
#' @returns  A water with updated pH/alk/etc.
#'
decarbonate_ph <- function(water, co2_removed) {
  validate_water(water, c("ph", "alk"))
  if (missing(co2_removed)) {
    stop("No CO2 removal defined. Enter a value for co2_removed between 0 and 1.")
  }

  if ((co2_removed > 1 | co2_removed < 0) & !is.na(co2_removed)) {
    stop("CO2 removed should be a fraction of the total CO2, between 0 and 1.")
  }

  co2_mol <- water@h2co3 * co2_removed
  co2_mg <- convert_units(co2_mol, "co2", "M", "mg/L") * -1

  chemdose_ph(water, co2 = co2_mg)
}



#' @rdname decarbonate_ph
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain]. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "dosed_chem_water".
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   decarbonate_ph_chain(input_water = "defined_water", output_water = "decarb", co2_removed = .95)
#'
#' @import dplyr
#' @export
#' @returns `decarbonate_ph_chain` returns a data frame with a column containing a water with updated pH, alk, etc.

decarbonate_ph_chain <- function(df, input_water = "defined_water", output_water = "decarbonated_water",
                                co2_removed = "use_col") {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  co2_removed <- tryCatch(co2_removed, error = function(e) enquo(co2_removed))

  arguments <- construct_helper(df, list("co2_removed" = co2_removed))

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        co2_removed = !!as.name(arguments$final_names$co2_removed)
      ),
      decarbonate_ph
    ))
}
