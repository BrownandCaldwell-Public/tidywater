#' @title Apply decarbonation to a water
#'
#' @description Calculates the new water quality (pH, alkalinity, etc) after a specified amount of CO2 is removed (removed as bicarbonate).
#' The function takes an object of class "water" and a fraction of CO2 removed, then returns a water class object with updated water slots.
#' For a single water, use `decarbonate_ph`; to apply the model to a dataframe, use `decarbonate_ph_chain`.
#' For a single water use `chemdose_toc`; for a dataframe use `chemdose_toc_chain`.
#' Use `pluck_cols = TRUE` to get values from the output water as new dataframe columns.
#' For most arguments in the `_chain` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details
#'
#' `decarbonate_ph` uses `water@h2co3` to determine the existing CO2 in water, then applies [chemdose_ph] to match the CO2 removal.
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
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "decarbonated".
#' @param pluck_cols Extract water slots modified by the function (ph, alk) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   decarbonate_ph_chain(
#'     input_water = "defined", output_water = "decarb",
#'     co2_removed = .95, pluck_cols = TRUE
#'   )
#'
#' @export
#' @returns `decarbonate_ph_chain` returns a data frame containing a water class column with updated ph and alk (and pH dependent ions).
#' Optionally, it also adds columns for each of those slots individually.

decarbonate_ph_chain <- function(df, input_water = "defined", output_water = "decarbonated",
                                 pluck_cols = FALSE, water_prefix = TRUE,
                                 co2_removed = "use_col") {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  co2_removed <- tryCatch(co2_removed, error = function(e) enquo(co2_removed))

  arguments <- construct_helper(df, list("co2_removed" = co2_removed))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    decarbonate_ph(
      water = df[[input_water]][[i]],
      co2_removed = df[[final_names$co2_removed]][i]
    )
  })

  output <- df

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("ph", "alk"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  return(output)
}
