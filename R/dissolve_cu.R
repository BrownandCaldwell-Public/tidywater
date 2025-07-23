# Calculate dissolved copper

#' @title Calculate Dissolved Copper Concentration
#' @description
#' This function takes a water defined by defined_water and output a column of dissolved copper. It is an empirical model developed
#' based on bench-scale copper solubility testing that can be used to predict copper levels as a function of pH, DIC, and orthophosphate.
#' For a single water, use `dissolve_cu`; to apply the model to a dataframe use `dissolve_cu_df`.
#'
#' @details Dissolved copper is a function of pH, DIC, and PO4. Output units are in mg/L.
#'
#' @source Lytle et al (2018)
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph and dic
#'
#' @examples
#'
#' example_cu <- define_water(ph = 7.5, alk = 125, tot_po4 = 2) %>%
#'   dissolve_cu()
#'
#' @export
#'
#' @returns `dissolve_cu` returns a column containing dissolved copper concentration in mg/L.
#'

dissolve_cu <- function(water) {
  validate_water(water, c("ph", "alk"))

  po4 <- convert_units(water@tot_po4, "h3po4", "M", "mg/L")

  # warnings if inputs are outside conditions the model was developed
  if (po4 < 0.2 || po4 > 3.1) {
    warning("This model was fit on waters with phosphate residual between 0.2-3.1 mg/L.")
  }
  if (water@ph > 8.52 || water@ph < 6.48) {
    warning("This model was not developed with pH values outside 6.48-8.52.")
  }

  cu <- 56.68 * (exp(-0.77 * water@ph)) * exp(-0.20 * water@tot_po4) * (water@dic^0.59)
  data.frame(cu)
}

#' @rdname dissolve_cu_df
#'
#' @title Calculate Dissolved Copper Concentration
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_df]
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#'
#' @examples
#' library(dplyr)
#' cu_calc <- water_df %>%
#'   define_water_df() %>%
#'   dissolve_cu_df()
#'
#' @returns `dissolve_cu_df` returns a data frame containing the original data frame and a column for dissolved copper in mg/L.
#'
#' @export
#'

dissolve_cu_df <- function(df, input_water = "defined", water_prefix = TRUE) {
  validate_water_helpers(df, input_water)

  cu_df <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    dissolve_cu(
      water = df[[input_water]][[i]]
    )
  }))

  if (water_prefix) {
    names(cu_df) <- paste0(input_water, "_", names(cu_df))
  }

  output <- cbind(df, cu_df)
  return(output)
}
