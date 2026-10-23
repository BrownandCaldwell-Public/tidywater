# Calculate dissolved copper

#' @title Calculate Dissolved Copper Concentration
#' @description
#' This function takes a water defined by defined_water and output a column of dissolved copper. It is an empirical model developed
#' based on bench-scale copper solubility testing that can be used to predict copper levels as a function of pH, DIC, and orthophosphate.
#' For a single water, use `dissolve_cu`; to apply the model to a dataframe use `dissolve_cu_df`.
#'
#' @details Dissolved copper is a function of pH, DIC (mg/L as C), and orthophosphate (mg/L as PO4). Output units are in
#' mg/L. The model was fit on waters with pH 6.48 to 8.52, orthophosphate 0.2 to 3.1 mg/L as PO4, and DIC of 10, 50,
#' and 100 mg/L as C (Lytle et al. 2018, equation 1); a warning is given for inputs outside these ranges.
#'
#' DIC is not an input: \code{\link{define_water}} calculates it from pH and alkalinity. Lytle et al. (2018) give no
#' alkalinity range, and the alkalinity that corresponds to a given DIC depends on pH. At 25 C, the 10 to 100 mg/L as C
#' DIC range is roughly 24 to 244 mg/L as CaCO3 alkalinity at pH 6.5, 34 to 340 at pH 7, 39 to 390 at pH 7.5,
#' and 41 to 410 at pH 8.
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
  # dic is not validated separately: define_water() always calculates it from ph and alk
  validate_water(water, c("ph", "alk"))

  # Lytle et al. (2018) fit orthophosphate in mg/L as PO4; water@tot_po4 is stored in M
  po4 <- convert_units(water@tot_po4, "po4", "M", "mg/L")

  # warnings if inputs are outside conditions the model was developed
  if (po4 < 0.2 | po4 > 3.1) {
    tidywater_warn("This model was fit on waters with phosphate residual between 0.2 and 3.1 mg/L.")
  }
  if (water@ph > 8.52 | water@ph < 6.48) {
    tidywater_warn("This model was not developed for pH values outside 6.48 to 8.52.")
  }
  # Lytle et al. (2018) fit the model at DIC of 10, 50, and 100 mg C/L
  if (water@dic < 10 | water@dic > 100) {
    tidywater_warn("This model was fit on waters with DIC (calculated from pH and alkalinity) between 10 and 100 mg/L as C.")
  }

  cu <- 56.68 * exp(-0.77 * water@ph) * exp(-0.20 * po4) * water@dic^0.59
  data.frame(cu)
}

#' @rdname dissolve_cu_df
#'
#' @title Calculate Dissolved Copper Concentration
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_df]
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
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
warning_counts <- list()

  cu_df <- do.call(
    rbind,
    lapply(seq_len(nrow(df)), function(i) {
      withCallingHandlers(
        dissolve_cu(
          water = df[[input_water]][[i]]
        ),
        warning = function(w) {
          msg <- conditionMessage(w)
          warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) + 1L
          invokeRestart("muffleWarning")
        }
      )
    })
  )

  for (msg in names(warning_counts)) {
    tidywater_warn("{msg} ({warning_counts[[msg]]} row{?s} affected.)")
  }

  if (water_prefix) {
    names(cu_df) <- paste0(input_water, "_", names(cu_df))
  }

  output <- cbind(df, cu_df)
  return(output)
}
