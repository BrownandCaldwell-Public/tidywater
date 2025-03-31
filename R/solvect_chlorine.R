# CT Calculations

#' Determine disinfection credit from chlorine.
#'
#' @description This function takes a water defined by [define_water] and other disinfection parameters
#' and outputs a data frame of the required CT (`ct_required`), actual CT (`ct_actual`), and giardia log removal (`glog_removal`).
#' For a single water, use `solvect_chlorine`; to apply the model to a dataframe, use `solvect_chlorine_once`.
#' For most arguments, the `_chain` and `_once` helpers
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details CT actual is a function of time, chlorine residual, and baffle factor, whereas CT required is a function of
#' pH, temperature, chlorine residual, and the standard 0.5 log removal of giardia requirement.  CT required is an
#' empirical regression equation developed by Smith et al. (1995) to provide conservative estimates for CT tables
#' in USEPA Disinfection Profiling Guidance.
#' Log removal is a rearrangement of the CT equations.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @source Smith et al. (1995)
#' @source USEPA (2020)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph and temp
#' @param time Retention time of disinfection segment in minutes.
#' @param residual Minimum chlorine residual in disinfection segment in mg/L as Cl2.
#' @param baffle Baffle factor - unitless value between 0 and 1.
#' @param use_free_cl_slot Defaults to FALSE. When TRUE, uses free_chlorine slot in water instead of 'residual' argument.
#'
#' @examples
#'
#' example_ct <- define_water(ph = 7.5, temp = 25) %>%
#'   solvect_chlorine(time = 30, residual = 1, baffle = 0.7)
#' @export
#'
#' @returns `solvect_chlorine` returns a data frame containing required CT (mg/L*min), actual CT (mg/L*min), and giardia log removal.

solvect_chlorine <- function(water, time, residual, baffle, use_free_cl_slot = FALSE) {
  if (use_free_cl_slot){
    validate_water(water, c("ph", "temp", "free_chlorine"))
    residual <- water@free_chlorine
  } else validate_water(water, c("ph", "temp"))

  validate_args(num_args = list("time" = time, "residual" = residual, "baffle" = baffle))

  ph <- water@ph
  temp <- water@temp

  ct_actual <- residual * time * baffle

  if (temp < 12.5) {
    ct_required <- (.353 * .5) * (12.006 + exp(2.46 - .073 * temp + .125 * residual + .389 * ph))
    giardia_log_removal <- ct_actual / (12.006 + exp(2.46 - .073 * temp + .125 * residual + .389 * ph)) * 1 / .353
  } else {
    ct_required <- (.361 * 0.5) * (-2.216 + exp(2.69 - .065 * temp + .111 * residual + .361 * ph))
    giardia_log_removal <- ct_actual / (-2.216 + exp(2.69 - .065 * temp + .111 * residual + .361 * ph)) / .361
  }

  tibble("ct_required" = ct_required, "ct_actual" = ct_actual, "glog_removal" = giardia_log_removal)
}


#' @rdname solvect_chlorine
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_chain]
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param water_prefix name of the input water used for the calculation will be appended to the start of output columns. Default is TRUE.
#'
#' @examples
#' library(dplyr)
#' ct_calc <- water_df %>%
#'   define_water_chain() %>%
#'   solvect_chlorine_once(residual = 2, time = 10, baffle = .5)
#'
#' ozone_resid <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   mutate(
#'     residual = seq(1, 12, 1),
#'     time = seq(2, 24, 2),
#'     baffle = 0.7
#'   ) %>%
#'   solvect_chlorine_once()
#'
#' @returns `solvect_chlorine_once` returns a data frame containing the original data frame and columns for required CT, actual CT, and giardia log removal.
#' @import dplyr
#' @export

solvect_chlorine_once <- function(df, input_water = "defined_water",
                                  time = "use_col", residual = "use_col", baffle = "use_col",
                                  water_prefix = TRUE) {
  calc <- ct_required <- ct_actual <- glog_removal <- NULL # Quiet RCMD check global variable note

  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  time <- tryCatch(time, error = function(e) enquo(time))
  residual <- tryCatch(residual, error = function(e) enquo(residual))
  baffle <- tryCatch(baffle, error = function(e) enquo(baffle))

  arguments <- construct_helper(df, list("time" = time, "residual" = residual, "baffle" = baffle))

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(calc := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        time = !!as.name(arguments$final_names$time),
        residual = !!as.name(arguments$final_names$residual),
        baffle = !!as.name(arguments$final_names$baffle)
      ),
      solvect_chlorine
    )) %>%
    unnest_wider(calc)

  if (water_prefix) {
    output <- output %>%
      rename(
        !!paste(input_water, "ct_required", sep = "_") := ct_required,
        !!paste(input_water, "ct_actual", sep = "_") := ct_actual,
        !!paste(input_water, "glog_removal", sep = "_") := glog_removal
      )
  }
}
