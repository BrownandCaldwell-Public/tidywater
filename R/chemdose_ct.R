# CT Calculations

#' Determine disinfection credit from chlorine.
#'
#' @description This function takes a water defined by \code{\link{define_water}} and other disinfection parameters
#' and outputs a data frame of the required CT (`ct_required`), actual CT (`ct_actual`), and giardia log removal (`glog_removal`).
#'
#' @details CT actual is a function of time, chlorine residual, and baffle factor, whereas CT required is a function of
#' pH, temperature, chlorine residual, and the standard 0.5 log removal of giardia requirement.  CT required is an
#' empirical regression equation developed by Smith et al. (1995) to provide conservative estimates for CT tables
#' in USEPA Disinfection Profiling Guidance.
#' Log removal is a rearrangement of the CT equations.
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
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' example_ct <- define_water(ph = 7.5, temp = 25) %>%
#'   chemdose_ct(time = 30, residual = 1, baffle = 0.7)
#' @export
#'
#' @returns A data frame of the required CT, actual CT, and giardia log removal.

chemdose_ct <- function(water, time, residual, baffle) {
  validate_water(water, c("ph", "temp"))

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


#' Apply `chemdose_ct` to a data frame and create a new column with residual ozone dose
#'
#' This function allows \code{\link{chemdose_ct}} to be added to a piped data frame.
#' Three additional columns will be added to the data frame; ct_required (mg/L*min), ct_actual (mg/L*min), glog_removal
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using \code{\link{define_water_chain}}
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param time Retention time of disinfection segment in minutes.
#' @param residual Minimum chlorine residual in disinfection segment in mg/L as Cl2.
#' @param baffle Baffle factor - unitless value between 0 and 1.
#' @param water_prefix name of the input water used for the calculation, appended to the start of output columns. Default is TRUE.
#'
#' @examples
#' library(dplyr)
#' ct_calc <- water_df %>%
#'   define_water_chain() %>%
#'   chemdose_ct_once(residual = 2, time = 10)
#'
#' ozone_resid <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   mutate(
#'     dose = seq(1, 12, 1),
#'     time = seq(2, 24, 2)
#'   ) %>%
#'   chemdose_ct_once()
#'
#' @import dplyr
#' @export
#' @returns A data frame containing the original data frame and columns for required CT, actual CT, and giardia log removal.

chemdose_ct_once <- function(df, input_water = "defined_water", time = 0, residual = 0, baffle = 0, water_prefix = TRUE) {
  calc <- ct_required <- ct_actual <- glog_removal <- ID <- NULL # Quiet RCMD check global variable note
  inputs_arg <- data.frame(time, residual, baffle) %>%
    select_if(~ any(. > 0))

  inputs_col <- df %>%
    subset(select = names(df) %in% c("time", "residual", "baffle")) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(inputs_col) < 3 & length(inputs_arg) == 0) {
    warning("Time, residual, and/or baffle arguments missing. Add them as a column or function argument.")
  }

  if (("time" %in% colnames(inputs_arg) & "time" %in% colnames(inputs_col)) | ("residual" %in% colnames(inputs_arg) & "residual" %in% colnames(inputs_col)) |
    ("baffle" %in% colnames(inputs_arg) & "baffle" %in% colnames(inputs_col))) {
    stop("Time, residual, and/or baffle were dosed as both a function argument and a data frame column. Choose one input method.")
  }

  arguments <- inputs_col %>%
    cross_join(inputs_arg)

  output <- df %>%
    subset(select = !names(df) %in% c("dose", "time")) %>%
    mutate(
      ID = row_number()
    ) %>%
    left_join(arguments, by = "ID") %>%
    select(-ID) %>%
    mutate(calc = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        time = time,
        residual = residual,
        baffle = baffle
      ),
      chemdose_ct
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
