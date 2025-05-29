#' Determine disinfection credit from ozone.
#'
#' @description This function takes a water defined by [define_water()] and the first order decay curve parameters
#' from an ozone dose and outputs a dataframe of actual CT, and log removal for giardia, virus, and crypto.
#' For a single water, use `solvect_o3`; to apply the model to a dataframe, use `solvect_o3_once`.
#' For most arguments, the `_once` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details First order decay curve for ozone has the form: `residual = dose * exp(kd*time)`. kd should be a negative number.
#' Actual CT is an integration of the first order curve. The first 30 seconds are removed from the integral to account for
#' instantaneous demand.
#'
#' When `kd` is not specified, a default decay curve is used from the Water Treatment Plant Model (2002). This model does
#' not perform well for ozone decay, so specifying the decay curve is recommended.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @source USEPA (2020) Equation 4-4 through 4-7
#' https://www.epa.gov/system/files/documents/2022-02/disprof_bench_3rules_final_508.pdf
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#'
#' @param water Source water object of class "water" created by [define_water()]. Water must include ph and temp
#' @param time Retention time of disinfection segment in minutes.
#' @param dose Ozone dose in mg/L. This value can also be the y intercept of the decay curve (often slightly lower than ozone dose.)
#' @param kd First order decay constant. This parameter is optional. If not specified, the default ozone decay equations will be used.
#' @param baffle Baffle factor - unitless value between 0 and 1.
#'
#' @examples
#'
#' # Use kd from experimental data (recommended):
#' define_water(ph = 7.5, temp = 25) %>%
#'   solvect_o3(time = 10, dose = 2, kd = -0.5, baffle = 0.9)
# Use modeled decay curve:
#' define_water(ph = 7.5, alk = 100, doc = 2, uv254 = .02, br = 50) %>%
#'   solvect_o3(time = 10, dose = 2, baffle = 0.5)
#'
#' @import dplyr
#' @export
#' @returns `solvect_o3` returns a data frame containing actual CT (mg/L*min), giardia log removal, virus log removal, and crypto log removal.
#'
solvect_o3 <- function(water, time, dose, kd, baffle) {
  validate_water(water, c("temp"))

  temp <- water@temp

  if (!missing(kd)) {
    if (!is.na(kd)) {
      use_kd <- TRUE
    } else {
      use_kd <- FALSE
    }
  } else {
    use_kd <- FALSE
  }
  # First order decay curve: y = dose * exp(k*t)
  # Integral from 0 to t of curve above: dose * (exp(kt) - 1) / k
  if (use_kd) {
    ct_tot <- dose * (exp(kd * time) - 1) / kd
    ct_inst <- dose * (exp(kd * .5) - 1) / kd
    ct_tot <- ct_tot - ct_inst # Remove the first 30 seconds to account for instantaneous demand
  } else {
    validate_water(water, c("ph", "temp", "alk", "doc", "uv254", "br"))

    decaycurve <- data.frame(time = seq(0, time, .5)) %>%
      dplyr::mutate(
        defined_water = list(water),
        dose = dose
      ) %>%
      solveresid_o3_once() %>%
      dplyr::mutate(ct = .data$o3resid * .5) %>%
      dplyr::filter(time != 0)
    ct_tot <- sum(decaycurve$ct)
  }

  ct_actual <- ct_tot * baffle
  giardia_log_removal <- 1.038 * 1.0741^temp * ct_actual
  virus_log_removal <- 2.1744 * 1.0726^temp * ct_actual
  crypto_log_removal <- 0.0397 * 1.09757^temp * ct_actual

  tibble(
    "ct_actual" = ct_actual, "glog_removal" = giardia_log_removal, "vlog_removal" = virus_log_removal,
    "clog_removal" = crypto_log_removal
  )
}

#' @rdname solvect_o3
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_chain()]
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param water_prefix name of the input water used for the calculation will be appended to the start of output columns. Default is TRUE.
#'
#' @examples
#' library(dplyr)
#' ct_calc <- water_df %>%
#'   define_water_chain() %>%
#'   solvect_o3_once(dose = 2, kd = -0.5, time = 10, baffle = .5)
#'
#' ozone_resid <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   mutate(
#'     dose = rep(seq(1, 4, 1), 3),
#'     time = rep(seq(2, 8, 2), 3),
#'     baffle = .5,
#'   ) %>%
#'   solvect_o3_once()
#'
#' @import dplyr
#' @export
#' @returns `solvect_o3_once` returns a data frame containing the original data frame and columns for required CT, actual CT, and giardia log removal.

solvect_o3_once <- function(df, input_water = "defined_water",
                            time = "use_col", dose = "use_col", kd = "use_col", baffle = "use_col",
                            water_prefix = TRUE) {
  calc <- ct_required <- ct_actual <- glog_removal <- vlog_removal <- clog_removal <- NULL # Quiet RCMD check global variable note

  validate_water_helpers(df, input_water)

  # This allows for the function to process unquoted column names without erroring
  time <- tryCatch(time, error = function(e) enquo(time))
  dose <- tryCatch(dose, error = function(e) enquo(dose))
  kd <- tryCatch(kd, error = function(e) enquo(kd))
  baffle <- tryCatch(baffle, error = function(e) enquo(baffle))

  arguments <- construct_helper(df, list("time" = time, "dose" = dose, "kd" = kd, "baffle" = baffle))

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
        dose = !!as.name(arguments$final_names$dose),
        kd = if (arguments$final_names$kd %in% names(.)) !!sym(arguments$final_names$kd) else rep(NA, nrow(.)),
        baffle = !!as.name(arguments$final_names$baffle)
      ),
      solvect_o3
    )) %>%
    unnest_wider(calc)

  if (water_prefix) {
    output <- output %>%
      rename(
        !!paste(input_water, "ct_actual", sep = "_") := ct_actual,
        !!paste(input_water, "glog_removal", sep = "_") := glog_removal,
        !!paste(input_water, "vlog_removal", sep = "_") := vlog_removal,
        !!paste(input_water, "clog_removal", sep = "_") := clog_removal
      )
  }
}
