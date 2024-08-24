# CT Calculations

#' Determine disinfection credit from chlorine.
#'
#' @description This function takes a water defined by \code{\link{define_water}} and other disinfection parameters
#' and outputs a dataframe of the required CT (`ct_required`), actual CT (`ct_actual`), and giardia log removal (`glog_removal`).
#'
#' @details CT actual is a function of time, chlorine residual, and baffle factor, whereas CT required is a function of
#' pH, temperature, chlorine residual, and the standard 0.5 log removal of giardia requirement.  CT required is an
#' empirical regression equation developed by Smith et al. (1995) to provide conservative estimates for CT tables
#' in USEPA Disinfection Profiling Guidance.
#' Log removal is a rearrangement of the CT equations.
#'
#' @source Smith et al. (1995)
#' @source USEPA (2020)
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
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

#' Determine disinfection credit from ozone.
#'
#' @description This function takes a water defined by \code{\link{define_water}} and the first order decay curve parameters
#' from an ozone dose and outputs a dataframe of acutal CT, and log removal for giardia, virus, and crypto
#'
#' @details First order decay curve for ozone has the form: `residual = dose * exp(kd*time)`. kd should be a negative number.
#' Actual CT is an integration of the first order curve. The first 30 seconds are removed from the integral to account for
#' instantaneous demand.
#'
#' @source USEPA (2020) Equation 4-4 through 4-7
#' https://www.epa.gov/system/files/documents/2022-02/disprof_bench_3rules_final_508.pdf
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph and temp
#' @param time Retention time of disinfection segment in minutes.
#' @param dose Ozone dose in mg/L. This value can also be the y intercept of the decay curve (often slightly lower than ozone dose.)
#' @param kd First order decay constant.
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' # Use kd from experimental data (recommended):
#' define_water(ph = 7.5, temp = 25) %>%
#'   ozonate_ct(time = 10, dose = 2, kd = -0.5)
#' # Use modeled decay curve:
#' define_water(ph = 7.5, alk = 100, doc = 2, uv254 = .02, br = 50) %>%
#'   ozonate_ct(time = 10, dose = 2)
#'
#' @export
#'
ozonate_ct <- function(water, time, dose, kd) {
  validate_water(water, c("temp"))

  temp <- water@temp

  # First order decay curve: y = dose * exp(k*t)
  # Integral from 0 to t of curve above: dose * (exp(kt) - 1) / k
  if (!missing(kd)) {
    ct_tot <- dose * (exp(kd * time) - 1) / kd
    ct_inst <- dose * (exp(kd * .5) - 1) / kd
    ct_actual <- ct_tot - ct_inst # Remove the first 30 seconds to account for instantaneous demand
  } else {
    validate_water(water, c("ph", "temp", "alk", "doc", "uv254", "br"))

    decaycurve <- data.frame(time = seq(0, time, .5)) %>%
      mutate(
        defined_water = list(water),
        dose = dose
      ) %>%
      solveresid_o3_once() %>%
      mutate(ct = o3resid * .5) %>%
      filter(time != 0)
    ct_actual <- sum(decaycurve$ct)
  }

  giardia_log_removal <- 1.038 * 1.0741^temp * ct_actual
  virus_log_removal <- 2.1744 * 1.0726^temp * ct_actual
  crypto_log_removal <- 0.0397 * 1.09757^temp * ct_actual

  tibble(
    "ct_actual" = ct_actual, "glog_removal" = giardia_log_removal, "vlog_removal" = virus_log_removal,
    "clog_removal" = crypto_log_removal
  )
}
