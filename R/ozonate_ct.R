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
#' @param kd First order decay constant. This parameter is optional. If not specified, the default ozone decay equations will be used.
#' @param baffle Baffle factor - unitless value between 0 and 1.
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' # Use kd from experimental data (recommended):
#' define_water(ph = 7.5, temp = 25) %>%
#'   ozonate_ct(time = 10, dose = 2, kd = -0.5, baffle = 0.9)
# Use modeled decay curve:
#' define_water(ph = 7.5, alk = 100, doc = 2, uv254 = .02, br = 50) %>%
#'   ozonate_ct(time = 10, dose = 2, baffle = 0.5)
#'
#' @import dplyr
#' @export
#'
ozonate_ct <- function(water, time, dose, kd, baffle) {
  validate_water(water, c("temp"))

  temp <- water@temp

  # First order decay curve: y = dose * exp(k*t)
  # Integral from 0 to t of curve above: dose * (exp(kt) - 1) / k
  if (!missing(kd)) {
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
