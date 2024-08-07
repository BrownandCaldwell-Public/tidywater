# CT Calculations

#' Determine disinfection credit from chlorine.
#'
#' @description This function takes a water defined by \code{\link{define_water}} and other disinfection parameters
#' and outputs a dataframe of the actual CT, required CT, and log removal.
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
#'   chemdose_ct()
#' example_ct <- define_water(ph = 7.5, temp = 25) %>%
#'   chemdose_ct()
#'
#' @export

chemdose_ct <- function(water, time, residual, baffle, volume, flow) {
  ph <- water@ph
  temp <- water@temp

  if (missing(time) & !missing(volume) & !missing(flow)) {
    time <- volume / flow
  }

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

ozonate_ct <- function(water, time, A, k) {
  ph <- water@ph
  temp <- water@temp


  # First order decay curve: y = A * exp(k*t)
  # Integral from 0 to t of curve above: A (exp(kt) - 1) / k

  ct_tot <- A * (exp(k * time) - 1) / k
  ct_inst <- A * (exp(k * .5) - 1) / k
  ct_actual <- ct_tot - ct_inst # Remove the first 30 seconds to account for instantaneous demand
  log_removal <- 1.038 * 1.0741^temp * ct_actual

  log_removal
}




solveresid_o3 <- function(water, dose, time) {
  doc <- water@doc
  ph <- water@ph
  temp <- water@temp
  uv254 <- water@uv254
  suva <- water@uv254 / water@doc * 100
  alk <- water@alk

  # This is the model from the WTP manual, but I can't get it working for a decay curve.
  o3demand <- 0.995 * dose^1.312 * (dose / uv254)^-.386 * suva^-.184 * (time)^.068 * alk^.023 * ph^.229 * temp^.087
  o3residual <- dose - o3demand
  o3residual
  # From me fitting data:
  # A = dose - 0.291 * doc ^ (0.656)
  # A <- dose - .291 * doc ^ .656

  # k = D*doc^d*E*doc^e*F^ph
  # fit all data: -4.9, 1.3, 5.4, -1,.0001,2.2 value = 1.13
  # fit tollefson: -1.3,1.8,.12,.23,.000002,5.8 value = .037

  # k <- -1.3 * dose ^ 1.8 * .12 * doc ^ .23 * 2E-6 * ph ^ 5.8

  # residual <- A * exp(k * time)
}
