# Chlorine/Chloramine Decay Modeling functions
# These functions predict chlorine residual concentration given reaction time

#' @title Calculate total OCL
#'
#' @description \code{chemdose_cl2} calculates the decay of chlorine/chloramine based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001). Required arguments include an object of class "water"
#' created by \code{\link{define_water}} chlorine/chloramine dose, type, reaction time, and treatment applied (if any).
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including TOC and UVA.
#'
#' @details The function will calculate haloacetic acids (HAA) as HAA5, and total trihalomethanes (TTHM).
#' The function returns a new object of class "water" with predicted chlorine/chloramine residual concentrations, which
#' is total/combined chlorine concentration. For free concentration, the chemdose_ph functions should be implemented after
#' this function.
#' Use \code{summarise_wq} to quickly tabulate the results.
#'
#' @source Ct, raw: U.S. EPA (2001) equation 5-113
#' @source Ct, coagulated/treated: U.S. EPA (2001) equation 5-117
#' @source Ct corrected: U.S. EPA (2001) equation 5-118
#' @source CAt: U.S. EPA (2001) equation 5-120
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2_dose Applied chlorine or chloramine dose (mg/L as cl2_dose). Model results are valid for doses between 0.995 and 41.7 mg/L for raw water,
#' and for doses between 1.11 and 24.7 mg/L for coagulated water.
#' @param time Reaction time (hours). Chlorine decay model results are valid for reaction times between 0.25 and 120 hours.Chloramine decay model
#' does not have a time restriction.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default), "coag" for
#' water that has been coagulated or softened.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @param chlorine_correction Defaults to FALSE. Corrects the initial chlorine residual concentration when the cl_type is also "chlorine".
#' @examples
#' example_cl2 <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chemdose_cl2(cl2_dose = 2, time = 8)
#' example_cl2 <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   chemdose_cl2(cl2_dose = 3, time = 168, treatment = "coag", location = "ds")
#'
#' @export
#' @returns An updated disinfectant residual in the tot_ocl water slot.
#'
chemdose_cl2 <- function(water, cl2_dose, time, treatment = "raw", cl_type = "chlorine", chlorine_correction = FALSE) {
  toc = water@toc
  uv254 = water@uv254

  # define table for decay coefficients
  clcoeffs <- tibble(
    treatment = c('raw','coag','either'),
    a = c(-0.8147, -0.8404,-0.99),
    b = c(-2.2808, -0.404,-0.015),
    c = c(-1.2971, -0.9108, NA)
  )

  # put in the same table for coefficients
  # eventually put in the compiled script


  # Handle missing arguments with warnings (not all parameters are needed for all models).
   if (missing(cl2_dose) | missing(time)) {
    stop("Missing value for cl2_dose or time. Please check the function inputs required to calculate chlorine/chloramine decay")
  }

  if (cl_type != "chlorine" & cl_type != "chloramine") {
    stop("cl_type should be 'chlorine' or 'chloramine'. Please check the spelling for cl_type to calculate chlorine/chloramine decay.")
  }

  # chlorine decay model
  if (cl_type == "chlorine") {

    if (treatment != "raw" & treatment != "coag") {
      stop("For chlorine decay, the treatment type should be 'raw' or 'coag'. Please check the spelling for treatment.")
     }

    if (is.na(toc) | is.na(uv254)) {
      stop("For chlorine decay, missing value for toc or uv254. Please add missing parameters to define_water.")
    }

    # toc warnings
    if (treatment == "raw" & (toc < 1.2 | toc > 16)) {
      warning("For chlorine decay estimate, TOC is outside the model bounds of 1.2 <= TOC <= 16 mg/L for raw water")
    }

    if (treatment == "coag" & (toc < 1.0 | toc > 11.1)) {
      warning("For chlorine decay estimate, TOC is outside the model bounds of 1.0 <= TOC <= 11.1 mg/L for coagulated water")
    }

    # uv254 warnings
    if (treatment == "raw" & (uv254 < 0.010 | uv254 > 0.730)) {
      warning("For chlorine decay estimate, uv254 is outside the model bounds of 0.010 <= uv254 <= 0.730 mg/L for raw water.")
    }

    if (treatment == "coag" & (uv254 < 0.012 | uv254 > 0.250)) {
      warning("For chlorine decay estimate, uv254 is outside the model bounds of 0.012 <= UV254 <= 0.250 cm-1 for coagulated water.")
    }

    # cl2_dose warnings
    if (treatment == "raw" & (cl2_dose < 0.995 | cl2_dose > 41.7)) {
      warning("For chlorine decay estimate, chlorine is outside the model bounds of 0.995 <= cl2_dose <= 41.7 mg/L for raw water")
    }

    if (treatment == "coag" & (cl2_dose < 1.11 | cl2_dose > 24.7)) {
      warning("For chlorine decay estimate, chlorine is outside the model bounds of 1.11 <= cl2_dose <= 24.7 mg/L for coagulated water")
    }

    # time warning
    if (time < 0.25 | time > 120) {
      warning("For chlorine decay estimate, reaction time is outside the model bounds of 0.25 (15minutes) <= time <= 120 hours.")
    }

    # get coefficients from defined clcoeffs table
    if (treatment == "raw") {
      coeffs <- clcoeffs %>%
        filter(treatment == "raw")
    } else if (treatment == "coag") {
      coeffs <- clcoeffs %>%
        filter(treatment == "coag")
    }

    # define function for chlorine decay
    solve_decay <- function(Ct, a, b, cl2_dose, uv254, time, c, toc) {
      a * cl2_dose * log(cl2_dose/Ct) - b * (cl2_dose/uv254)^c *toc*time + cl2_dose - Ct
    }

  #chloramine decay model
  } else if (cl_type == "chloramine") {

    # chloramine decay model stop
    if (is.na(uv254)) {
      stop("For chloramine decay estimate, missing value for uv254 for chloramine decay estimate.
           Please add missing parameters to define_water.")
    }

    # define function for chloramine decay
    solve_decay <- function(Ct, a, b, cl2_dose, uv254, time, c, toc) {
      a * cl2_dose * log(cl2_dose/Ct) - b * uv254 * time + cl2_dose - Ct
    }

    coeffs <- clcoeffs %>%
      filter(treatment == "either")

  }


  # if dose is 0, do not run uniroot function
  if (cl2_dose == 0) {

    Ct <- 0

  } else {

      root_Ct <- stats::uniroot(solve_decay, interval = c(0, cl2_dose),
       a = coeffs$a,
       b = coeffs$b,
       c = coeffs$c,
       cl2_dose = cl2_dose,
       uv254 = uv254,
       toc = toc,
       time = time,
       tol = 1e-14)$root

      if (cl_type == "chlorine" & chlorine_correction == TRUE) {
        warning("Be mindful about the corrected chlorine residual concentration, because the correction
        applies to the initial Chlorine residual concentration, while the model input cl2_dose is the initial dose")

        # cl2_resid <- stats::uniroot(solve_decay, interval = c(0, cl2_dose),
        #  a = coeffs$a,
        #  b = coeffs$b,
        #  c = coeffs$c,
        #  cl2_dose = cl2_dose,
        #  uv254 = uv254,
        #  toc = toc,
        #  time = 1/100/60/60,
        #  tol = 1e-14)$root
        #
        # Ct <-  cl2_resid + (root_Ct - cl2_resid) / 0.85
        #
        # if (Ct < 0) { # correct negative concentrations to 0
        #   Ct <- 0
        # } else if (Ct > cl2_dose) { # correct concentrations greater than initial dose
        #   Ct <- cl2_dose
        # }


        Ct <- cl2_dose + (root_Ct - cl2_dose) / 0.85

      } else {
        Ct <- root_Ct
  }

  }

  # Convert final result to molar
  water@tot_ocl <- convert_units(Ct, "ocl", "mg/L", "M")

  return(water)
}
















