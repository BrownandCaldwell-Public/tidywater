# Chlorine/Chloramine Decay Modeling functions
# These functions predict chlorine residual concentration given reaction time

#' @title Calculate chlorine decay
#'
#' @description \code{chemdose_cl2} calculates the decay of chlorine or chloramine based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001).
#'
#' @details Required arguments include an object of class "water" created by \code{\link{define_water}},
#' applied chlorine/chloramine dose, type, reaction time, and treatment applied (options include "raw" for
#' no treatment, or "coag" for coagulated water). The function also requires additional water quality
#' parameters defined in \code{define_water} including TOC and UV254. The output is a new "water" class
#' with the calculated total chlorine value stored in the 'tot_ocl' slot. When modeling residual concentrations
#' through a unit process, the U.S. EPA Water Treatment Plant Model applies a correction factor based on the
#' influent and effluent residual concentrations (see U.S. EPA (2001) equation 5-118) that may need to be
#' applied manually be the user based on the output of \code{chemdose_cl2}.
#'
#' @source U.S. EPA (2001)
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2_dose Applied chlorine or chloramine dose (mg/L as cl2). Model results are valid for doses between 0.995 and 41.7 mg/L for raw water,
#' and for doses between 1.11 and 24.7 mg/L for coagulated water.
#' @param time Reaction time (hours). Chlorine decay model results are valid for reaction times between 0.25 and 120 hours.Chloramine decay model
#' does not have specified boundary conditions.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default), "coag" for
#' water that has been coagulated or softened.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @examples
#' example_cl2 <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = 0.2)) %>%
#'   chemdose_cl2(cl2_dose = 2, time = 8)
#' @export
#' @returns An updated disinfectant residual in the tot_ocl water slot in units of M. Use \code{\link{convert_units}} to convert to mg/L.
#'
chemdose_cl2 <- function(water, cl2_dose, time, treatment = "raw", cl_type = "chlorine") {

  validate_water(water, c("toc", "uv254"))

  toc = water@toc
  uv254 = water@uv254

  # Handle missing arguments with warnings (not all parameters are needed for all models).
   if (missing(cl2_dose)) {
    stop("Missing value for chlorine dose. Please check the function inputs required to calculate chlorine/chloramine decay.")
   }

  if (missing(time)) {
    stop("Missing value for reaction time. Please check the function inputs required to calculate chlorine/chloramine decay.")
  }

  if (!(cl_type %in% c("chlorine", "chloramine"))) {
    stop("cl_type should be 'chlorine' or 'chloramine'. Please check the spelling for cl_type to calculate chlorine/chloramine decay.")
  }

  # chlorine decay model
  if (cl_type == "chlorine") {

    if (!(treatment %in% c("raw", "coag"))) {
      stop("The treatment type should be 'raw' or 'coag'. Please check the spelling for treatment.")
     }

    # toc warnings
    if (treatment == "raw" & (toc < 1.2 | toc > 16)) {
      warning("TOC is outside the model bounds of 1.2 <= TOC <= 16 mg/L for raw water.")
    }

    if (treatment == "coag" & (toc < 1.0 | toc > 11.1)) {
      warning("TOC is outside the model bounds of 1.0 <= TOC <= 11.1 mg/L for coagulated water.")
    }

    # uv254 warnings
    if (treatment == "raw" & (uv254 < 0.010 | uv254 > 0.730)) {
      warning("UV254 is outside the model bounds of 0.010 <= UV254 <= 0.730 cm-1 for raw water.")
    }

    if (treatment == "coag" & (uv254 < 0.012 | uv254 > 0.250)) {
      warning("UV254 is outside the model bounds of 0.012 <= UV254 <= 0.250 cm-1 for coagulated water.")
    }

    # cl2_dose warnings
    if (treatment == "raw" & (cl2_dose < 0.995 | cl2_dose > 41.7)) {
      warning("Chlorine dose is outside the model bounds of 0.995 <= cl2_dose <= 41.7 mg/L for raw water.")
    }

    if (treatment == "coag" & (cl2_dose < 1.11 | cl2_dose > 24.7)) {
      warning("Chlorine dose is outside the model bounds of 1.11 <= cl2_dose <= 24.7 mg/L for coagulated water.")
    }

    # time warning
    if (time < 0.25 | time > 120) {
      warning("For chlorine decay estimate, reaction time is outside the model bounds of 0.25 <= time <= 120 hours.")
    }

    # get coefficients from defined clcoeffs table
    if (treatment == "raw") {
      coeffs <- subset(tidywater::cl2coeffs, treatment == "chlorine_raw")
    } else if (treatment == "coag") {
      coeffs <- subset(tidywater::cl2coeffs, treatment == "chlorine_coag")
    }

    # define function for chlorine decay
    # U.S. EPA (2001) equation 5-113 (raw) and equation 5-117 (coag)
    solve_decay <- function(ct, a, b, cl2_dose, uv254, time, c, toc) {
      a * cl2_dose * log(cl2_dose/ct) - b * (cl2_dose/uv254)^c *toc * time + cl2_dose - ct
    }

  #chloramine decay model
  } else if (cl_type == "chloramine") {

    # Chloramine code commented out until water slot added. Remove next line once added.
    warning("Chloramine calculations still under development.")


    # define function for chloramine decay
    #U.S. EPA (2001) equation 5-120
    # solve_decay <- function(ct, a, b, cl2_dose, uv254, time, c, toc) {
    #   a * cl2_dose * log(cl2_dose/ct) - b * uv254 * time + cl2_dose - ct
    # }
    #
    # coeffs <- subset(cl2coeffs, treatment == "chloramine")
  }

  # if dose is 0, do not run uniroot function
  if (cl2_dose == 0) {

    ct <- 0

  } else {

      root_ct <- stats::uniroot(solve_decay, interval = c(0, cl2_dose),
       a = coeffs$a,
       b = coeffs$b,
       c = coeffs$c,
       cl2_dose = cl2_dose,
       uv254 = uv254,
       toc = toc,
       time = time,
       tol = 1e-14)

      ct <- root_ct$root
  }

  # Convert final result to molar
  water@tot_ocl <- convert_units(ct, "ocl", "mg/L", "M")

  return(water)
}
