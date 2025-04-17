# Chlorine/Chloramine Decay Modeling functions
# These functions predict chlorine residual concentration given reaction time

#' @title Calculate chlorine decay
#'
#' @description calculates the decay of chlorine or chloramine based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001).
#' For a single water use `chemdose_chlordecay`; for a dataframe use `chemdose_chlordecay_chain`.
#' For most arguments in the `_chain` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details Required arguments include an object of class "water" created by [define_water],
#' applied chlorine/chloramine dose, type, reaction time, and treatment applied (options include "raw" for
#' no treatment, or "coag" for coagulated water). The function also requires additional water quality
#' parameters defined in [define_water] including TOC and UV254. The output is a new "water" class
#' with the calculated total chlorine value stored in the 'free_chlorine' or 'combined_chlorine' slot,
#' depending on what type of chlorine is dosed. When modeling residual concentrations
#' through a unit process, the U.S. EPA Water Treatment Plant Model applies a correction factor based on the
#' influent and effluent residual concentrations (see U.S. EPA (2001) equation 5-118) that may need to be
#' applied manually by the user based on the output.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.#'
#'
#' @source U.S. EPA (2001)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2_dose Applied chlorine or chloramine dose (mg/L as cl2). Model results are valid for doses between 0.995 and 41.7 mg/L for raw water,
#' and for doses between 1.11 and 24.7 mg/L for coagulated water.
#' @param time Reaction time (hours). Chlorine decay model results are valid for reaction times between 0.25 and 120 hours.Chloramine decay model
#' does not have specified boundary conditions.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default), "coag" for
#' water that has been coagulated or softened.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @param use_chlorine_slot Defaults to FALSE. When TRUE, uses either free_chlorine or combined_chlorine slot in water (depending on cl_type).
#' If 'cl2_dose' argument, not specified, chlorine slot will be used. If 'cl2_dose' specified and use_chlorine_slot is TRUE,
#' all chlorine will be summed.
#' @examples
#' example_cl2 <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = 0.2)) %>%
#'   chemdose_chlordecay(cl2_dose = 2, time = 8)
#'
#' example_cl2 <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = 0.2, free_chlorine = 3) %>%
#'   chemdose_chlordecay(cl2_dose = 2, time = 8, use_chlorine_slot = TRUE))
#'
#' @export
#' @returns `chemdose_chlordecay` returns an updated disinfectant residual in the free_chlorine or combined_chlorine water slot in units of M.
#' Use [convert_units] to convert to mg/L.
#'
chemdose_chlordecay <- function(water, cl2_dose, time, treatment = "raw", cl_type = "chlorine", use_chlorine_slot = FALSE) {


  if (use_chlorine_slot & cl_type == "chlorine") {
    validate_water(water, c("toc", "uv254", "free_chlorine"))
  } else if (use_chlorine_slot & cl_type == "chloramine") {
    validate_water(water, c("toc", "uv254", "combined_chlorine"))
  } else {
    validate_water(water, c("toc", "uv254"))
  }


  toc <- water@toc
  uv254 <- water@uv254

  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (missing(cl2_dose)) {

    if (!use_chlorine_slot) {
      stop("Missing value for chlorine dose. Please check the function inputs required to calculate chlorine/chloramine decay.")

    } else if (use_chlorine_slot) {

      if (cl_type == "chlorine") {
        cl2_dose <- convert_units(water@free_chlorine, "cl", "M", "mg/L")
        warning("Missing value for chlorine dose. Function used free_chlorine water slot instead.")

      } else if (cl_type == "chloramine") {
        cl2_dose <- convert_units(water@combined_chlorine, "cl", "M", "mg/L")
        warning("Missing value for chlorine dose. Function used combined_chlorine water slot instead.")
      }
    }
  } else if (cl2_dose > 0 & use_chlorine_slot) {
    if (cl_type == "chlorine") {
      cl2_dose <- cl2_dose + convert_units(water@free_chlorine, "cl", "M", "mg/L")
      warning("Function summed both cl2_dose and free_chlorine water slot for input chlorine dose.")

    } else if (cl_type == "chloramine") {
      cl2_dose <- cl2_dose + convert_units(water@combined_chlorine, "cl", "M", "mg/L")
      warning("Function summed both cl2_dose and combined_chlorine water slot for input chlorine dose.")
    }
  }

  if (missing(time)) {
    stop("Missing value for reaction time. Please check the function inputs required to calculate chlorine/chloramine decay.")
  }

  if (!(cl_type %in% c("chlorine", "chloramine"))) {
    stop("cl_type should be 'chlorine' or 'chloramine'. Please check the spelling for cl_type to calculate chlorine/chloramine decay.")
  }

  # breakpoint warning
  if (water@tot_nh3 > 0) {
    warning("Background ammonia present, chloramines may form.\nUse chemdose_chloramine for breakpoint caclulations.")
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
      a * cl2_dose * log(cl2_dose / ct) - b * (cl2_dose / uv254)^c * toc * time + cl2_dose - ct
    }

    # chloramine decay model
  } else if (cl_type == "chloramine") {
    # define function for chloramine decay
    # U.S. EPA (2001) equation 5-120
    solve_decay <- function(ct, a, b, cl2_dose, uv254, time, c, toc) {
      a * cl2_dose * log(cl2_dose / ct) - b * uv254 * time + cl2_dose - ct
    }

    coeffs <- subset(tidywater::cl2coeffs, treatment == "chloramine")
  }

  # if dose is 0, do not run uniroot function
  if (cl2_dose == 0) {
    ct <- 0
  } else {
    root_ct <- stats::uniroot(solve_decay,
      interval = c(0, cl2_dose),
      a = coeffs$a,
      b = coeffs$b,
      c = coeffs$c,
      cl2_dose = cl2_dose,
      uv254 = uv254,
      toc = toc,
      time = time,
      tol = 1e-14
    )

    ct <- root_ct$root
  }

  # Convert final result to molar
  if (cl_type == "chlorine") {
    # chlorine residual correction Eq. 5-118
    ct_corrected <- cl2_dose + (ct - cl2_dose) / 0.85
    water@free_chlorine <- convert_units(ct_corrected, "cl2", "mg/L", "M")
  } else if (cl_type == "chloramine") {
    water@combined_chlorine <- convert_units(ct, "cl2", "mg/L", "M")
  }

  return(water)
}


#' @rdname chemdose_chlordecay
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_once]. The df may include a column named for the applied chlorine dose (cl2),
#' and a column for time in hours.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "disinfected_water".
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_chlordecay_chain(input_water = "balanced_water", cl2_dose = 4, time = 8)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50,
#'          free_chlorine = 2) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     cl2_dose = seq(2, 24, 2),
#'     ClTime = 30
#'   ) %>%
#'   chemdose_chlordecay_chain(input_water = "balanced_water", time = ClTime, use_chlorine_slot = TRUE)
#'
#' example_df <- water_df %>%
#'   mutate(br = 80) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(time = 8) %>%
#'   chemdose_chlordecay_chain(
#'     input_water = "balanced_water", cl2_dose = 6, treatment = "coag",
#'     cl_type = "chloramine"
#'   )
#'
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_chlordecay_chain(input_water = "balanced_water", cl2_dose = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns `chemdose_chlordecay_chain` returns a data frame containing a water class column with updated chlorine residuals.

chemdose_chlordecay_chain <- function(df, input_water = "defined_water", output_water = "disinfected_water",
                                      cl2_dose = "use_col", time = "use_col",
                                      treatment = "use_col", cl_type = "use_col",
                                      use_chlorine_slot = "use_col") {
  # This allows for the function to process unquoted column names without erroring
  cl2_dose <- tryCatch(cl2_dose, error = function(e) enquo(cl2_dose))
  time <- tryCatch(time, error = function(e) enquo(time))
  treatment <- tryCatch(treatment, error = function(e) enquo(treatment))
  cl_type <- tryCatch(cl_type, error = function(e) enquo(cl_type))
  use_chlorine_slot <- tryCatch(use_chlorine_slot, error = function(e) enquo(use_chlorine_slot))

  validate_water_helpers(df, input_water)

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, list(
    "cl2_dose" = cl2_dose, "time" = time,
    "treatment" = treatment, "cl_type" = cl_type, "use_chlorine_slot" = use_chlorine_slot
  ))

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        cl2_dose = !!as.name(arguments$final_names$cl2_dose),
        time = !!as.name(arguments$final_names$time),
        treatment = if (arguments$final_names$treatment %in% names(.)) !!sym(arguments$final_names$treatment) else rep("raw", nrow(.)),
        cl_type = if (arguments$final_names$cl_type %in% names(.)) !!sym(arguments$final_names$cl_type) else rep("chlorine", nrow(.)),
        use_chlorine_slot = if (arguments$final_names$use_chlorine_slot %in% names(.)) !!sym(arguments$final_names$use_chlorine_slot) else rep(FALSE, nrow(.))
      ),
      chemdose_chlordecay
    ))
}
