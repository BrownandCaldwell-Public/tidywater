#'@title Inorganic Oxidant Demand
#'
#' @description Calculates Cl residual from inorganic scavengers present in the input water object: Mg, Fe, and Ammonia. The output water will have updated `free_chlorine` slot
#' to reflect the chlorine residual after accounting for the inorganic demand. The `mn`, `fe`, and `tot_nh3` slots will be set to 0, as they are assumed to be fully oxidized by the chlorine dose.
#' Other products of the oxidation reactions will not be tracked or updated in the output water object.
#' For a single water use `chemdose_inorgdemand`; to apply the model to a dataframe use `chemdose_inorgdemand_df`.
#'
#' By default (`use_chlorine_slot = FALSE`), the output `free_chlorine` is `cl2_dose` minus the inorganic demand, and
#' any free chlorine already in the water is overwritten (with a warning). Set `use_chlorine_slot = TRUE` to add the
#' existing `free_chlorine` slot to `cl2_dose` before the demand is subtracted. If `cl2_dose` is not specified, the existing
#' `free_chlorine` slot is used (with a warning when `use_chlorine_slot = FALSE`). The function errors if the inorganic
#' demand is greater than the available chlorine.
#'
#' The inorganic demand of ammonia is calculated assuming all ammonia is converted to nitrogen gas, not considering the conversion of ammonia to nitrate. For chloramine chemistry, use `chemdose_chloramine`
#' if interested in more precise breakpoint chlorination calculations.
#'
#' @param water Input water object with mn, fe, and tot_nh3 slots defined. The free_chlorine slot must also be defined
#' when the slot is used (`use_chlorine_slot = TRUE` or `cl2_dose` not specified).
#' @param cl2_dose Chlorine dose in mg/L Cl2.
#' @param use_chlorine_slot Defaults to FALSE. When TRUE, the existing free_chlorine slot in the water is added to
#' `cl2_dose`. If `cl2_dose` is not specified, only the chlorine slot will be used, regardless of use_chlorine_slot.
#'
#' @examples
#'
#' water1<-define_water(ph = 7, alk = 70, mn = 1, fe = 2, tot_nh3 = 1)
#' cl_residual <- chemdose_inorgdemand(water1, 20)
#'
#' # Use the existing free chlorine in the water
#' water2 <- define_water(ph = 7, alk = 70, mn = 1, fe = 2, tot_nh3 = 1, free_chlorine = 12)
#' cl_residual <- chemdose_inorgdemand(water2, use_chlorine_slot = TRUE)
#'
#' @export
#'
#' @returns `chemdose_inorgdemand` returns a water class object with updated free_chlorine slot that reflects the chlorine residual.
#'

chemdose_inorgdemand <- function(water, cl2_dose, use_chlorine_slot = FALSE) {
  if (!is.logical(use_chlorine_slot)) {
    cli::cli_abort("{.arg use_chlorine_slot} must be {.code TRUE} or {.code FALSE}.")
  }

  if (missing(cl2_dose)) {
    if (!use_chlorine_slot) {
      tidywater_warn("{.arg cl2_dose} not specified, so the existing {.field free_chlorine} slot was used. Specify {.arg cl2_dose} or set {.code use_chlorine_slot = TRUE} to silence this warning.")
    }
    cl2_dose <- 0
    use_chlorine_slot <- TRUE
  }

  if (use_chlorine_slot) {
    validate_water(water, c("mn", "fe", "tot_nh3", "free_chlorine"))
  } else {
    validate_water(water, c("mn", "fe", "tot_nh3"))
  }

  if (!use_chlorine_slot & !is.na(water@free_chlorine) & water@free_chlorine > 0) {
    tidywater_warn("Existing {.field free_chlorine} slot will be overridden based on recent dose. To sum results instead, set {.code use_chlorine_slot = TRUE}.")
  }

    # convert each species to mg/L
    mn  <- convert_units(water@mn, "mn", "M", "mg/L")
    fe  <- convert_units(water@fe, "fe", "M", "mg/L")
    nh3 <- convert_units(water@tot_nh3, "n", "M", "mg/L") # tot nh3 stored as N
    free_cl <- if (use_chlorine_slot) convert_units(water@free_chlorine, "cl2", "M", "mg/L") else 0

    mn_demands <- mn * 1.29
    fe_demands <- fe * 0.64
    nh3_demands <- nh3 * 7.6
    demand <- mn_demands + fe_demands + nh3_demands
    available_cl <- free_cl + cl2_dose
    residual <- available_cl - demand

    if (residual < 0) {
      available_msg <- if (use_chlorine_slot) {
        "Available chlorine is {.arg cl2_dose} plus the existing {.field free_chlorine} in {.arg water}."
      } else {
        "Available chlorine is {.arg cl2_dose}. Set {.code use_chlorine_slot = TRUE} to include the existing {.field free_chlorine} in {.arg water}."
      }
      cli::cli_abort(c(
        "Inorganic demand is greater than the available chlorine.",
        "x" = "Demand from Mn, Fe, and ammonia is {signif(demand, 3)} mg/L as Cl2, but only {signif(available_cl, 3)} mg/L as Cl2 is available.",
        "i" = available_msg,
        "i" = "Increase {.arg cl2_dose} to at least {signif(demand - free_cl, 3)} mg/L as Cl2."
      ))
    } else {
      output_water <- water
      output_water@free_chlorine <- convert_units(residual, "cl2")
      output_water@mn <- 0
      output_water@fe <- 0
      output_water@tot_nh3 <- 0

      return(output_water)
    }
}

#' @rdname chemdose_inorgdemand
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include columns for the chlorine dose (cl2_dose) and use_chlorine_slot.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "oxidized".
#' @param pluck_cols Extract the free_chlorine slot into a new numeric column for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#' @param keep_dose_cols Option to keep the original chemical dose columns in the output data frame. Default is FALSE.
#'
#' @examples
#' \donttest{
#' example_df <- water_df %>%
#'   dplyr::mutate(mn = 20, fe = 0.5, tot_nh3 = 0.2) %>%
#'   define_water_df() %>%
#'   chemdose_inorgdemand_df(cl2_dose = 5, pluck_cols = TRUE)
#'
#' example_df <- water_df %>%
#'   dplyr::mutate(mn = 20, fe = 0.5, tot_nh3 = 0.2) %>%
#'   define_water_df() %>%
#'   dplyr::mutate(ClDose = seq(3, 14, 1)) %>%
#'   chemdose_inorgdemand_df(cl2_dose = ClDose, output_water = "chlorinated")
#'
#' # Use the free_chlorine already in each water instead of a dose
#' example_df <- water_df %>%
#'   dplyr::mutate(mn = 20, fe = 0.5, tot_nh3 = 0.2, free_chlorine = 4) %>%
#'   define_water_df() %>%
#'   chemdose_inorgdemand_df(use_chlorine_slot = TRUE, pluck_cols = TRUE)
#' }
#'
#' @export
#'
#' @returns `chemdose_inorgdemand_df` returns a data frame containing a water class column with updated free_chlorine,
#' mn, fe, and tot_nh3 slots. Optionally, it also adds a column for free_chlorine.

chemdose_inorgdemand_df <- function(
  df,
  input_water = "defined",
  output_water = "oxidized",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  keep_dose_cols = FALSE,
  cl2_dose = "use_col",
  use_chlorine_slot = "use_col"
) {
  # This allows for the function to process unquoted column names without erroring
  cl2_dose <- tryCatch(cl2_dose, error = function(e) enquo(cl2_dose))
  use_chlorine_slot <- tryCatch(use_chlorine_slot, error = function(e) enquo(use_chlorine_slot))

  validate_water_helpers(df, input_water)

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, list("cl2_dose" = cl2_dose, "use_chlorine_slot" = use_chlorine_slot))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(df, final_names, list(use_chlorine_slot = FALSE))
  df <- defaults_added$data

  # cl2_dose may be omitted to use the chlorine slot; let the base function handle the missing dose
  has_dose <- final_names$cl2_dose %in% names(df)

  warning_counts <- list()

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    withCallingHandlers(
      if (has_dose) {
        chemdose_inorgdemand(
          water = df[[input_water]][[i]],
          cl2_dose = df[[final_names$cl2_dose]][i],
          use_chlorine_slot = df[[final_names$use_chlorine_slot]][i]
        )
      } else {
        chemdose_inorgdemand(
          water = df[[input_water]][[i]],
          use_chlorine_slot = df[[final_names$use_chlorine_slot]][i]
        )
      },
      warning = function(w) {
        msg <- conditionMessage(w)
        warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) + 1L
        invokeRestart("muffleWarning")
      }
    )
  })

  for (msg in names(warning_counts)) {
    tidywater_warn("{msg} ({warning_counts[[msg]]} row{?s} affected.)")
  }
  output <- df[, !names(df) %in% defaults_added$defaults_used]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("free_chlorine"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  if (!keep_dose_cols) {
    output <- output[, !colnames(output) %in% final_names$cl2_dose]
  }

  return(output)
}
