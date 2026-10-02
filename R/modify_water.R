#' Modify slots in a `water` class object
#'
#' This function modifies selected slots of a `water` class object without impacting the other parameters. For example, you can
#' manually update "tthm" and the new speciation will not be calculated. This function is designed to make sure all parameters
#' are stored in the correct units when manually updating a water. Some slots cannot be modified with this function because
#' they are interconnected with too many others (usually pH dependent, eg, hco3). For those parameters, update [define_water].
#'
#' @param water A water class object
#' @param slot A vector of slots in the water to modify, eg, "tthm"
#' @param value A vector of new values for the modified slots
#' @param units A vector of units for each value being entered, typically one of c("mg/L", "ug/L", "M", "cm-1"). For ions any units supported by [convert_units]
#' are allowed. For organic carbon, one of "mg/L", "ug/L". For uv254 one of "cm-1", "m-1". For DBPs, one of "ug/L" or "mg/L".
#'
#' @examples
#' water1 <- define_water(ph = 7, alk = 100, tds = 100, toc = 5) %>%
#'   modify_water(slot = "toc", value = 4, units = "mg/L")
#'
#' water2 <- define_water(ph = 7, alk = 100, tds = 100, toc = 5, ca = 10) %>%
#'   modify_water(slot = c("ca", "toc"), value = c(20, 10), units = c("mg/L", "mg/L"))
#'
#' @export
#' @returns A data frame containing columns of selected parameters from a list of water class objects.

modify_water <- function(water, slot, value, units) {
  # Make sure a water is present.
  if (missing(water)) {
    cli::cli_abort("No source water defined. Create a water using {.fn define_water}.")
  }
  if (!methods::is(water, "water")) {
    cli::cli_abort("Input water must be of class {.cls water}. Create a water using {.fn define_water}.")
  }

  tthmlist <- c("chcl3", "chcl2br", "chbr2cl", "chbr3")
  haa5list <- c("mcaa", "dcaa", "tcaa", "mbaa", "dbaa")
  haa9list <- c("bcaa", "cdbaa", "dcbaa", "tbaa")

  if (missing(units)) {
    cli::cli_abort("{.arg units} missing. Typical units include {.val mg/L}, {.val ug/L}, or {.val M}.")
  }

  for (i in seq_along(slot)) {
    slot_n <- slot[i]
    value_n <- value[i]
    units_n <- units[i]

    if (!is.numeric(value_n)) {
      cli::cli_abort("{.arg value} must be numeric.")
    }

    # Check lists
    if (slot_n %in% c("na", "ca", "mg", "k", "cl", "so4", "no3", "no2", "br", "bro3", "f", "fe", "al", "mn")) {
      new_value <- convert_units(value_n, slot_n, units_n, "M")
    } else if (slot_n %in% c("toc", "doc", "bdoc")) {
      if (units_n == "mg/L") {
        new_value <- value_n
      } else if (units_n == "ug/L") {
        new_value <- value_n / 10^3
      } else {
        cli::cli_abort("{slot_n} must be specified in {.val mg/L} or {.val ug/L}.")
      }
    } else if (slot_n %in% c("uv254")) {
      if (units_n == "cm-1") {
        new_value <- value_n
      } else if (units_n == "m-1") {
        new_value <- value_n / 100
      } else {
        cli::cli_abort("{slot_n} must be specified in {.val cm-1} or {.val m-1}.")
      }
    } else if (slot_n %in% c(tthmlist, haa5list, haa9list, "tthm", "haa5")) {
      if (units_n == "ug/L") {
        new_value <- value_n
      } else if (units_n == "mg/L") {
        new_value <- value_n * 10^3
      } else {
        cli::cli_abort("{slot_n} must be specified in {.val ug/L} or {.val mg/L}.")
      }
    } else {
      cli::cli_abort("{slot_n} is not a supported slot for {.fn modify_water}. Check spelling or use {.fn define_water}.")
    }
    methods::slot(water, slot_n) <- new_value
  }

  return(water)
}

#' @rdname modify_water
#' @param df a data frame containing a water class column, which has already been computed using [define_water_df]
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "modified".
#'
#' @details In `modify_water_df`, `slot`, `value`, and `units` can each be a constant, an unquoted column name, or left as
#' "use_col" to read a column of the same name ("slot", "value", "units"). Constants are applied to every row as
#' given, so `slot = c("br", "na")`, `value = c(50, 60)`, `units = c("ug/L", "mg/L")` sets both slots on each water.
#' Unlike other `_df` functions, vector constants are not expanded into one row per combination. Columns are read row by row.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(bromide = 50) %>%
#'   modify_water_df(slot = "br", value = bromide, units = "ug/L")
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   modify_water_df(
#'     slot = c("br", "na"),
#'     value = c(50, 60),
#'     units = c("ug/L", "mg/L")
#'   )
#'
#' @export
#'
#' @returns `modify_water_df` returns a data frame containing a water class column with updated slot

modify_water_df <- function(
  df,
  input_water = "defined",
  output_water = "modified",
  slot = "use_col",
  value = "use_col",
  units = "use_col"
) {
  validate_water_helpers(df, input_water)

  slot <- tryCatch(slot, error = function(e) enquo(slot))
  value <- tryCatch(value, error = function(e) enquo(value))
  units <- tryCatch(units, error = function(e) enquo(units))

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list("slot" = slot, "value" = value, "units" = units))
  final_names <- arguments$final_names
  # arguments$new_cols is deliberately unused: merging that expand.grid would split vector constants across rows.

  # Constants apply to every row as given (not expanded into a grid), so vectors modify several slots at once.
  all_args <- list("slot" = slot, "value" = value, "units" = units)
  constants <- Filter(function(x) !inherits(x, "quosure") && !identical(x, "use_col"), all_args)
  missing_cols <- setdiff(unlist(final_names[setdiff(names(all_args), names(constants))]), names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Column{?s} {.val {missing_cols}} not found in {.arg df}. Pass the argument directly or add the column.")
  }
  row_arg <- function(arg, i) {
    if (arg %in% names(constants)) constants[[arg]] else df[[final_names[[arg]]]][[i]]
  }

warning_counts <- list()

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    withCallingHandlers(
      modify_water(
        water = df[[input_water]][[i]],
        slot = row_arg("slot", i),
        value = row_arg("value", i),
        units = row_arg("units", i)
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) +1L
        invokeRestart("muffleWarning")
      }
    )
  })
for (msg in names(warning_counts)) {
    tidywater_warn("{msg} ({warning_counts[[msg]]} row{?s} affected.)")
  }
  output <- df[, !names(df) %in% c("slot", "value", "units"), drop = FALSE]

  

  return(output)
}
