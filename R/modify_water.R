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
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using define_water.")
  }

  tthmlist <- c("chcl3", "chcl2br", "chbr2cl", "chbr3")
  haa5list <- c("mcaa", "dcaa", "tcaa", "mbaa", "dbaa")
  haa9list <- c("bcaa", "cdbaa", "dcbaa", "baa")

  if (missing(units)) {
    stop("Units missing. Typical units include: 'mg/L', 'ug/L', 'M'")
  }

  for (i in seq_along(slot)) {
    slot_n <- slot[i]
    value_n <- value[i]
    units_n <- units[i]

    if (!is.numeric(value_n)) {
      stop("value must be numeric")
    }

    # Check lists
    if (slot_n %in% c("na", "ca", "mg", "k", "cl", "so4", "no3", "br", "bro3", "f", "fe", "al", "mn")) {
      new_value <- convert_units(value_n, slot_n, units_n, "M")
    } else if (slot_n %in% c("toc", "doc", "bdoc")) {
      if (units_n == "mg/L") {
        new_value <- value_n
      } else if (units_n == "ug/L") {
        new_value <- value_n * 10^3
      } else {
        stop(paste(slot_n, "must be specified in mg/L or ug/L"))
      }
    } else if (slot_n %in% c("uv254")) {
      if (units_n == "cm-1") {
        new_value <- value_n
      } else if (units_n == "m-1") {
        new_value <- value_n / 100
      } else {
        stop(paste(slot_n, "must be specified in cm-1 or m-1"))
      }
    } else if (slot_n %in% c(tthmlist, haa5list, haa9list, "tthm", "haa5")) {
      if (units_n == "ug/L") {
        new_value <- value_n
      } else if (units_n == "mg/L") {
        new_value <- value_n / 10^3
      } else {
        stop(paste(slot_n, "must be specified in ug/L or mg/L"))
      }
    } else {
      stop(paste(slot_n, "is not a supported slot for modify water. Check spelling or change using `define_water`."))
    }
    methods::slot(water, slot_n) <- new_value
  }

  return(water)
}

#' @rdname modify_water
#' @param df a data frame containing a water class column, which has already been computed using [define_water_df]
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "modified_water".
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(bromide = 50) %>%
#'   modify_water_df(slot = "br", value = bromide, units = "ug/L")
#'
#' multislot_example_df1 <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(
#'     slot = list(c("br", "na")),
#'     value = list(c(50, 60)),
#'     units = list(c("ug/L", "mg/L"))
#'   ) %>%
#'   modify_water_df()
#'
#' multislot_example_df2 <- water_df %>%
#'   define_water_df() %>%
#'   modify_water_df(
#'     slot = list(c("br", "na")),
#'     value = list(c(50, 60)),
#'     units = list(c("ug/L", "mg/L"))
#'   )
#'
#' @export
#'
#' @returns `modify_water_df` returns a data frame containing a water class column with updated slot

modify_water_df <- function(df, input_water = "defined", output_water = "modified",
                            slot = "use_col", value = "use_col", units = "use_col") {
  validate_water_helpers(df, input_water)

  slot <- tryCatch(slot, error = function(e) enquo(slot))
  value <- tryCatch(value, error = function(e) enquo(value))
  units <- tryCatch(units, error = function(e) enquo(units))

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list("slot" = slot, "value" = value, "units" = units))
  final_names <- arguments$final_names

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    modify_water(
      water = df[[input_water]][[i]],
      slot = df[[final_names$slot]][[i]],
      value = df[[final_names$value]][[i]],
      units = df[[final_names$units]][[i]]
    )
  })

  output <- df[, !names(df) %in% c("slot", "value", "units"), drop = FALSE]
  return(output)
}
