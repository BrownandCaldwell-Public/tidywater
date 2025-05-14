#' Modify a single slot in a `water` class object
#'
#' This function a single slot of a `water` class object without impacting the other parameters. For example, you can
#' manually update "tthm" and the new speciation will not be calculated. This function is designed to make sure all parameters
#' are stored in the correct units when manually updating a water. Some slots cannot be modified with this function because
#' they are interconnected with too many others (usually pH dependent, eg, hco3). For those parameters, update [define_water].
#'
#' @param water A water class object
#' @param slot A character string of the slot in the water to modify, eg, "tthm"
#' @param value New value for the modified slot
#' @param units Units of the value being entered, typically one of c("mg/L", "ug/L", "M", "cm-1"). For ions any units supported by [convert_units]
#' are allowed. For organic carbon, one of "mg/L", "ug/L". For uv254 one of "cm-1", "m-1". For DBPs, one of "ug/L" or "mg/L".
#'
#' @examples
#' water1 <- define_water(ph = 7, alk = 100, tds = 100, toc = 5)
#' modify_water(water1, slot = "toc", value = 4, units = "mg/L")
#'
#' @import dplyr
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

  tthmlist <-  c("chcl3", "chcl2br", "chbr2cl", "chbr3")
  haa5list <- c("mcaa", "dcaa", "tcaa", "mbaa", "dbaa")
  haa9list <- c("bcaa", "cdbaa" , "dcbaa", "baa")

  # Check lists
  if(slot %in% c("na", "ca", "mg", "k", "cl", "so4", "no3", "br", "bro3", "f", "fe", "al", "mn")) {
    new_value <- convert_units(value, slot, units, "M")
  } else if (slot %in% c("toc", "doc", "bdoc")) {
    if (units == "mg/L") {
      new_value <- value
    } else if(units == "ug/L") {
      new_value <- value * 10^3
    } else {
      stop(paste(slot, "must be specified in mg/L or ug/L"))
    }
  } else if (slot %in% c("uv254")) {
    if (units == "cm-1") {
      new_value <- value
    } else if (units == "m-1") {
      new_value <- value / 100
    } else {
      stop(paste(slot, "must be specified in cm-1 or m-1"))
    }
  } else if (slot %in% c(tthmlist, haa5list, haa9list, "tthm", "haa5")) {
    if (units == "ug/L") {
      new_value <- value
    } else if (units == "mg/L") {
      new_value <- value / 10^3
    } else {
      stop(paste(slot, "must be specified in ug/L or mg/L"))
    }

  } else {
    stop(paste(slot, "is not a supported slot for modify water. Check spelling or change using `define_water`."))
  }

  methods::slot(water, slot) <- new_value

}

