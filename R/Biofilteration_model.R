# TOC models

#' @title Determine TOC removal using BDOC model
#'
#' @description This function calculates TOC removal based on BDOC removal using different rate constants for various conditions.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}.
#' @param EBCT Empty Bed Contact Time in minutes
#' @param temp Temperature in °C
#' @param ozonated Logical; TRUE if the water is ozonated, FALSE otherwise
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1)
#' dosed_water <- chemdose_bdoc(water, EBCT = 10, temp = 15, ozonated = TRUE)
#'
#' @export
#'
chemdose_bdoc <- function(water, EBCT, temp, ozonated) {
  
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }
  
  if (is.na(water@toc)) {
    stop("Water is missing a TOC parameter. Make sure TOC is specified.")
  }
  
  # Define BDOC fractions
  BDOC_fraction_nonozonated <- 0.2
  BDOC_fraction_ozonated <- 0.3
  
  # Determine BDOC fraction and rate constant k' based on temperature and ozonation
  if (ozonated) {
    if (temp <= 10) {
      k <- 0.03
      BDOC_fraction <- BDOC_fraction_ozonated
    } else if (temp <= 20) {
      k <- 0.06
      BDOC_fraction <- BDOC_fraction_ozonated
    } else {
      k <- 0.15
      BDOC_fraction <- BDOC_fraction_ozonated
    }
  } else {
    if (temp <= 10) {
      k <- 0.03
      BDOC_fraction <- BDOC_fraction_nonozonated
    } else if (temp <= 20) {
      k <- 0.09
      BDOC_fraction <- BDOC_fraction_nonozonated
    } else {
      k <- 0.11
      BDOC_fraction <- BDOC_fraction_nonozonated
    }
  }
  
  # Calculate BDOC influent concentration
  BDOC_inf <- BDOC_fraction * water@toc
  
  # Calculate BDOC effluent concentration using the exponential decay model
  BDOC_eff <- BDOC_inf * exp(-k * EBCT)
  
  # Calculate TOC removal percentage
  TOC_removal <- (BDOC_inf - BDOC_eff) / water@toc
  
  # Update water object with new TOC and DOC values
  water@toc <- water@toc * (1 - TOC_removal)
  water@doc <- water@toc * BDOC_fraction
  
  # Append treatment description
  water@treatment <- paste(water@treatment, "_bdocremoved", sep = "")
  
  return(water)
}

# SUVA calculation function remains unchanged
calc_suva <- function(doc, uv254) {
  uv254 / doc * 100
}

water <- define_water(ph = 7, temp = 15, alk = 100, toc = 5.0, doc = 4.0, uv254 = 0.1)

dosed_water <- chemdose_bdoc(water, EBCT = 10, temp = 15, ozonated = TRUE)

print(dosed_water)

