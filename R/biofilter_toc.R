#' @title Determine TOC removal using Terry & Summers BDOC model
#'
#' @description This function applies the Terry model to a water created by \code{\link{define_water}} to determine biofiltered
#' DOC (mg/L).
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}.
#' @param ebct The empty bed contact time (min) used for the biofilter
#' @param ozonated Logical; TRUE if the water is ozonated (default), FALSE otherwise
#'
#' @source Terry and Summers 2018
#' @examples
#' library(tidywater)
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1) %>%
#'   biofilter_toc(ebct = 10, ozonated = TRUE)
#'
#' @returns  A water class object with modeled DOC removal from biofiltration.
#' @export
#'
biofilter_toc <- function(water, ebct, ozonated = TRUE) {

  if (!is.logical(ozonated)) {
    warning("Ozonated argument must be logical. Calculation will be skipped for this instance.")
  }

  temp = water@temp

  validate_water(water, "toc")

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
  BDOC_eff <- BDOC_inf * exp(-k * ebct)

  # Calculate TOC removal percentage
  BDOC_removed <- (BDOC_inf - BDOC_eff)

  # Update water object with new TOC and DOC values
  water@toc <- water@toc - BDOC_removed
  water@doc <- water@toc - BDOC_removed
  water@bdoc <- BDOC_eff
  water@applied_treatment <- paste(water@applied_treatment, "_biofilter", sep = "")

  return(water)
}
