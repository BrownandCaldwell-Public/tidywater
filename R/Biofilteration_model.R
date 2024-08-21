
#' @title Determine TOC removal from biofiltration
#'
#' @description This function applies the Terry model to a water created by \code{\link{define_water}} to determine biofiltered
#' DOC (mg/L).
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph, doc, and uv254
#' @param ebct The empty bed contact time (min) used for the biofilter
#' @param o3_dose Applied ozone (O3) dose in mg/L. Defaults to 0 mg/L.
#'
#' @source Terry and Summers 2018
#'
#' @examples
#' example1 <- define_water(ph = 7, temp = 15, alk = 100, toc = 4, doc = 3.8, uv254 = .1) %>%
#'   biofilter_toc(ebct = 20)
#'
#' example2 <- define_water(ph = 7, temp = 15, alk = 100, toc = 4, doc = 3.8, uv254 = .1) %>%
#'   biofilter_toc(ebct = 20, o3_dose = 3)
#'
#' @export
#'
biofilter_toc <- function(water, ebct, o3_dose = 0) {
  temperature <- water@temp

  # Check if water object is missing or not of class 'water'
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!inherits(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }

  # Determine the median k' value based on temperature and oxidation condition
  if (o3_dose == 0) {
    if (temperature < 10) {
      k_prime <- 0.05
    } else if (temperature <= 20) {
      k_prime <- 0.09
    } else {
      k_prime <- 0.11
    }
  } else if (o3_dose > 0) {
    if (temperature < 10) {
      k_prime <- 0.04
    } else if (temperature <= 20) {
      k_prime <- 0.09
    } else {
      k_prime <- 0.15
    }
  }
  # Calculate the ratio c/c_inf using the pseudo-first-order model
  c_cinf_ratio <- exp(-k_prime * ebct)

  # Adjust water TOC based on the calculated TOC removal and c/c_inf ratio
  if (!is.na(water@toc)) {
    water@toc <- water@toc * c_cinf_ratio
  } else if (is.na(water@toc)) {
    warning("Input water TOC not specified. Output water TOC will be NA.")
  }

  water@doc <- water@doc * c_cinf_ratio
  water@applied_treatment <- paste(water@applied_treatment, "_biofilter", sep = "")
  return(water)
}
