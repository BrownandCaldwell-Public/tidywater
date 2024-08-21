
#' @title Determine TOC removal from biofiltration
#'
#' @description This function applies the XX model to a water created by \code{\link{define_water}} to determine biofiltered
#' DOC (mg/L).
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph, doc, and uv254
#' @param ebct The empty bed contact time (min) used for the biofilter
#' @param o3_dose Applied ozone (O3) dose in mg/L. Defaults to 0 mg/L.
#' @param media_type Biofilter media. Choose from c("GAC", "sand", "anthracite"). Defaults to GAC
#'
#' @source XXX
#'
#' @examples
#' example1 <- define_water(ph = 7, temp = 15, alk = 100, toc = 4, doc = 3.8, uv254 = .1) %>%
#'   biofilter_toc(ebct = 20, media_type = "GAC")
#'
#' example2 <- define_water(ph = 7, temp = 15, alk = 100, toc = 4, doc = 3.8, uv254 = .1) %>%
#'   biofilter_toc(ebct = 20, o3_dose = 3, media_type = "sand")
#'
#' @export
#'
biofilter_toc <- function(water, ebct, o3_dose = 0, media_type = "GAC") {
  temperature <- water@temp
  o3_toc_ratio <- o3_dose/water@toc

  # Check if water object is missing or not of class 'water'
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!inherits(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }

  # Define the model parameters
  intercept <- 0.132
  media_coefficients <- list(GAC = 0.087, sand = 0.074, anthracite = 0.065)
  o3_toc_coefficient <- 0.053
  ebct_coefficient <- 0.039
  ebct_o3_toc_coefficient <- 0.014

  # Get the media coefficient
  if (!media_type %in% names(media_coefficients)) {
    stop("Invalid media type. Choose from 'GAC', 'sand', or 'anthracite'.")
  }
  media_coefficient <- media_coefficients[[media_type]]

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

  # Calculate TOC removal using the model
  if (o3_dose == 0) {
    tocremoval <- intercept + media_coefficient + (ebct_coefficient * ebct)
  } else if (o3_dose >0) {
    tocremoval <- intercept + media_coefficient + (o3_toc_coefficient * o3_toc_ratio) +
      (ebct_coefficient * ebct) + (ebct_o3_toc_coefficient * ebct * o3_toc_ratio)
  }
  # Adjust water TOC based on the calculated TOC removal and c/c_inf ratio
  if (!is.na(water@toc) & water@toc >= water@doc) {
    water@toc <- water@toc - (water@toc * tocremoval / 100) * c_cinf_ratio
  } else if (!is.na(water@toc) & water@toc < water@doc) {
    warning("TOC of input water less than DOC. TOC will be set equal to DOC.")
    water@toc <- water@doc - (water@doc * tocremoval / 100) * c_cinf_ratio
  } else if (is.na(water@toc)) {
    warning("Input water TOC not specified. Output water TOC will be NA.")
    water@toc <- NA_real_
  }

  water@doc <- water@toc  # Assuming DOC is a subset of TOC in this simplified model
  water@treatment <- paste(water@treatment, "_biofilter", sep = "")
  return(water)
}
