# TOC models


#' @title Determine TOC removal from coagulation
#'
#' @description This function applies the Edwards (1997) model to a water created by \code{\link{define_water}} to determine coagulated
#' DOC. Coagulated UVA is from U.S. EPA (2001) equation 5-80. Note that the models rely on pH of coagulation. If
#' only raw water pH is known, utilize \code{\link{chemdose_ph}} first.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph, doc, and uv254
#' @param alum Amount of hydrated aluminum sulfate added in mg/L: Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param fecl3 Amount of ferric chloride added in mg/L: FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param fe2so43 Amount of ferric sulfate added in mg/L: Fe2(SO4)3 + 6HCO3 -> 2Fe(OH)3(am) +3SO4 + 6CO2
#' @param coeff String specifying the Edwards coefficients to be used from "Alum", "Ferric", "General Alum", "General Ferric", or "Low DOC" or
#' named vector of coefficients, which must include: k1, k2, x1, x2, x3, b
#'
#' @seealso \code{\link{chemdose_ph}}
#'
#' @source Edwards (1997)
#' @source U.S. EPA (2001)
#' @source See reference list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 3.7, doc = 3.5, uv254 = .1)
#' dosed_water <- chemdose_ph(water, alum = 30) %>%
#'   chemdose_toc(alum = 30, coeff = "Alum")
#'
#' dosed_water <- chemdose_ph(water, fe2so43 = 30) %>%
#'   chemdose_toc(fe2so43 = 30, coeff = "Ferric")
#'
#' dosed_water <- chemdose_ph(water, alum = 10, h2so4 = 10) %>%
#'   chemdose_toc(alum = 10, coeff = c("x1" = 280, "x2" = -73.9, "x3" = 4.96, "k1" = -0.028, "k2" = 0.23, "b" = 0.068))
#'
#' @export

#'Define the Water Object Function
# define_water <- function(ph, temp, alk, toc, doc, uv254) {
#   structure(list(ph = ph, temp = temp, alk = alk, toc = toc, doc = doc, uv254 = uv254, treatment = ""), class = "water")
# }

# Biofilter TOC Function
biofilter_toc <- function(water, ebct, temperature, media_type = "GAC", oxidation_condition = "non-ozonated", o3_toc_ratio = NA) {
  
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
  if (oxidation_condition == "non-ozonated") {
    if (temperature < 10) {
      k_prime <- 0.05
    } else if (temperature <= 20) {
      k_prime <- 0.09
    } else {
      k_prime <- 0.11
    }
  } else if (oxidation_condition == "ozonated") {
    if (temperature < 10) {
      k_prime <- 0.04
    } else if (temperature <= 20) {
      k_prime <- 0.09
    } else {
      k_prime <- 0.15
    }
  } else {
    stop("Invalid oxidation condition. Choose from 'non-ozonated' or 'ozonated'.")
  }
  
  # Calculate the ratio c/c_inf using the pseudo-first-order model
  c_cinf_ratio <- exp(-k_prime * ebct)
  
  # Calculate TOC removal using the model
  if (oxidation_condition == "non-ozonated") {
    tocremoval <- intercept + media_coefficient + (ebct_coefficient * ebct)
  } else if (oxidation_condition == "ozonated") {
    tocremoval <- intercept + media_coefficient + (o3_toc_coefficient * o3_toc_ratio) +
      (ebct_coefficient * ebct) + (ebct_o3_toc_coefficient * ebct * o3_toc_ratio)
  } else {
    stop("Invalid oxidation condition. Choose from 'non-ozonated' or 'ozonated'.")
  }
  
  # Adjust water TOC based on the calculated TOC removal and c/c_inf ratio
  if (!is.na(water@toc) & water@toc >= water@doc) {
    water@toc <- water@toc - (water@toc * tocremoval / 100) * c_cinf_ratio
  } else if (!is.na(water@toc) & water@toc < water@doc) {
    warning("TOC of input water less than DOC. TOC will be set equal to DOC.")
    water$toc <- water$doc - (water$doc * tocremoval / 100) * c_cinf_ratio
  } else if (is.na(water$toc)) {
    warning("Input water TOC not specified. Output water TOC will be NA.")
    water$toc <- NA_real_
  }
  
  water$doc <- water$toc  # Assuming DOC is a subset of TOC in this simplified model
  water$treatment <- paste(water$treatment, "_tocremoved", sep = "")
  
  # Convert to data frame
  water_df <- data.frame(
    ph = water$ph,
    temp = water$temp,
    alk = water$alk,
    toc = water$toc,
    doc = water$doc,
    uv254 = water$uv254,
    treatment = water$treatment
  )
  
  return(water_df)
}

# Example usage
# Define your water object with your specific values
water <- define_water(ph = 7, temp = 25, alk = 100, toc = 3.7, doc = 3.5, uv254 = .1)

# Run the biofilter_toc function with your values and print the results

# For non-ozonated condition using GAC
filtered_water_non_ozonated_gac <- biofilter_toc(water, ebct = 20, temperature = 15, media_type = "GAC", oxidation_condition = "non-ozonated")
print(filtered_water_non_ozonated_gac)

# For non-ozonated condition using sand
filtered_water_non_ozonated_sand <- biofilter_toc(water, ebct = 20, temperature = 15, media_type = "sand", oxidation_condition = "non-ozonated")
print(filtered_water_non_ozonated_sand)

# For non-ozonated condition using anthracite
filtered_water_non_ozonated_anthracite <- biofilter_toc(water, ebct = 20, temperature = 15, media_type = "anthracite", oxidation_condition = "non-ozonated")
print(filtered_water_non_ozonated_anthracite)

# For ozonated condition using GAC
filtered_water_ozonated_gac <- biofilter_toc(water, ebct = 20, temperature = 15, media_type = "GAC", oxidation_condition = "ozonated", o3_toc_ratio = 0.8)
print(filtered_water_ozonated_gac)

# For ozonated condition using sand
filtered_water_ozonated_sand <- biofilter_toc(water, ebct = 20, temperature = 15, media_type = "sand", oxidation_condition = "ozonated", o3_toc_ratio = 0.8)
print(filtered_water_ozonated_sand)

# For ozonated condition using anthracite
filtered_water_ozonated_anthracite <- biofilter_toc(water, ebct = 20, temperature = 15, media_type = "anthracite", oxidation_condition = "ozonated", o3_toc_ratio = 0.8)
print(filtered_water_ozonated_anthracite)

