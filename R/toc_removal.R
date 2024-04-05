# TOC models


#' Determine TOC removal from coagulation
#'
#' This function applies the Edwards (1997) model to a water created by \code{\link{define_water}} to determine coagulated
#' DOC. Coagulated UVA is from the Water Treatment Plant Model 2.0, equation 5-80. Note that the models rely on pH of coagulation. If
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
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 3.7, doc = 3.5, uv254 = .1)
#' dosed_water <- chemdose_ph(water, alum = 30) %>%
#' chemdose_toc(alum = 30, coeff = "Alum")
#'
#' dosed_water <- chemdose_ph(water, fe2so43 = 30) %>%
#' chemdose_toc(fe2so43 = 30, coeff = "Ferric")
#'
#' dosed_water <- chemdose_ph(water, alum = 10, h2so4 = 10) %>%
#' chemdose_toc(alum = 10, coeff = c("x1" = 280, "x2" = -73.9, "x3" = 4.96, "k1" = -0.028, "k2" = 0.23, "b" = 0.068))
#'
#' @export
#'
chemdose_toc <- function(water, alum = 0, fecl3 = 0, fe2so43 = 0, coeff = "Alum") {

  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")}
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }

  if (is.na(water@doc) | is.na(water@ph) | is.na(water@uv254)) {
    stop("Water is missing a modeling parameter. Make sure ph, doc, and uv254 are all specified.")
  }

  if (class(coeff) == "character") {
    coeffs <- filter(edwardscoeff, ID == coeff)
    if(nrow(coeffs) != 1) {
      stop("coeff must be one of 'Alum', 'Ferric', 'General Alum', 'General Ferric', or 'Low DOC' or coefficients can be manually specified with a vector.")
    }
  } else if (class(coeff) == "numeric") {
    coeffs <- data.frame(k1 = coeff["k1"], k2 = coeff["k2"], x1 = coeff["x1"], x2 = coeff["x2"], x3 = coeff["x3"], b = coeff["b"])
    if(any(is.na(coeffs))) {
      stop("coeff must be specified as a named vector and include 'k1', 'k2', 'x1', 'x2', 'x3', and 'b' or choose coefficients from Edwards model using a string.")
    }
  } else {
    stop("coeffs must be specified with a string or named vector. See documentation for acceptable formats.")
  }

  if (alum <= 0 & fecl3 <= 0 & fe2so43 <= 0) {
    warning("No coagulants dosed. Final water will equal input water.")
  } else if (alum > 0 & (fecl3 > 0 | fe2so43 > 0)) {
    warning("Both alum and ferric coagulants entered.")
  } else if ((fecl3 > 0 | fe2so43 > 0) & any(grepl("Alum", coeff))) {
    warning("Ferric coagulants used with coefficients fit on Alum. Check 'coeff' argument.")
  } else if (alum > 0 & any(grepl("Ferric", coeff))) {
    warning("Alum used with coefficients fit on Ferric. Check 'coeff' argument.")
  }


  # Alum - hydration included
  alum = convert_units(alum, "alum", endunit = "mM")
  # Ferric chloride
  fecl3 = convert_units(fecl3, "fecl3", endunit = "mM")
  # Ferric sulfate
  fe2so43 = convert_units(fe2so43, "fe2so43", endunit = "mM")

  # Convert coagulant units to mMol/L as Al3+ or Fe3+ for DOC model
  coag = alum*2 + fecl3*1 + fe2so43*2
  # Convert to meq/L for UV model
  coag2 = alum*2*3 + fecl3*1*3 + fe2so43*2*3

  # Edwards calculations
  nonadsorb <- water@doc * (coeffs$k1 * calc_suva(water@doc, water@uv254) + coeffs$k2)

  sterm = (1 - calc_suva(water@doc, water@uv254) * coeffs$k1 - coeffs$k2)
  xterm = (coeffs$x1 * water@ph + coeffs$x2 * water@ph ^ 2 + coeffs$x3 * water@ph^3)
  b = coeffs$b

  # Rearrangement of equation from wolfram alpha
  adsorb <- (sqrt(b^2*(water@doc*sterm - coag*xterm)^2 + 2*b*(coag*xterm+water@doc*sterm) + 1) -
      b*coag*xterm+b*water@doc*sterm-1) /
    (2*b)

  if (coag == 0) {
    water@doc = water@doc
    water@uv254 = water@uv254
  } else {
    if(!is.na(water@toc) & water@toc >= water@doc) {
      water@toc = water@toc - water@doc + nonadsorb + adsorb
    } else if (!is.na(water@toc) & water@toc < water@doc) {
      warning("TOC of input water less than DOC. TOC will be set equal to DOC.")
      water@toc = nonadsorb + adsorb
    } else if (is.na(water@toc)) {
      warning("Input water TOC not specified. Output water TOC will be NA.")
      water@toc = NA_real_
    }

    water@doc = nonadsorb + adsorb
    water@uv254 = 5.716 * water@uv254 ^ 1.0894 * coag2 ^ 0.306 * water@ph ^ -.9513
  }
  
  water@treatment <- paste(water@treatment, "_tocremoved", sep = "")

  return(water)

}

# SUVA calc
calc_suva <- function(doc, uv254) {
  uv254 / doc * 100
}
