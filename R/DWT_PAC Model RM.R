

#PAC modeling
#Used for predicting DOC concentration

#' @title Calculate DOC Concentration in PAC system
#'
#' @description \code{chemdose_PAC} calculates DOC concentration multiple linear regression model found in 2-METHYLISOBORNEOL AND NATURAL ORGANIC MATTER
# ADSORPTION BY POWDERED ACTIVATED CARBON by HYUKJIN CHO (2007)
#' Required arguments include an object of class "water"
#' created by \code{\link{define_water}} initial DOC concentration, amount of PAC added to system, contact time with PAC, type of PAC
#' 
#' water must contain DOC or TOC value.
#'
#' @details The function will calculate DOC concentration by PAC adsorption in drinking water treatment
#' The function returns a new object of class "water" with predicted DOC concentrations. UV254 concentrations are predicted based on a linear relationship with DOC.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#' @source CHO(2007)
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param dose Applied PAC dose (mg/L). Model results are valid for doses concentrations between 5 and 30 mg/L.
#' @param time Contact time (minutes). Model results are valid for reaction times between 10 and 1440 minutes
#' @param type Type of PAC applied, either "bituminous", "lignite", "wood".
#' 
#' @example
#' water <- suppressWarnings(define_water(doc=2.5, uv254=.05,toc=1.5)) %>%
#' pac_toc(dose = 15, time=50, type="wood")
#'
#' @export
#'
pac_toc <- function(water, dose, time, type = "bituminous") {

  #make case insensitive
  PAC_type <- tolower(type)

  
  doc_0 = water@doc
uv254 = water@uv254
toc=water@toc

if (missing(dose)) {

  stop("PAC Dose not specified")
}

if (missing(time)) {
  stop("Time not specified")
}


#warnings for bounds of PAC dose, time, defined doc in tidywater etc. 
if (dose< 5 | dose> 30) {
  warning("PAC Dose is outside the model bounds of 5 to 30 mg/L")
}


if (time < 10 | time > 1440) {
  warning("Duration is outside the model bounds of 10 to 1440 min")
}



if (missing(water)) {
  stop("No source water defined. Create a water using the 'define_water' function.")}
if (!methods::is(water, "water")) {
  stop("Input water must be of class 'water'. Create a water using 'define_water'.")
}

if (dose<= 0) {

  warning("No PAC added. Final water will equal input water.")
}


#more warnings
if (!is.na(water@toc) & water@toc < water@doc) {
  warning("TOC of input water less than DOC. TOC will be set equal to DOC.") }
if (is.na(water@toc)) {
  warning("Input water TOC not specified. Output water TOC will be NA.")
}

if (doc_0 < 1 || doc_0 > 5) {
  stop("DOC concentration is outside the model bounds of 1 to 5 mg/L")
}


#Calculate toc
org_carbon_undissolved <- toc-doc_0


if (PAC_type== "bituminous") {
  result <- .1561+.9114*doc_0 - .0263*PAC_amount - .002*time
} else if (PAC_type== "lignite") {
  result <- .4078+.8516*doc_0 - .0225*PAC_amount - .002*time
} else if (PAC_type== "wood") {
  result <- .3653+.8692*doc_0 - .0151*PAC_amount - .0025*time
} else {
  stop("Invalid PAC type. Choose either 'Bituminous', 'Wood' or 'Lignite' ")
}

# Predict DOC concentration via UV absorbance

#UVA can be a good indicator to predict DOC concentration by PAC adsorption
#can be predicted through relationship of DOC and UVA removal --> dimensionless unit (C/C0) 

UVA <-.0376*result-.041

toc_new <- result + org_carbon_undissolved

water@doc=result
water@uva=UVA
water@toc= toc_new

return(water)

}

