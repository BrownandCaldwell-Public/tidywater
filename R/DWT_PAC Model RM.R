

#PAC modeling
#Used for predicting DOC concentration

#' @title Calculate DOC Concentration in PAC system
#'
#' @description \code{chemdose_PAC} calculates DOC concentration multiple linear regression model found in 2-METHYLISOBORNEOL AND NATURAL ORGANIC MATTER
# ADSORPTION BY POWDERED ACTIVATED CARBON by HYUKJIN CHO (2007)
#' Required arguments include an object of class "water"
#' created by \code{\link{define_water}} initial DOC concentration, amount of PAC added to system, contact time with PAC, type of PAC
#' 
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including  DOC, TOC, UV254 
#'
#' @details The function will calculate DOC concentration by PAC adsorption in drinking water treatment
#' The function returns a new object of class "water" with predicted DOC concentrations.
#' Use \code{summarise_wq} to quickly tabulate the results.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#' @source CHO(2007)
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param PAC_amount Applied PAC (mg/L). Model results are valid for doses concentrations between 5 and 30 mg/L.
#' @param time Contact time (minutes). Model results are valid for reaction times between 10 and 1440 minutes
#' @param PAC_type Type of PAC applied, either "bituminous", "lignite", "wood".
#' 
#define input water ---- !!DOC input is preferred !! -----
# @example
#water <- suppressWarnings(define_water(doc=2.5, uv254=.05,toc=1.5)) %>%
#PACdose_toc( time=50, PAC_type="wood",PAC_amount=15)

#define function based on input PAC type 
#'
PACdose_toc <- function(water, PAC_amount, time, PAC_type, doc_0,toc) {
  #make case insensitive
  PAC_type <- tolower(PAC_type)
  
  doc_0 = water@doc
uv254 = water@uv254
toc=water@toc

if (missing(PAC_amount)) {
  stop("PAC Dose not specified")
}

if (missing(time)) {
  stop("Time not specified")
}


#warnings for bounds of PAC dose, time, defined doc in tidywater etc. 
if (PAC_amount < 5 || PAC_amount > 30) {
  stop("PAC Dose is outside the model bounds of 5 to 30 mg/L")
}


if (time < 10 || time > 1440) {
  stop("Duration is outside the model bounds of 10 to 1440 mg/L")
}


if (missing(water)) {
  stop("No source water defined. Create a water using the 'define_water' function.")}
if (!methods::is(water, "water")) {
  stop("Input water must be of class 'water'. Create a water using 'define_water'.")
}

if (PAC_amount<= 0 & time <= 0 & water@doc <= 0) {
  warning("No PAC added. Final water will equal input water.")
}


#set default PAC type if not specified
if (PAC_type == "") {
  PAC_type <- "bituminous"
}


#more warnings
if (!is.na(water@toc) & water@toc < water@doc) {
  warning("TOC of input water less than DOC. TOC will be set equal to DOC.") }
if (is.na(water@toc)) {
  warning("Input water TOC not specified. Output water TOC will be NA.")
  water@toc = NA}
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

result <- doc_final(water, PAC_amount, time, PAC_type, doc_0) 
print(paste("result for DOC given inputted PAC type", material, "is",result))

# Predict DOC concentration via UV absorbance

#UVA can be a good indicator to predict DOC concentration by PAC adsorption
#can be predicted through relationship of DOC and UVA removal --> dimensionless unit (C/C0) 

UVA <-.0376*result-.041
print(paste("UV Absorbance via estimated doc:", UVA))

toc_new <- result + org_carbon_undissolved

water@doc=result
water@uva=UVA
water@toc= toc_new

return(water)

}

