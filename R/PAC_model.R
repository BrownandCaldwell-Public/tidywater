
#PAC modeling
#Used for predicting change in DOC concentration with powdered activated carbon (PAC) dose

#' @title Calculate DOC Concentration in PAC system
#'
#' @description The function will calculate DOC concentration by PAC adsorption in drinking water treatment
#' The function returns a new object of class "water" with predicted DOC concentrations. UV254 concentrations
#' are predicted based on a linear relationship with DOC.
#'
#' @details \code{chemdose_PAC} calculates DOC concentration using a multiple linear regression model found in
#' 2-METHYLISOBORNEOL AND NATURAL ORGANIC MATTER ADSORPTION BY POWDERED ACTIVATED CARBON by HYUKJIN CHO (2007).
#' Required arguments include an object of class "water" created by \code{\link{define_water}}, initial DOC concentration,
#' amount of PAC added to system, contact time with PAC, and type of PAC. Input water must contain DOC or TOC value.
#'
#' @source CHO(2007)
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param dose Applied PAC dose (mg/L). Model results are valid for doses concentrations between 5 and 30 mg/L.
#' @param time Contact time (minutes). Model results are valid for reaction times between 10 and 1440 minutes
#' @param type Type of PAC applied, either "bituminous", "lignite", "wood".
#'
#' @examples
#' water <- suppressWarnings(define_water(doc=1.5, uv254=.05, toc=2))
#' dosed_water <- pac_toc(water, dose = 15, time=50, type="wood")
#' dosed_water@toc
#'
#' @export

pac_toc <- function(water, dose, time, type = "bituminous") {

  #make case insensitive
  type <- tolower(type)


  doc = water@doc
  uv254 = water@uv254
  toc=water@toc

  if (missing(dose)) {
    stop("PAC dose not specified.")
    }

  if (missing(time)) {
    stop("Time not specified.")
    }


  #warnings for bounds of PAC dose, time, defined doc in tidywater etc.
  if (dose< 5 | dose> 30) {
    warning("PAC Dose is outside the model bounds of 5 to 30 mg/L")
    }

  if (time < 10 | time > 1440) {
    warning("Time is outside the model bounds of 10 to 1440 min.")
    }

  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
    }

  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
    }

  if (dose == 0) {
    warning("No PAC added. Final water will equal input water.")
  }

  if (dose < 0) {
    warning("Cannot enter a negative dose.")
  }


  #more warnings
  if (!is.na(water@toc) & water@toc < water@doc) {
    warning("TOC of input water less than DOC. TOC will be set equal to DOC.")
  }

  if (is.na(water@toc)) {
    warning("Input water TOC not specified. Output water TOC will be NA.")
    }

  if (doc < 1 || doc > 5) {
    stop("DOC concentration is outside the model bounds of 1 to 5 mg/L")
  }

  if (!(type %in% c("bituminous", "wood", "lignite"))){
    stop("Invalid PAC type. Choose either 'bituminous', 'wood' or 'lignite'.")
  }

  #Calculate toc
  particulate_org_carbon <- toc-doc

  if (dose>0 & type== "bituminous") {
    result <- 0.1561 + 0.9114*doc - 0.0263*dose - 0.002*time
  } else if (dose>0 & type == "lignite") {
    result <- 0.4078 + 0.8516*doc - 0.0225*dose - 0.002*time
  } else if (dose>0 & type == "wood") {
    result <- 0.3653 + 0.8692*doc - 0.0151*dose - 0.0025*time
  } else if (dose==0) {
    result <- doc
  }

  # Predict DOC concentration via UV absorbance

  #UVA can be a good indicator to predict DOC concentration by PAC adsorption
  #can be predicted through relationship of DOC and UVA removal --> dimensionless unit (C/C0)

  UVA <- 0.0376*result - 0.041

  toc_new <- result + particulate_org_carbon

  water@doc=result
  water@uv254=UVA
  water@toc= toc_new

  return(water)
}

