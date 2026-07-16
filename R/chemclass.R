# Create water class
methods::setClass(
  "chemical",
  representation(
    # formula
    # strength
    # dose unit
    # hydration
    # cost
    # solids formula

    chemical = "character",
    dose = "numeric",
    strength = "numeric",
    dose_unit = "character",
    hydration = "numeric",
    cost = "numeric",
    solids_formula = "character"
  ),
  prototype(
    # defaults
    chemical = "",
    # empty default
    dose = NA_real_,
    # default units below
    strength = 100,
    # default strength 100 like TW already assumes
    dose_unit = "mg/L",
    # TW chemicals typically in mg/L as now
    hydration = NA_real_,
    # empty default
    cost = NA_real_,
    # empty default
    solids_formula = ""
    # empty default
  )
)

# functions/ methods

methods::setMethod(
  "show",
  "chemical",
  function(object) {
    # General parameters
    cat("Chemical Name: ", object@chemical, "\n")
    cat("Strength : ", object@strength, "%", "\n")
    cat("Dosage Units: ", object@dose_unit, "\nUse summary functions or slot names to view other parameters.\n")
  }
)

methods::setMethod(
  "convert_to_mg/L",
  "chemical",
  function(object) {
    if (object@dose_unit != "mg/L") {
      return(0)
    }

    input_unit <- object@dose_unit

    chemical <- object@chemical

    dose <- object@dose

    # this implementation would req a new function specifically for converting chemicals
    object@dose <- convert_chemical(chemical, dose, input_unit, "mg/L")

    object@dose_unit <- "mg/L"
  }
)
