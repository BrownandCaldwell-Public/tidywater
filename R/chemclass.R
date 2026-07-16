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
    strength = "numeric",
    dose_unit = "character",
    hydration = "numeric",
    cost = "numeric",
    solids_formula = "character"
  ),
  prototype(
    # defaults
    chemical = "",
    strength = 100,
    # default strength 100 like TW already assumes
    dose_unit = "mg/L",
    # TW chemicals typically in mg/L as now
    hydration = NA_real_,
    cost = NA_real_,
    solids_formula = ""
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

