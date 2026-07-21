




methods::setClass(
  "chemical",
  representation(
    # formula
    # strength
    # dose unit
    # hydration
    # cost
    # solids formula

    formula = "character",
    dose = "numeric",
    strength = "numeric",
    dose_unit = "character",
    hydration = "numeric",
    cost = "numeric",
    solids_formula = "character"
    #molecular_weight = "numeric"
    # maybe add in later 
  ),
  prototype(
    # defaults
    formula = NA_character_,
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
    solids_formula = NA_character_,
    # empty default
    molecular_weight=NA_real_
  )
)

#list of checmicals 
#  chems<-list( 
#   hcl,
#   h2so4,
#   h3po4,
#   hno3,
#   ch3cooh,
#   co2,
#   naoh,
#   caoh2,
#   mgoh2,
#   na2co3,
#   nahco3,
#   caco3,
#   caso4,
#   caocl2,
#   cacl2,
#   cl2,
#   naocl,
#   nh4oh,
#   nh42so4,
#   alum,
#   ferricchloride,
#   ferricsulfate,
#   ach,
#   pacl,
#   kmno4,
#   naf,
#   na3po4
#  )




# functions/ methods

# Summary function (show is a default generic so it dosen't need to be defined above)
methods::setMethod(
  "show",
  "chemical",
  function(object) {
    # General parameters
    cat("Chemical Name: ", object@formula, "\n")
    cat("Dose: ", object@dose, "\n")
    cat("Strength : ", object@strength, "%", "\n")
    cat("Dosage Units: ", object@dose_unit, "\nUse summary functions or slot names to view other parameters.\n")
  }
)
