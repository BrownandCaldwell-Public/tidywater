
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


# generics 
setGeneric("convert_to_mg_L", function(object ) standardGeneric("convert_to_mg_L"))
setGeneric("convert_chem", function(object ) standardGeneric("convert_chem"))

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
  "convert_to_mg_L",
  "chemical",
  function(object) {
    if (object@dose_unit == "mg/L") {
      return()
    }

    input_unit <- object@dose_unit

    chemical <- object@chemical

    dose <- object@dose

    object@dose <- tidywater::convert_units( dose, chemical, input_unit, "mg/L")

    object@dose_unit <- "mg/L"

    return(object)
  }
)

methods::setMethod(
  "convert_chem",
  "chemical",
  function(object){
    
    cat("select output unit (enter number)")

    dose<- readline(prompt = "options: 
      1) mg/L
      2) M
      3) ug/L
      ")
    
    switch(dose,
      '1' = end_unit<- "mg/L",
      '2' = end_unit<- "M",
      '3' = end_unit<- "ug/L"

    )
    
    input_unit <- object@dose_unit

    chemical <- object@chemical

    dose <- object@dose

    #end_unit<-"mg/L"

    object@dose <- tidywater::convert_units( dose, chemical, input_unit, end_unit)

    object@dose_unit <- end_unit

    return(object)
  }
)