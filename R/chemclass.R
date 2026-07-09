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
    strength = NA_real_,
    dose_unit = "",
    hydration = NA_real_,
    cost = NA_real_,
    solids_formula = ""
  )
)

# functions/ methods

