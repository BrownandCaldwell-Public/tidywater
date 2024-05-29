# tidywater 0.2.0
## New features
* TOC removal through coagulation, `chemdose_toc` and matching `_chain` and `_once` helper functions.
* DBP formation from coagulation, `chemdose_dbp`. No helper functions yet except `summarise_dbp`
* Calculation of corrosion indices, `calculate_corrosion` and `summarise_corrosion` with helper functions.
* Theoretical lead solubility `dissolve_pb` with helper functions.
* Helper function `pluck_water` to pull one slot from a `water` column in a data frame.
## Breaking changes
* Changes in S4 `water` class and `define_water` to handle more water quality parameters.
## Calculation changes
* Activity is calculated from ionic strength and used in pH calculations.
* Ionic strength is based on TDS or conductivity and is recalculated when appropriate in `balance_ions` and `chemdose_ph`


# tidywater 0.1.0

* Initial release
* Acid/base equilibrium with assumption activity = concentration
* Helper functions `_chain` and `_once` for applying models to data frames.
