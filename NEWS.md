# tidywater 0.4.0

## Fixes
* solve_ph code updated to handle starting po4 concentration

## New features
* convert_watermg for cleaner water exports
* bromate formation models
* ammonia in pH chemistry
* new water slots for F, Fe, Al, etc
* helper functions for chemdose_dbp
* PAC models (incomplete)

## Breaking changes
* treatment slot renamed "applied_treatments"
* solve_ph changes. Should only see different values when po4 is in the water.
* Added hydration to ferric sulfate and renamed coagulants for consistency.
* pluck_water doesn't allow specification of output_column.  It is named by default from the input and parameters. 
Improved pluck does allow multiple parameters and waters in one function.


# tidywater 0.3.0

## Fixes
* Raw water DBP models do not require UVA
* Updated incorrect DBP model coefficients

## New features
* CaCl2 now included in possible chemical addition.

## Breaking changes
* `define_water` now has arguments for "ca" and "mg" and no longer has "ca_hard".
* `summarize_dbp` and `summarize_corrosion` removed. `summarize_wq` now takes arguments to summarize general, ions, dbps, or corrosion


# tidywater 0.2.1

## Bug fixes
* Small vignette changes to fix package build.


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
