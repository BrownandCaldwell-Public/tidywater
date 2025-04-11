# tidywater development version

* `calculate_corrosion` expanded CCPP search range (fewer errors)

## Breaking changes
* Helper function (`_chain` and `_once`) behavior change: can now specify column name unquoted (eg, alum = AlumDose)
* Removed most (`_once`) helper functions because the desired output is almost never all water slots as columns. 
Refer to `pluck_water(parameter = "all")` for same behavior.
* `pac_toc` now constrained to accept a smaller range of doses and times. The form of the equation was allowing negative TOC outputs.
* `define_water`, `chemdose_ph`, `blend_waters`, and other pH related functions slightly impacted by a fix in concentration vs activity. 
Previous code assumed pH = 10^-[H+], code has been corrected to pH = 10^-{H+}

## New features
* User may now choose which cation or anion to use for balancing ions

# tidywater 0.7.0

## New features
* chlorine and chloramine decay: `chemdose_chlordecay`
* New water slots for chloramine chemistry: `combined_chlorine`, `nh2cl`, `nhcl2`, `ncl3`
* `solvemass_solids` separates functionality from `solvecost_solids` to solve lb/day
* `biofilter_toc`, `chemdose_chlordecay`, `ozonate_bromate`, and `solvect` helpers now available.

## Breaking changes
* `chemdose_ct` renamed `solvect_chlorine`
* `ozonate_ct` renamed `solvect_o3`
* `tot_ocl` slot in water renamed `free_chlorine`
* `define_water` argument changes: `tot_ocl` changed to `free_chlorine`, added `combined_chlorine`
* Helper function (`_chain` and `_once`) behavior change: if multiple values are specified for multiple arguments, all combinations are used.

# tidywater 0.6.2

* CRAN resubmission.
* Minor changes to DESCRIPTION and examples using `plan`

# tidywater 0.6.1

* Initial CRAN submission.
* Fix R CMD check notes

# tidywater 0.6.0

## New features
* biofilter_toc updates the bdoc water slot
* pac_toc helper functions _chain and _once

## Breaking changes
* biofilter_toc argument, o3_dose, was replaced with ozonated, which accepts TRUE or FALSE inputs

# tidywater 0.5.0

## Fixes
* default temperature is now 25C
* corrected enthalpy of reaction for ammonium ion
* completed PAC models

## New features
* chemdose_ct: CT calculations, including CT actual, CT required, and giardia log removal
* solvecost_ family: cost calculations, including chemicals, power, solids, and labor
* solvemass_ :convert chemical doses from mg/L to lb/day
* solveresid_o3: ozone decay model and corresponding helper function from WTP model
* ozonate_ct: ozone CT model
* validate water function, not exported but useful for function writing
* chemdose_f: fluoride model for alum addition. Requires site specific fitting.
* biofilter_toc: biofiltration model (Terry & Summers)
* added ACH to chemdose_ph

## Breaking changes
* total ammonia water slot changed from tot_nh4 to tot_nh3

## Code structure changes
* renamed and rearranged R scripts to better find functions and associated helper functions
* update most functions to use base R, and only use dplyr functions where necessary (increase speed)

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
