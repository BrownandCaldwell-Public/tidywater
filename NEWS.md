# tidywater 0.11.0

## New features

* Changes to existing functions
  * `chemdose_toc()` gains reuse-water models from Wilder & Korak (2025) for aluminum coagulants.
  * `biofilter_toc()` gains reuse and ATP-based models from Peterson & Summers (2021) and Terry et al. (2024).
  * `chemdose_ph()` now supports polyaluminum chloride and citric acid. `chemdose_toc()` now supports ACH and PACl.
  * `define_water()` gains `no2` (nitrite, mg/L as N) and `tot_citric` (citric acid) arguments.
  * The `water` class gains a `co2` slot (dissolved CO2, H2CO3*, in mg/L as CO2). It is calculated from `h2co3`.
  * `convert_units()` supports mg/L as P and mg/L as Cl2. Start and end units and formulas allow for "close enough" spellings (eg; case insensitive, with or without spaces and slashes, etc). `formula` is optional when converting within the same unit type (e.g. mg/L to g/L).

* New functions
  * `scavenge_oh()` calculates the background OH radical scavenging term for UV/AOP.
  * `chemdose_inorgdemand()` calculates the free chlorine residual after the inorganic chlorine demand of Mn, Fe, and ammonia.
  * `check_water()` contains many of the warnings that were previously included in `define_water()`. The warnings were cumbersome and often ignored, so this function provides a way to deliberately evaluate how a water quality is defined.
  * `convert_flow()` for unit conversions involving flow rates.
  * `define_chemical()` function and new `chemical` class are the starting point for better units handling of chemicals, but are not yet incorporated into other functions.

## Breaking changes

* API changes
  * `chemdose_ph_df()` and the other `chemdose_*_df()` functions gain `keep_dose_cols` (default `FALSE`), which drops the dose columns from the output to prevent double dosing downstream.
  * `solvemass_solids()` has been revamped - both inputs and outputs have changed and the underlying calculation is now based directly on stoichiometry.
  * `regulate_toc()` has been revamped - both inputs and outputs have changed.  
* Major calculation changes
  * `convert_units()` now correctly converts through moles of the formula, which changes some results: mg/L as CaCO3 to or from M or eq/L now uses the formula's charge. Monovalent ions were previously off by a factor of 2 (e.g. 50 mg/L as CaCO3 of HCO3 is now 1 mM, not 0.5 mM). Divalent conversions like hardness and alkalinity are unchanged. mg/L as N counts N atoms per formula, so (NH4)2SO4 is 2 N.
  * `define_water()` now warns and sets carbonate species to 0 when `alk` is less than the non-carbonate alkalinity
  (e.g. hydroxide at high pH). Previously, negative carbonate alkalinity could be carried through. Argument `tot_bo3` is now `tot_bo4`, and the `bo3` and `tot_bo3` slots are replaced by `h3bo3`, `h4bo4`, and `tot_bo4`.
  * Kw is now calculated consistently with `K_temp_adjust()` using the van 't Hoff equation, assuming a constant enthalpy of 55,830 J/mol for H2O -> H+ + OH-. It previously used a rounded empirical formula; pH results change slightly at temperatures other than 25 C.
  * `chemdose_chloramine()` previously stored free ammonia in the `tot_nh3` slot. The slot now correctly holds total ammonia.
  * `chemdose_chlordecay()` with `use_chlorine_slot = TRUE` now reads the chlorine slot as Cl2, not Cl, so the slot counts at its full value. The free chlorine residual is also clamped at 0 and no longer goes negative.
  * `solvect_chlorine()` now correctly converts the `free_chlorine` slot from mol/L to mg/L as Cl2 when `free_cl_slot = "slot_only"` or `"sum_with_residual"`. 
  * `dissolve_cu()` now correctly uses orthophosphate in mg/L as PO4; it previously used mol/L.
  * `gacbv_toc()` now correctly looks up bed volume at C/C0 = `target_doc / doc`.
* Minor calculation changes and edge cases
  * `balance_ions()` now includes Fe, Al, Mn, F, and Br.
  * `chemdose_ph()` now accounts for borate and silicate.
  * `solveresid_o3()` returns 0 instead of a negative residual when ozone demand exceeds the dose, so `solvect_o3()` no longer returns negative CT. 
  * `gacrun_toc()`, `gac_toc()`, and `gacbv_toc()` now use influent DOC, not TOC. Both the Zachman and WTP models are written in terms of TOC, but GAC influent is normally coagulated and filtered water, where TOC = DOC.
  * `pac_toc()` corrections at low DOC and UV254.
  * `convert_watermg()` returns `tot_sio4` in mg/L as SiO2, matching `define_water()` input.
  * `modify_water()` now converts TOC, DOC, and BDOC entered in ug/L and DBPs entered in mg/L correctly. Both conversions were inverted. It also accepts `tbaa` and no longer lists the nonexistent `baa` slot.
  * `modify_water_df()` now applies `slot`, `value`, and `units` passed as constants. Before, they were silently ignored and the water was returned unchanged. It now errors when a column it needs is missing.




## Other changes

* `opensys_ph()` widens its CO2 search when more than 100 mg/L of CO2 has to be removed or added, instead of failing
with a `uniroot()` error. If no equilibrium dose is found, it gives a clear error.
* `solvedose_alk()` no longer lists or accepts `chemical = "co2"`, since CO2 doesn't change alkalinity. It now errors with "Selected chemical addition is not supported" instead of a `uniroot()` error.
* `define_water_df()` now maps columns to `define_water()` arguments by name.
* Warnings and errors now use cli. Repeated warnings from `_df` functions are reported once with the number of rows
affected. 

# tidywater 0.10.0

## New features

* Revamped helpers `_df` replace `chain` and `once` functions. Use `pluck_cols = TRUE` to mimic `once` behavior, although it will always
return a water column as well.
* Virus log removal added to `solvect_chlorine` based on the EPA Guidance Manual Table E-7 (1991)
* `modify_water` can now modify multiple water slots at once. Note that in order to modify multiple slots using `modify_water_df`, all input arguments must be included as a list.
* Acetic acid added as chemical to `chemdose_ph`. In addition to changing pH, dosing acetic acid will also update TOC and DOC of the input water.
* `regulate_toc` now available for calculating TOC removal compliance
* `gacrun_toc`, `gac_toc`, and `gacbv_toc` added to predict TOC removal from GAC treatment using either the EPA WTP model (2001) or 
the Zachman and Summers (2018) model. Different functions provide different output types: a data frame of the TOC breakthrough curve, 
a new water with updated TOC, DOC, and UV254 water slots, or the 
necessary bed volumes to stay below the target effluent TOC. See individual functions for documentation.
* `pluck_water` and `_df` helpers sped up by relying on base R instead of purrr
* `chemdose_toc` can now account for doc removal due to lime softening from the WTP Model (2001). To implement, use the `caoh2` argument.
* New model `opensys_ph` added to predict pH and alkalinity of an input water in an open carbonate system at equilibrium
* New function `plot_lead` added to graph the pH vs DIC contour plot for lead solubility
* Alkalinity slots added to the water class. `alk` and `alk_eq` represent the total alkalinity, and individual alkalinities due to carbonate, phosphate, ammonium, borate, silicate,
and hypochlorite have been added as individual slots (eg. carbonate_alk_eq).

## Breaking changes
* `_chain` and `_once` functions have been deprecated. Replace with `_df` functions. Code should be similar, but has slightly different outputs.
* Default water naming in helper functions has been shortened to remove "_water". 
Eg, new default `output_water = "defined"`, old default `output_water = "defined_water"`. See function documentation for new defaults.
* Total alkalinity (`alk`) now accounts for phosphate, silicate, borate, hypochlorite, and ammonia. Could have a minor impact on final pH and alkalinity
calculations in waters with those compounds.
* Now depends on R >= 4.1.0 for built in pipe operator, `|>`

# tidywater 0.9.0

## New features

* New `chemdose_ph` chemicals: CaOCl2, CaSO4, HNO3, KMnO4, NaF, and Na3PO4
* New model `dissolve_cu` predicts the concentration of copper given pH, DIC, and phosphate based on the empirical model described in Lytle et al. (2018). 
`dissolve_cu_once` can also predict copper for waters in a data frame.
* `chemdose_dbp` can now input custom fitting coefficients as a data frame
* `chemdose_ph_once` and `chemdose_toc_once` are back and only return relevant waters slots in its output.
* `decarbonate_ph`: function to remove CO2 (H2CO3) from a water and determine the new pH (and division of ions)
* `modify_water`: function to modify individual slots in a water that handles unit conversions

## Breaking changes

* `applied_treatment` slot removed from water. Was not providing any benefit and added complexity. Should be the responsibility of the user to track.
* `h2co3` slot added to `water` class calculated based on {H} and total carbonate.
* `chemdose_ph` warns when ion water slots aren't updated due to NA slots.
* `solvedose_ph` updated search range to allow for more water qualities without erroring. Updated search process results in slightly different outputs.
* `_toc` functions are DOC-based and previously assumed no particulate TOC removal. In reality, virtually all particulate 
TOC is removed with other particulates, so models have been updated to return TOC = DOC
* `chemdose_toc` custom coeff now accepts a data frame instead of a named list
* Corrosion index slots removed from `water` class. 
* `calculate_corrosion` now outputs a data frame with the corrosion indices as columns. 
`calculate_corrosion_chain` removed because the base function has a numeric output.
* `calculate_corrosion` updated CCPP search range to allow for more water qualities without erroring.
* `solvect_o3` and `solveresid_o3` now return zero instead of NaN when the input dose is zero

# tidywater 0.8.2

* Reduce examples for CRAN speed again.

# tidywater 0.8.1

* Pare down examples and vignettes to speed up CRAN checks
* Minor updates to `chemdose_chloramine` warnings.

# tidywater 0.8.0

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
* dic now calculated in `define_water`
* dic now available in `convert_units`
* `chemdose_chloramine`: chloramine formation model given chlorine, ammonia, time
* `chemdose_chlordecay` now has argument, `use_chlorine_slot`. Function can now use chlorine dose and/or free_chlorine or combined_chlorine slots.

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
