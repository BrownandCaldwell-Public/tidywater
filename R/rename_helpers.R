#' Functions renamed in tidywater 0.10.0
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' tidywater 0.10.0 renamed several helper functions to simplify naming and match tidyverse conventions
#'
#' * `_chain` -> `_df`
#' * For functions that return a water: `_once` -> `_df(pluck_cols = TRUE)`
#' * For functions with numeric outputs: `_once` -> `_df()`
#'
#' @keywords internal
#' @name rename_helpers
#' @aliases NULL
NULL

#' @rdname rename_helpers
#' @export
balance_ions_chain <- function(df, input_water = "defined_water", output_water = "balanced_water",
                               anion = "cl", cation = "na") {
  lifecycle::deprecate_warn("0.10.0", "balance_ions_chain()", "balance_ions_df()")
  balance_ions_df(df, input_water, output_water, anion, cation)
}

#' @rdname rename_helpers
#' @export
biofilter_toc_chain <- function(df, input_water = "defined_water", output_water = "biofiltered_water",
                                ebct = "use_col", ozonated = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "biofilter_toc_chain()", "biofilter_toc_df()")
  biofilter_toc_df(df, input_water, output_water, ebct, ozonated)
}

#' @rdname rename_helpers
#' @export
blend_waters_chain <- function(df, waters, ratios, output_water = "blended_water") {
  lifecycle::deprecate_warn("0.10.0", "blend_waters_chain()", "blend_waters_df()")
  blend_waters_df(df, waters, ratios, output_water)
}

#' @rdname rename_helpers
#' @export
chemdose_chloramine_chain <- function(df, input_water = "defined_water", output_water = "chlorinated_water",
                                      time = "use_col", cl2 = "use_col", nh3 = "use_col",
                                      use_free_cl_slot = "use_col", use_tot_nh3_slot = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "chemdose_chloramine_chain()", "chemdose_chloramine_df()")
  chemdose_chloramine_df(df, input_water, output_water,
                         pluck_cols = FALSE, water_prefix = TRUE,
                         time, cl2, nh3,
                         use_free_cl_slot, use_tot_nh3_slot)
}

#' @rdname rename_helpers
#' @export
chemdose_chlordecay_chain <- function(df, input_water = "defined_water", output_water = "disinfected_water",
                                      cl2_dose = "use_col", time = "use_col",
                                      treatment = "use_col", cl_type = "use_col",
                                      use_chlorine_slot = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "chemdose_chlordecay_chain()", "chemdose_chlordecay_df()")
  chemdose_chlordecay_df(df, input_water, output_water,
                         pluck_cols = FALSE, water_prefix = TRUE,
                         cl2_dose, time,
                         treatment, cl_type,
                         use_chlorine_slot)
}

#' @rdname rename_helpers
#' @export
chemdose_dbp_chain <- function(df, input_water = "defined_water", output_water = "disinfected_water",
                               cl2 = "use_col", time = "use_col",
                               treatment = "use_col", cl_type = "use_col", location = "use_col", correction = TRUE, coeff = NULL) {
  lifecycle::deprecate_warn("0.10.0", "chemdose_dbp_chain()", "chemdose_dbp_df()")
  chemdose_dbp_df(df, input_water, output_water,
                  pluck_cols = FALSE, water_prefix = TRUE,
                  cl2, time,
                  treatment, cl_type, location, correction, coeff)
}

#' @rdname rename_helpers
#' @export
chemdose_ph_chain <- function(df, input_water = "defined_water", output_water = "dosed_chem_water",
                              hcl = "use_col", h2so4 = "use_col", h3po4 = "use_col", hno3 = "use_col", co2 = "use_col", naoh = "use_col",
                              na2co3 = "use_col", nahco3 = "use_col", caoh2 = "use_col", mgoh2 = "use_col",
                              caocl2 = "use_col", cacl2 = "use_col", cl2 = "use_col", naocl = "use_col",
                              nh4oh = "use_col", nh42so4 = "use_col", caco3 = "use_col", caso4 = "use_col",
                              alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col", ach = "use_col",
                              kmno4 = "use_col", naf = "use_col", na3po4 = "use_col", softening_correction = "use_col", na_to_zero = TRUE) {
  lifecycle::deprecate_warn("0.10.0", "chemdose_ph_chain()", "chemdose_ph_df()")
  chemdose_ph_df(df, input_water, output_water,
                 na_to_zero = na_to_zero, pluck_cols = FALSE, water_prefix = TRUE,
                 hcl, h2so4, h3po4, hno3, co2, naoh,
                 na2co3, nahco3, caoh2, mgoh2,
                 caocl2, cacl2, cl2, naocl,
                 nh4oh, nh42so4, caco3, caso4,
                 alum, ferricchloride, ferricsulfate, ach,
                 kmno4, naf, na3po4, softening_correction)
}

#' @rdname rename_helpers
#' @export
chemdose_toc_chain <- function(df, input_water = "defined_water", output_water = "coagulated_water",
                               alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col",
                               coeff = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "chemdose_toc_chain()", "chemdose_toc_df()")
  chemdose_toc_df(df, input_water, output_water,
                  pluck_cols = FALSE, water_prefix = TRUE,
                  alum, ferricchloride, ferricsulfate,
                  coeff)
}

#' @rdname rename_helpers
#' @export
define_water_chain <- function(df, output_water = "defined_water") {
  lifecycle::deprecate_warn("0.10.0", "define_water_chain()", "define_water_df()")
  define_water_df(df, output_water)
}

#' @rdname rename_helpers
#' @export
decarbonate_ph_chain <- function(df, input_water = "defined_water", output_water = "decarbonated_water",
                                 co2_removed = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "decarbonate_ph_chain()", "decarbonate_ph_df()")
  decarbonate_ph_df(df, input_water, output_water,
                    pluck_cols = FALSE, water_prefix = TRUE,
                    co2_removed)
}

#' @rdname rename_helpers
#' @export
modify_water_chain <- function(df, input_water = "defined_water", output_water = "modified_water",
                               slot = "use_col", value = "use_col", units = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "modify_water_chain()", "modify_water_df()")
  modify_water_df(df, input_water, output_water,
                  slot, value, units)
}

#' @rdname rename_helpers
#' @export
ozonate_bromate_chain <- function(df, input_water = "defined_water", output_water = "ozonated_water",
                                  dose = "use_col", time = "use_col", model = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "ozonate_bromate_chain()", "ozonate_bromate_df()")
  ozonate_bromate_df(df, input_water, output_water, pluck_cols = FALSE, water_prefix = TRUE,
                     dose, time, model)
}

#' @rdname rename_helpers
#' @export
pac_toc_chain <- function(df, input_water = "defined_water", output_water = "pac_water",
                          dose = "use_col", time = "use_col", type = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "pac_toc_chain()", "pac_toc_df()")
  pac_toc_df(df, input_water, output_water, pluck_cols = FALSE, water_prefix = TRUE,
             dose, time, type)
}


#' @rdname rename_helpers
#' @export
calculate_corrosion_once <- function(df, input_water = "defined_water",
                                     index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
                                     form = "calcite") {
  lifecycle::deprecate_warn("0.10.0", "calculate_corrosion_once()", "calculate_corrosion_df()")
  calculate_corrosion_df(df, input_water, water_prefix = TRUE, index, form)
}

#' @rdname rename_helpers
#' @export
chemdose_dbp_once <- function(df, input_water = "defined_water", cl2 = "use_col", time = "use_col",
                              treatment = "use_col", cl_type = "use_col", location = "use_col", correction = TRUE, coeff = NULL,
                              water_prefix = TRUE) {
  lifecycle::deprecate_warn("0.10.0", "chemdose_dbp_once()", "chemdose_dbp_df()")
  chemdose_dbp_df(df, input_water, output_water = "disinfected_water", pluck_cols = TRUE, water_prefix = FALSE,
                  cl2, time,
                  treatment, cl_type, location, correction, coeff)
}

#' @rdname rename_helpers
#' @export
chemdose_ph_once <- function(df, input_water = "defined_water",
                             hcl = "use_col", h2so4 = "use_col", h3po4 = "use_col", hno3 = "use_col", co2 = "use_col", naoh = "use_col",
                             na2co3 = "use_col", nahco3 = "use_col", caoh2 = "use_col", mgoh2 = "use_col",
                             caocl2 = "use_col", cacl2 = "use_col", cl2 = "use_col", naocl = "use_col",
                             nh4oh = "use_col", nh42so4 = "use_col", caco3 = "use_col", caso4 = "use_col",
                             alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col", ach = "use_col",
                             kmno4 = "use_col", naf = "use_col", na3po4 = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "chemdose_ph_once()", "chemdose_ph_df()")
  chemdose_ph_df(df, input_water, output_water,
                 na_to_zero = FALSE, pluck_cols = TRUE, water_prefix = FALSE,
                 hcl, h2so4, h3po4, hno3, co2, naoh,
                 na2co3, nahco3, caoh2, mgoh2,
                 caocl2, cacl2, cl2, naocl,
                 nh4oh, nh42so4, caco3, caso4,
                 alum, ferricchloride, ferricsulfate, ach,
                 kmno4, naf, na3po4, softening_correction)
}

#' @rdname rename_helpers
#' @export
chemdose_toc_once <- function(df, input_water = "defined_water", output_water = "coagulated_water",
                              alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col",
                              coeff = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "chemdose_toc_once()", "chemdose_toc_df()")
  chemdose_toc_df(df, input_water, output_water,
                  pluck_cols = TRUE, water_prefix = FALSE,
                  alum, ferricchloride, ferricsulfate,
                  coeff)
}

#' @rdname rename_helpers
#' @export
define_water_once <- function(df) {
  lifecycle::deprecate_warn("0.10.0", "define_water_once()", "define_water_df()")
  define_water_df(df)
}

#' @rdname rename_helpers
#' @export
dissolve_cu_once <- function(df, input_water = "defined_water") {
  lifecycle::deprecate_warn("0.10.0", "dissolve_cu_once()", "dissolve_cu_df()")
  # TODO: dissolve_cu_df(water)
}
#' @rdname rename_helpers
#' @export
dissolve_pb_once <- function(df, input_water = "defined_water", output_col_solid = "controlling_solid",
                             output_col_result = "pb", hydroxypyromorphite = "Schock",
                             pyromorphite = "Topolska", laurionite = "Nasanen", water_prefix = TRUE) {
  lifecycle::deprecate_warn("0.10.0", "dissolve_pb_once()", "dissolve_pb_df()")
  # TODO: dissolve_pb_df(water)
}
#' @rdname rename_helpers
#' @export
solvect_chlorine_once <- function(df, input_water = "defined_water",
                                  time = "use_col", residual = "use_col", baffle = "use_col",
                                  free_cl_slot = "residual_only",
                                  water_prefix = TRUE) {
  lifecycle::deprecate_warn("0.10.0", "solvect_chlorine_once()", "solvect_chlorine_df()")
  # TODO: solvect_chlorine_df(water)
}
#' @rdname rename_helpers
#' @export
solvect_o3_once <- function(df, input_water = "defined_water",
                            time = "use_col", dose = "use_col", kd = "use_col", baffle = "use_col",
                            water_prefix = TRUE) {
  lifecycle::deprecate_warn("0.10.0", "solvect_o3_once()", "solvect_o3_df()")
  # TODO: solvect_o3_df(water)
}
#' @rdname rename_helpers
#' @export
solvedose_alk_once <- function(df, input_water = "defined_water", output_column = "dose_required", target_alk = "use_col", chemical = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "solvedose_alk_once()", "solvedose_alk_df()")
  # TODO: solvedose_alk_df(water)
}
#' @rdname rename_helpers
#' @export
solvedose_ph_once <- function(df, input_water = "defined_water", output_column = "dose_required", target_ph = "use_col", chemical = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "solvedose_ph_once()", "solvedose_ph_df()")
  # TODO: solvedose_ph_df(water)
}
#' @rdname rename_helpers
#' @export
solveresid_o3_once <- function(df, input_water = "defined_water", output_column = "o3resid",
                               dose = "use_col", time = "use_col") {
  lifecycle::deprecate_warn("0.10.0", "solveresid_o3_once()", "solveresid_o3_df()")
  # TODO: solveresid_o3_df(water)
}


