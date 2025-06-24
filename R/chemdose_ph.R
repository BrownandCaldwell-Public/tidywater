#' @title Calculate new pH and ion balance after chemical addition
#'
#' @description Calculates the new pH, alkalinity, and ion balance of a water based on different chemical
#' additions.
#' For a single water use `chemdose_ph`; for a dataframe use `chemdose_ph_chain`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#' For most arguments in the `_chain` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details The function takes an object of class "water" created by \code{\link{define_water}} and user-specified
#' chemical additions and returns a new object of class "water" with updated water quality.
#' Units of all chemical additions are in mg/L as chemical (not as product).
#'
#' `chemdose_ph` works by evaluating all the user-specified chemical additions and solving for what the new pH
#' must be using [uniroot] to satisfy the principle of electroneutrality in pure water while correcting for the existing alkalinity
#' of the water that the chemical is added to. Multiple chemicals can be added simultaneously or each addition can be
#' modeled independently through sequential doses.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param water Source water object of class "water" created by [define_water]
#' @param hcl Amount of hydrochloric acid added in mg/L: HCl -> H + Cl
#' @param h2so4 Amount of sulfuric acid added in mg/L: H2SO4 -> 2H + SO4
#' @param h3po4 Amount of phosphoric acid added in mg/L: H3PO4 -> 3H + PO4
#' @param co2 Amount of carbon dioxide added in mg/L: CO2 (gas) + H2O -> H2CO3*
#' @param naoh Amount of caustic added in mg/L: NaOH -> Na + OH
#' @param caoh2 Amount of lime added in mg/L: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Amount of magneisum hydroxide added in mg/L: Mg(OH)2 -> Mg + 2OH
#' @param na2co3 Amount of soda ash added in mg/L: Na2CO3 -> 2Na + CO3
#' @param nahco3 Amount of sodium bicarbonate added in mg/L: NaHCO3 -> Na + H + CO3
#' @param caco3 Amount of calcium carbonate added (or removed) in mg/L: CaCO3 -> Ca + CO3
#' @param caso4 Amount of calcium sulfate added (for post-RO condition) in mg/L: CaSO4 -> Ca + SO4
#' @param cacl2 Amount of calcium chloride added in mg/L: CaCl2 -> Ca2+ + 2Cl-
#' @param cl2 Amount of chlorine gas added in mg/L as Cl2: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Amount of sodium hypochlorite added in mg/L as Cl2: NaOCl -> Na + OCl
#' @param nh4oh Amount of ammonium hydroxide added in mg/L as N: NH4OH -> NH4 + OH
#' @param nh42so4 Amount of ammonium sulfate added in mg/L as N: (NH4)2SO4 -> 2NH4 + SO4
#' @param alum Amount of hydrated aluminum sulfate added in mg/L: Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Amount of ferric Chloride added in mg/L: FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param ach Amount of aluminum chlorohydrate added in mg/L: Al2(OH)5Cl*2H2O + HCO3 -> 2Al(OH)3(am) + Cl + 2H2O + CO2
#' @param softening_correction Set to TRUE to correct post-softening pH (caco3 must be < 0). Default is FALSE. Based on WTP model equation 5-62
#'
#' @seealso \code{\link{define_water}}, \code{\link{convert_units}}
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 10)
#' # Dose 1 mg/L of hydrochloric acid
#' dosed_water <- chemdose_ph(water, hcl = 1)
#' dosed_water@ph
#'
#' # Dose 1 mg/L of hydrochloric acid and 5 mg/L of alum simultaneously
#' dosed_water <- chemdose_ph(water, hcl = 1, alum = 5)
#' dosed_water@ph
#'
#' # Dose 1 mg/L of hydrochloric acid and 5 mg/L of alum sequentially
#' dosed_water1 <- chemdose_ph(water, hcl = 1)
#' dosed_water1@ph
#' dosed_water2 <- chemdose_ph(dosed_water1, alum = 5)
#' dosed_water2@ph
#'
#' # Softening:
#' water2 <- define_water(ph = 7, temp = 25, alk = 100, tot_hard = 350)
#' dosed_water1 <- chemdose_ph(water2, caco3 = -100)
#' dosed_water1@ph
#' dosed_water2 <- chemdose_ph(water2, caco3 = -100, softening_correction = TRUE)
#' dosed_water2@ph
#'
#' @export
#'
#' @returns `chemdose_ph` returns a water class object with updated pH, alkalinity, and ions post-chemical addition.
#'
chemdose_ph <- function(water, hcl = 0, h2so4 = 0, h3po4 = 0, co2 = 0,
                        naoh = 0, caoh2 = 0, mgoh2 = 0,
                        na2co3 = 0, nahco3 = 0, caco3 = 0, caso4 = 0, cacl2 = 0,
                        cl2 = 0, naocl = 0, nh4oh = 0, nh42so4 = 0,
                        alum = 0, ferricchloride = 0, ferricsulfate = 0, ach = 0,
                        softening_correction = FALSE) {
  if ((cacl2 > 0 | cl2 > 0 | naocl > 0) & (nh4oh > 0 | nh42so4 > 0)) {
    warning("Both chlorine- and ammonia-based chemicals were dosed and may form chloramines.\nUse chemdose_chloramine for breakpoint caclulations.")
  }
  if ((cacl2 > 0 | cl2 > 0 | naocl > 0) & water@tot_nh3 > 0) {
    warning("A chlorine-based chemical was dosed into a water containing ammonia, which may form chloramines.\nUse chemdose_chloramine for breakpoint caclulations.")
  }

  if ((nh4oh > 0 | nh42so4 > 0) & (water@free_chlorine > 0 | water@combined_chlorine > 0)) {
    warning("An ammonia-based chemical was dosed into a water containing chlorine, which may form chloramines.\nUse chemdose_chloramine for breakpoint caclulations.")
  }

  validate_water(water, c("ph", "alk"))

  #### CONVERT INDIVIDUAL CHEMICAL ADDITIONS TO MOLAR ####

  # Hydrochloric acid (HCl) dose
  hcl <- convert_units(hcl, "hcl")
  # Sulfuric acid (H2SO4) dose
  h2so4 <- convert_units(h2so4, "h2so4")
  # Phosphoric acid (H3PO4) dose
  h3po4 <- convert_units(h3po4, "h3po4")
  # Carbon dioxide
  co2 <- convert_units(co2, "co2")

  # Caustic soda (NaOH) dose
  naoh <- convert_units(naoh, "naoh")
  # Lime (Ca(OH)2) dose
  caoh2 <- convert_units(caoh2, "caoh2")
  # Magnesium hydroxide (Mg(OH)2) dose
  mgoh2 <- convert_units(mgoh2, "mgoh2")
  # Soda ash (Na2CO3) dose
  na2co3 <- convert_units(na2co3, "na2co3")
  # Sodium bicarbonate (NaHCO3) dose
  nahco3 <- convert_units(nahco3, "nahco3")

  # Calcium chloride (CaCl2) dose
  cacl2 <- convert_units(cacl2, "cacl2")
  # Chlorine gas (Cl2)
  cl2 <- convert_units(cl2, "cl2")
  # Sodium hypochlorite (NaOCl) as Cl2
  naocl <- convert_units(naocl, "cl2")

  # CaCO3
  caco3 <- convert_units(caco3, "caco3")
  
  # CaSO4
  caso4 <- convert_units(caso4, "caso4")

  # Ammonium hydroxide
  nh4oh <- convert_units(nh4oh, "n")
  # Ammonium sulfate
  nh42so4 <- convert_units(nh42so4, "n")

  # Alum - hydration included
  alum <- convert_units(alum, "alum")
  # Ferric chloride
  ferricchloride <- convert_units(ferricchloride, "ferricchloride")
  # Ferric sulfate - hydration included
  ferricsulfate <- convert_units(ferricsulfate, "ferricsulfate")
  # ACH
  ach <- convert_units(ach, "ach")

  #### CALCULATE NEW ION BALANCE FROM ALL CHEMICAL ADDITIONS ####
  dosed_water <- water

  # Total sodium
  na_dose <- naoh + 2 * na2co3 + nahco3 + naocl
  dosed_water@na <- water@na + na_dose

  # Total calcium
  ca_dose <- caoh2 + cacl2 + caco3 + caso4
  dosed_water@ca <- water@ca + ca_dose

  # Total magnesium
  mg_dose <- mgoh2
  dosed_water@mg <- water@mg + mg_dose

  # Total potassium
  k_dose <- 0
  dosed_water@k <- water@k + k_dose

  # Total chloride
  cl_dose <- hcl + cl2 + 2 * cacl2 + 3 * ferricchloride + ach
  dosed_water@cl <- water@cl + cl_dose

  # Total sulfate
  so4_dose <- h2so4 + 3 * alum + 3 * ferricsulfate + nh42so4 + caso4
  dosed_water@so4 <- water@so4 + so4_dose

  # Total phosphate
  po4_dose <- h3po4
  dosed_water@tot_po4 <- water@tot_po4 + po4_dose

  # Total hypochlorite
  ocl_dose <- cl2 + naocl
  dosed_water@free_chlorine <- water@free_chlorine + ocl_dose

  # Total ammonia
  nh4_dose <- nh4oh + 2 * nh42so4
  dosed_water@tot_nh3 <- water@tot_nh3 + nh4_dose

  # Total carbonate
  co3_dose <- na2co3 + nahco3 + co2 + caco3
  dosed_water@tot_co3 <- water@tot_co3 + co3_dose

  # Calculate dosed TDS/IS/conductivity
  # Assume that all parameters can be determined by calculating new TDS.
  dosed_water@tds <- water@tds + convert_units(na_dose, "na", "M", "mg/L") +
    convert_units(cl_dose, "cl", "M", "mg/L") + convert_units(k_dose, "k", "M", "mg/L") +
    convert_units(ca_dose, "ca", "M", "mg/L") + convert_units(mg_dose, "mg", "M", "mg/L") +
    convert_units(co3_dose, "co3", "M", "mg/L") + convert_units(po4_dose, "po4", "M", "mg/L") +
    convert_units(so4_dose, "so4", "M", "mg/L") + convert_units(ocl_dose, "ocl", "M", "mg/L") +
    convert_units(nh4_dose, "nh4", "M", "mg/L")
  dosed_water@is <- correlate_ionicstrength(dosed_water@tds, from = "tds")
  dosed_water@cond <- correlate_ionicstrength(dosed_water@tds, from = "tds", to = "cond")

  # Calculate new pH, H+ and OH- concentrations
  ph <- solve_ph(dosed_water, so4_dose = so4_dose, na_dose = na_dose, ca_dose = ca_dose, mg_dose = mg_dose, cl_dose = cl_dose)

  if (softening_correction == TRUE & caco3 < 0) {
    ph_corrected <- (ph - 1.86) / 0.71 # WTP Model eq 5-62
    ph <- ph_corrected
  }

  # Convert from pH to concentration (not activity)
  h <- (10^-ph) / calculate_activity(1, water@is, water@temp)
  oh <- dosed_water@kw / (h * calculate_activity(1, water@is, water@temp)^2)

  # Correct eq constants
  ks <- correct_k(dosed_water)

  # Carbonate and phosphate ions and ocl ions
  alpha1 <- calculate_alpha1_carbonate(h, ks) # proportion of total carbonate as HCO3-
  alpha2 <- calculate_alpha2_carbonate(h, ks) # proportion of total carbonate as CO32-
  dosed_water@hco3 <- dosed_water@tot_co3 * alpha1
  dosed_water@co3 <- dosed_water@tot_co3 * alpha2

  alpha1p <- calculate_alpha1_phosphate(h, ks)
  alpha2p <- calculate_alpha2_phosphate(h, ks)
  alpha3p <- calculate_alpha3_phosphate(h, ks)

  dosed_water@h2po4 <- dosed_water@tot_po4 * alpha1p
  dosed_water@hpo4 <- dosed_water@tot_po4 * alpha2p
  dosed_water@po4 <- dosed_water@tot_po4 * alpha3p

  dosed_water@ocl <- dosed_water@free_chlorine * calculate_alpha1_hypochlorite(h, ks)
  dosed_water@nh4 <- dosed_water@tot_nh3 * calculate_alpha1_ammonia(h, ks)

  # Calculate new alkalinity
  dosed_water@alk_eq <- (dosed_water@hco3 + 2 * dosed_water@co3 + oh - h)
  dosed_water@alk <- convert_units(dosed_water@alk_eq, formula = "caco3", startunit = "eq/L", endunit = "mg/L CaCO3")

  # Compile complete dosed water data frame
  dosed_water@ph <- ph
  dosed_water@h <- h
  dosed_water@oh <- oh

  # update total hardness
  dosed_water@tot_hard <- convert_units(dosed_water@ca + dosed_water@mg, "caco3", "M", "mg/L CaCO3")

  return(dosed_water)
}

#' @rdname chemdose_ph
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain] The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "dosed_chem_water".
#' @param na_to_zero option to convert all NA values in the data frame to zeros. Default value is TRUE.
#'
#' @examples
#'
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   slice_head(n = 3) %>%
#'   define_water_chain() %>%
#'   chemdose_ph_chain(input_water = "defined_water", naoh = 5)
#'
#' example_df <- water_df %>%
#'   slice_head(n = 3) %>%
#'   define_water_chain() %>%
#'   mutate(
#'     hcl = c(2, 4, 6),
#'     Caustic = 20
#'   ) %>%
#'   chemdose_ph_chain(mgoh2 = c(20, 55), co2 = 4, naoh = Caustic)
#'
#' \donttest{
#' # Initialize parallel processing
#' library(furrr)
#'# plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   chemdose_ph_chain(naoh = 5)
#'
#' # Optional: explicitly close multisession processing
#'# plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns `chemdose_ph_chain` returns a data frame containing a water class column with updated pH, alkalinity, and ions post-chemical addition.

chemdose_ph_chain <- function(df, input_water = "defined_water", output_water = "dosed_chem_water",
                              hcl = "use_col", h2so4 = "use_col", h3po4 = "use_col", co2 = "use_col", naoh = "use_col",
                              na2co3 = "use_col", nahco3 = "use_col", caoh2 = "use_col", mgoh2 = "use_col",
                              cacl2 = "use_col", cl2 = "use_col", naocl = "use_col",
                              nh4oh = "use_col", nh42so4 = "use_col",
                              alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col", ach = "use_col",
                              caco3 = "use_col", caso4 = "use_col", softening_correction = "use_col", na_to_zero = TRUE) {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  hcl <- tryCatch(hcl, error = function(e) enquo(hcl))
  h2so4 <- tryCatch(h2so4, error = function(e) enquo(h2so4))
  h3po4 <- tryCatch(h3po4, error = function(e) enquo(h3po4))
  co2 <- tryCatch(co2, error = function(e) enquo(co2))
  naoh <- tryCatch(naoh, error = function(e) enquo(naoh))

  na2co3 <- tryCatch(na2co3, error = function(e) enquo(na2co3))
  nahco3 <- tryCatch(nahco3, error = function(e) enquo(nahco3))
  caoh2 <- tryCatch(caoh2, error = function(e) enquo(caoh2))
  mgoh2 <- tryCatch(mgoh2, error = function(e) enquo(mgoh2))

  cacl2 <- tryCatch(cacl2, error = function(e) enquo(cacl2))
  cl2 <- tryCatch(cl2, error = function(e) enquo(cl2))
  naocl <- tryCatch(naocl, error = function(e) enquo(naocl))

  nh4oh <- tryCatch(nh4oh, error = function(e) enquo(nh4oh))
  nh42so4 <- tryCatch(nh42so4, error = function(e) enquo(nh42so4))

  alum <- tryCatch(alum, error = function(e) enquo(alum))
  ferricchloride <- tryCatch(ferricchloride, error = function(e) enquo(ferricchloride))
  ferricsulfate <- tryCatch(ferricsulfate, error = function(e) enquo(ferricsulfate))
  ach <- tryCatch(ach, error = function(e) enquo(ach))
  caco3 <- tryCatch(caco3, error = function(e) enquo(caco3))
  caso4 <- tryCatch(caso4, error = function(e) enquo(caso4))

  softening_correction <- tryCatch(softening_correction, error = function(e) enquo(softening_correction))

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list(
    "hcl" = hcl, "h2so4" = h2so4, "h3po4" = h3po4, "co2" = co2, "naoh" = naoh,
    "na2co3" = na2co3, "nahco3" = nahco3, "caoh2" = caoh2, "mgoh2" = mgoh2,
    "cacl2" = cacl2, "cl2" = cl2, "naocl" = naocl,
    "nh4oh" = nh4oh, "nh42so4" = nh42so4,
    "alum" = alum, "ferricchloride" = ferricchloride, "ferricsulfate" = ferricsulfate, "ach" = ach, "caco3" = caco3, "caso4" = caso4,
    "softening_correction" = softening_correction
  ))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  
  # If na_to_zero is TRUE, change all NAs in the dataframe to zero
  if (na_to_zero == TRUE) {
    df[is.na(df)] <- 0
  }
  
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        hcl = if (final_names$hcl %in% names(.)) !!sym(final_names$hcl) else rep(0, nrow(.)),
        h2so4 = if (final_names$h2so4 %in% names(.)) !!sym(final_names$h2so4) else rep(0, nrow(.)),
        h3po4 = if (final_names$h3po4 %in% names(.)) !!sym(final_names$h3po4) else rep(0, nrow(.)),
        co2 = if (final_names$co2 %in% names(.)) !!sym(final_names$co2) else rep(0, nrow(.)),
        naoh = if (final_names$naoh %in% names(.)) !!sym(final_names$naoh) else rep(0, nrow(.)),
        na2co3 = if (final_names$na2co3 %in% names(.)) !!sym(final_names$na2co3) else rep(0, nrow(.)),
        nahco3 = if (final_names$nahco3 %in% names(.)) !!sym(final_names$nahco3) else rep(0, nrow(.)),
        caoh2 = if (final_names$caoh2 %in% names(.)) !!sym(final_names$caoh2) else rep(0, nrow(.)),
        mgoh2 = if (final_names$mgoh2 %in% names(.)) !!sym(final_names$mgoh2) else rep(0, nrow(.)),
        cacl2 = if (final_names$cacl2 %in% names(.)) !!sym(final_names$cacl2) else rep(0, nrow(.)),
        cl2 = if (final_names$cl2 %in% names(.)) !!sym(final_names$cl2) else rep(0, nrow(.)),
        naocl = if (final_names$naocl %in% names(.)) !!sym(final_names$naocl) else rep(0, nrow(.)),
        nh4oh = if (final_names$nh4oh %in% names(.)) !!sym(final_names$nh4oh) else rep(0, nrow(.)),
        nh42so4 = if (final_names$nh42so4 %in% names(.)) !!sym(final_names$nh42so4) else rep(0, nrow(.)),
        alum = if (final_names$alum %in% names(.)) !!sym(final_names$alum) else rep(0, nrow(.)),
        ferricchloride = if (final_names$ferricchloride %in% names(.)) !!sym(final_names$ferricchloride) else rep(0, nrow(.)),
        ferricsulfate = if (final_names$ferricsulfate %in% names(.)) !!sym(final_names$ferricsulfate) else rep(0, nrow(.)),
        ach = if (final_names$ach %in% names(.)) !!sym(final_names$ach) else rep(0, nrow(.)),
        caco3 = if (final_names$caco3 %in% names(.)) !!sym(final_names$caco3) else rep(0, nrow(.)),
        caso4 = if (final_names$caso4 %in% names(.)) !!sym(final_names$caso4) else rep(0, nrow(.)),
        softening_correction = if (final_names$softening_correction %in% names(.)) !!sym(final_names$softening_correction) else rep(FALSE, nrow(.))
      ),
      chemdose_ph
    ))
}

#' @rdname chemdose_ph
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain] The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_once(input_water = "balanced_water", naoh = 5)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     hcl = seq(1, 12, 1),
#'     naoh = 20
#'   ) %>%
#'   chemdose_ph_once(input_water = "balanced_water", mgoh2 = 55, co2 = 4)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns `chemdose_ph_once` returns a data frame with columns for pH and alkalinity post-chemical addition.
#'

chemdose_ph_once <- function(df, input_water = "defined_water",
                             hcl = "use_col", h2so4 = "use_col", h3po4 = "use_col", co2 = "use_col", naoh = "use_col",
                             na2co3 = "use_col", nahco3 = "use_col", caoh2 = "use_col", mgoh2 = "use_col",
                             cl2 = "use_col", naocl = "use_col", nh4oh = "use_col", nh42so4 = "use_col",
                             alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col", ach = "use_col",
                             caco3 = "use_col") {
  dose_chem <- dosed_chem_water <- temp <- tds <- kw <- toc <- estimated <- NULL # Quiet RCMD check global variable note
  
  # This allows for the function to process unquoted column names without erroring
  hcl <- tryCatch(hcl, error = function(e) enquo(hcl))
  h2so4 <- tryCatch(h2so4, error = function(e) enquo(h2so4))
  h3po4 <- tryCatch(h3po4, error = function(e) enquo(h3po4))
  co2 <- tryCatch(co2, error = function(e) enquo(co2))
  naoh <- tryCatch(naoh, error = function(e) enquo(naoh))
  
  na2co3 <- tryCatch(na2co3, error = function(e) enquo(na2co3))
  nahco3 <- tryCatch(nahco3, error = function(e) enquo(nahco3))
  caoh2 <- tryCatch(caoh2, error = function(e) enquo(caoh2))
  mgoh2 <- tryCatch(mgoh2, error = function(e) enquo(mgoh2))
  
  cl2 <- tryCatch(cl2, error = function(e) enquo(cl2))
  naocl <- tryCatch(naocl, error = function(e) enquo(naocl))
  nh4oh <- tryCatch(nh4oh, error = function(e) enquo(nh4oh))
  nh42so4 <- tryCatch(nh42so4, error = function(e) enquo(nh42so4))
  
  alum <- tryCatch(alum, error = function(e) enquo(alum))
  ferricchloride <- tryCatch(ferricchloride, error = function(e) enquo(ferricchloride))
  ferricsulfate <- tryCatch(ferricsulfate, error = function(e) enquo(ferricsulfate))
  ach <- tryCatch(ach, error = function(e) enquo(ach))
  caco3 <- tryCatch(caco3, error = function(e) enquo(caco3))
  
  output <- df %>%
    chemdose_ph_chain(
      input_water = input_water, output_water = "dosed_chem_water",
      hcl, h2so4, h3po4, co2, naoh,
      na2co3, nahco3, caoh2, mgoh2,
      cl2, naocl, nh4oh, nh42so4,
      alum, ferricchloride, ferricsulfate, ach, caco3
    ) %>%
    mutate(dose_chem = furrr::future_map(dosed_chem_water, convert_water)) %>%
  unnest(dose_chem) %>%
    select(-c(dosed_chem_water, temp, tds:kw, toc:estimated))
}
           