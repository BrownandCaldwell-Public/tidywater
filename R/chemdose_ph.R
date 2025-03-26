#' @title Calculate new pH and ion balance after chemical addition
#'
#' @description \code{chemdose_ph} calculates the new pH, alkalinity, and ion balance of a water based on different chemical
#' additions.
#'
#' @details The function takes an object of class "water" created by \code{\link{define_water}} and user-specified
#' chemical additions and returns a new object of class "water" with updated water quality.
#' Units of all chemical additions are in mg/L as chemical (not as product).
#'
#' \code{chemdose_ph} works by evaluating all the user-specified chemical additions and solving for what the new pH
#' must be using \code{uniroot} to satisfy the principle of electroneutrality in pure water while correcting for the existing alkalinity
#' of the water that the chemical is added to. Multiple chemicals can be added simultaneously or each addition can be
#' modeled independently through sequential doses.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
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
#' @returns A water class object with updated pH, alkalinity, and ions post-chemical addition.
#'
chemdose_ph <- function(water, hcl = 0, h2so4 = 0, h3po4 = 0, co2 = 0,
                        naoh = 0, caoh2 = 0, mgoh2 = 0,
                        na2co3 = 0, nahco3 = 0, caco3 = 0, cacl2 = 0,
                        cl2 = 0, naocl = 0, nh4oh = 0, nh42so4 = 0,
                        alum = 0, ferricchloride = 0, ferricsulfate = 0, ach = 0,
                        softening_correction = FALSE) {
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
  ca_dose <- caoh2 + cacl2 + caco3
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
  so4_dose <- h2so4 + 3 * alum + 3 * ferricsulfate + nh42so4
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
  dosed_water@applied_treatment <- paste(dosed_water@applied_treatment, "_chemdosed", sep = "")

  # update total hardness
  dosed_water@tot_hard <- convert_units(dosed_water@ca + dosed_water@mg, "caco3", "M", "mg/L CaCO3")

  return(dosed_water)
}

#' Apply `chemdose_ph` function and output a dataframe
#'
#' This function allows \code{\link{chemdose_ph}} to be added to a piped data frame.
#' Its output is a data frame with updated ions and pH.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a column(s) name matching a valid chemical(s), the function will dose that chemical(s) in addition to the
#' ones specified in the function's arguments.
#' The column names must match the chemical names as displayed in \code{\link{chemdose_ph}}.
#' To see which chemicals can be passed into the function, see \code{\link{chemdose_ph}}.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param hcl Hydrochloric acid: HCl -> H + Cl
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3po4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param co2 Carbon Dioxide CO2 (gas) + H2O -> H2CO3*
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param nh4oh Amount of ammonium hydroxide added in mg/L as N: NH4OH -> NH4 + OH
#' @param nh42so4 Amount of ammonium sulfate added in mg/L as N: (NH4)2SO4 -> 2NH4 + SO4
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param ach Amount of aluminum chlorohydrate added in mg/L: Al2(OH)5Cl*2H2O + HCO3 -> 2Al(OH)3(am) + Cl + 2H2O + CO2
#' @param caco3 Amount of calcium carbonate added (or removed) in mg/L: CaCO3 -> Ca + CO3
#'
#' @seealso \code{\link{chemdose_ph}}
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
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_once(input_water = "balanced_water", naoh = 5)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns A data frame with updated pH, alkalinity, and ions post-chemical addition.
#'

chemdose_ph_once <- function(df, input_water = "defined_water",
                             hcl = 0, h2so4 = 0, h3po4 = 0, co2 = 0, naoh = 0,
                             na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                             cl2 = 0, naocl = 0, nh4oh = 0, nh42so4 = 0,
                             alum = 0, ferricchloride = 0, ferricsulfate = 0, ach = 0, caco3 = 0) {
  dose_chem <- dosed_chem_water <- NULL # Quiet RCMD check global variable note
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
    select(-dosed_chem_water)
}

#' Apply `chemdose_ph` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{chemdose_ph}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' Ions and pH will be updated based on input chemical doses.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a column(s) name matching a valid chemical(s), the function will dose that chemical(s) in addition to the
#' ones specified in the function's arguments.
#' The column names must match the chemical names as displayed in \code{\link{chemdose_ph}}.
#' To see which chemicals can be passed into the function, see \code{\link{chemdose_ph}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "dosed_chem_water".
#' @param hcl Hydrochloric acid: HCl -> H + Cl
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3po4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param co2 Carbon Dioxide CO2 (gas) + H2O -> H2CO3*
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param nh4oh Amount of ammonium hydroxide added in mg/L as N: NH4OH -> NH4 + OH
#' @param nh42so4 Amount of ammonium sulfate added in mg/L as N: (NH4)2SO4 -> 2NH4 + SO4
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param ach Amount of aluminum chlorohydrate added in mg/L: Al2(OH)5Cl*2H2O + HCO3 -> 2Al(OH)3(am) + Cl + 2H2O + CO2
#' @param caco3 Amount of calcium carbonate added (or removed) in mg/L: CaCO3 -> Ca + CO3
#'
#' @seealso \code{\link{chemdose_ph}}
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
#'   chemdose_ph_chain(input_water = "balanced_water", naoh = 5)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     hcl = seq(1, 12, 1),
#'     naoh = 20
#'   ) %>%
#'   chemdose_ph_chain(input_water = "balanced_water", mgoh2 = 55, co2 = 4)
#'
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(input_water = "balanced_water", naoh = 5)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns A data frame containing a water class column with updated pH, alkalinity, and ions post-chemical addition.

chemdose_ph_chain <- function(df, input_water = "defined_water", output_water = "dosed_chem_water",
                              hcl = 0, h2so4 = 0, h3po4 = 0, co2 = 0, naoh = 0,
                              na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                              cl2 = 0, naocl = 0, nh4oh = 0, nh42so4 = 0,
                              alum = 0, ferricchloride = 0, ferricsulfate = 0, ach = 0, caco3 = 0) {
  ID <- NULL # Quiet RCMD check global variable note

  validate_water_helpers(df, input_water)

  dosable_chems <- tibble(
    hcl, h2so4, h3po4, co2, naoh,
    na2co3, nahco3, caoh2, mgoh2,
    cl2, naocl, nh4oh, nh42so4,
    alum, ferricchloride, ferricsulfate, ach, caco3
  )

  chem_inputs_arg <- dosable_chems %>%
    select_if(~ any(. > 0))

  chem_inputs_col <- df %>%
    subset(select = names(df) %in% names(dosable_chems)) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(chem_inputs_col) - 1 == 0 & length(chem_inputs_arg) == 0) {
    warning("No chemical dose found. Create dose column, enter a dose argument, or check availbility of chemical in the chemdose_ph function.")
  }

  if (length(chem_inputs_col) > 0 & length(chem_inputs_arg) > 0) {
    if (any(names(chem_inputs_arg) %in% names(chem_inputs_col))) {
      stop("At least one chemical was dosed as both a function argument and a data frame column. Remove your chemical(s) from one of these inputs.")
    }
  }

  if (nrow(chem_inputs_arg) == 1) {
    chem_doses <- chem_inputs_col %>%
      cross_join(chem_inputs_arg)
    # Add missing chemical columns
    chem2 <- dosable_chems %>%
      subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
      cross_join(chem_doses) %>%
      mutate(ID = row_number())
  } else if (nrow(chem_inputs_arg) > 1) {
    chem_doses <- chem_inputs_col %>%
      cross_join(chem_inputs_arg)
    chem2 <- dosable_chems %>%
      subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
      unique() %>%
      cross_join(chem_doses)
  }

  output <- df %>%
    subset(select = !names(df) %in% names(chem_inputs_col)) %>%
    mutate(ID = row_number()) %>%
    left_join(chem2, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        hcl = hcl,
        h2so4 = h2so4,
        h3po4 = h3po4,
        co2 = co2,
        naoh = naoh,
        na2co3 = na2co3,
        nahco3 = nahco3,
        caoh2 = caoh2,
        mgoh2 = mgoh2,
        cl2 = cl2,
        naocl = naocl,
        nh4oh = nh4oh,
        nh42so4 = nh4oh,
        alum = alum,
        ferricchloride = ferricchloride,
        ferricsulfate = ferricsulfate,
        ach = ach,
        caco3 = caco3
      ),
      chemdose_ph
    )) %>%
    select(!any_of(names(dosable_chems)), any_of(names(chem_doses)))
}
