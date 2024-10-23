#' @title Determine TOC removal from coagulation
#'
#' @description This function applies the Edwards (1997) model to a water created by \code{\link{define_water}} to determine coagulated
#' DOC. Coagulated UVA is from U.S. EPA (2001) equation 5-80. Note that the models rely on pH of coagulation. If
#' only raw water pH is known, utilize \code{\link{chemdose_ph}} first.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph, doc, and uv254
#' @param alum Amount of hydrated aluminum sulfate added in mg/L: Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Amount of ferric chloride added in mg/L: FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param coeff String specifying the Edwards coefficients to be used from "Alum", "Ferric", "General Alum", "General Ferric", or "Low DOC" or
#' named vector of coefficients, which must include: k1, k2, x1, x2, x3, b
#'
#' @seealso \code{\link{chemdose_ph}}
#'
#' @source Edwards (1997)
#' @source U.S. EPA (2001)
#' @source See reference list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 3.7, doc = 3.5, uv254 = .1)
#' dosed_water <- chemdose_ph(water, alum = 30) %>%
#'   chemdose_toc(alum = 30, coeff = "Alum")
#'
#' dosed_water <- chemdose_ph(water, ferricsulfate = 30) %>%
#'   chemdose_toc(ferricsulfate = 30, coeff = "Ferric")
#'
#' dosed_water <- chemdose_ph(water, alum = 10, h2so4 = 10) %>%
#'   chemdose_toc(alum = 10, coeff = c(
#'     "x1" = 280, "x2" = -73.9, "x3" = 4.96,
#'     "k1" = -0.028, "k2" = 0.23, "b" = 0.068
#'   ))
#'
#' @export
#'
#' @returns A water class object with an updated DOC, TOC, and UV254 concentration.
#'
chemdose_toc <- function(water, alum = 0, ferricchloride = 0, ferricsulfate = 0, coeff = "Alum") {
  validate_water(water, c("ph", "doc", "uv254"))

  if (is.character(coeff)) {
    edwardscoeff <- tidywater::edwardscoeff
    coeffs <- subset(edwardscoeff, edwardscoeff$ID == coeff)
    if (nrow(coeffs) != 1) {
      stop("coeff must be one of 'Alum', 'Ferric', 'General Alum', 'General Ferric', or 'Low DOC' or coefficients can be manually specified with a vector.")
    }
  } else if (is.numeric(coeff)) {
    coeffs <- data.frame(k1 = coeff["k1"], k2 = coeff["k2"], x1 = coeff["x1"], x2 = coeff["x2"], x3 = coeff["x3"], b = coeff["b"])
    if (any(is.na(coeffs))) {
      stop("coeff must be specified as a named vector and include 'k1', 'k2', 'x1', 'x2', 'x3', and 'b' or choose coefficients from Edwards model using a string.")
    }
  } else {
    stop("coeffs must be specified with a string or named vector. See documentation for acceptable formats.")
  }

  if (alum <= 0 & ferricchloride <= 0 & ferricsulfate <= 0) {
    warning("No coagulants dosed. Final water will equal input water.")
  } else if (alum > 0 & (ferricchloride > 0 | ferricsulfate > 0)) {
    warning("Both alum and ferric coagulants entered.")
  } else if ((ferricchloride > 0 | ferricsulfate > 0) & any(grepl("Alum", coeff))) {
    warning("Ferric coagulants used with coefficients fit on Alum. Check 'coeff' argument.")
  } else if (alum > 0 & any(grepl("Ferric", coeff))) {
    warning("Alum used with coefficients fit on Ferric. Check 'coeff' argument.")
  }


  # Alum - hydration included
  alum <- convert_units(alum, "alum", endunit = "mM")
  # Ferric chloride
  ferricchloride <- convert_units(ferricchloride, "ferricchloride", endunit = "mM")
  # Ferric sulfate
  ferricsulfate <- convert_units(ferricsulfate, "ferricsulfate", endunit = "mM")

  # Convert coagulant units to mMol/L as Al3+ or Fe3+ for DOC model
  coag <- alum * 2 + ferricchloride * 1 + ferricsulfate * 2
  # Convert to meq/L for UV model
  coag2 <- alum * 2 * 3 + ferricchloride * 1 * 3 + ferricsulfate * 2 * 3

  # Edwards calculations
  nonadsorb <- water@doc * (coeffs$k1 * calc_suva(water@doc, water@uv254) + coeffs$k2)

  sterm <- (1 - calc_suva(water@doc, water@uv254) * coeffs$k1 - coeffs$k2)
  xterm <- (coeffs$x1 * water@ph + coeffs$x2 * water@ph^2 + coeffs$x3 * water@ph^3)
  b <- coeffs$b

  # Rearrangement of equation from wolfram alpha
  adsorb <- (sqrt(b^2 * (water@doc * sterm - coag * xterm)^2 + 2 * b * (coag * xterm + water@doc * sterm) + 1) -
    b * coag * xterm + b * water@doc * sterm - 1) /
    (2 * b)

  if (coag == 0) {
    water@doc <- water@doc
    water@uv254 <- water@uv254
  } else {
    if (!is.na(water@toc) & water@toc >= water@doc) {
      water@toc <- water@toc - water@doc + nonadsorb + adsorb
    } else if (!is.na(water@toc) & water@toc < water@doc) {
      warning("TOC of input water less than DOC. TOC will be set equal to DOC.")
      water@toc <- nonadsorb + adsorb
    } else if (is.na(water@toc)) {
      warning("Input water TOC not specified. Output water TOC will be NA.")
      water@toc <- NA_real_
    }

    water@doc <- nonadsorb + adsorb
    water@uv254 <- 5.716 * water@uv254^1.0894 * coag2^0.306 * water@ph^-.9513
  }

  water@applied_treatment <- paste(water@applied_treatment, "_tocremoved", sep = "")

  return(water)
}


#' Apply `chemdose_toc` function and output a data frame
#'
#' This function allows \code{\link{chemdose_toc}} to be added to a piped data frame.
#' Its output is a data frame with updated TOC, DOC, and UV254.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a column(s) name matching a valid coagulant(s), the function will dose that coagulant(s). Note:
#' The function can only dose a coagulant as either a column or from the function arguments, not both.
#'
#' The column names must match the coagulant names as displayed in \code{\link{chemdose_toc}}.
#' To see which coagulants can be passed into the function, see \code{\link{chemdose_toc}}.
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
#' \code{\link{define_water_chain}}. The df may include a column named for the coagulant being dosed,
#' and a column named for the set of coefficients to use.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param coeff String specifying the Edwards coefficients to be used from "Alum", "Ferric", "General Alum", "General Ferric", or "Low DOC" or
#' named vector of coefficients, which must include: k1, k2, x1, x2, x3, b
#'
#' @seealso \code{\link{chemdose_toc}}
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
#'   chemdose_ph_chain(alum = 30) %>%
#'   chemdose_toc_once(input_water = "dosed_chem_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     ferricchloride = seq(1, 12, 1),
#'     coeff = "Ferric"
#'   ) %>%
#'   chemdose_toc_once(input_water = "balanced_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_toc_once(input_water = "balanced_water", alum = 40, coeff = "General Alum")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(ferricchloride = seq(1, 12, 1)) %>%
#'   chemdose_toc_once(input_water = "balanced_water", coeff = "Ferric")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns A data frame with an updated DOC, TOC, and UV254 concentration.

chemdose_toc_once <- function(df, input_water = "defined_water",
                              alum = 0, ferricchloride = 0, ferricsulfate = 0, coeff = "Alum") {
  dosed_chem_water <- dose_chem <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    chemdose_toc_chain(
      input_water = input_water, output_water = "dosed_chem_water",
      alum, ferricchloride, ferricsulfate, coeff
    ) %>%
    mutate(dose_chem = furrr::future_map(dosed_chem_water, convert_water)) %>%
    unnest(dose_chem) %>%
    select(-dosed_chem_water)
}

#' Apply `chemdose_toc` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{chemdose_toc}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' TOC, DOC, and UV254 will be updated based on input chemical doses.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a coagulant(s) name matching a valid coagulant(s), the function will dose that coagulant(s). Note:
#' The function can only dose a coagulant either a column or from the function arguments, not both.
#'
#' The column names must match the chemical names as displayed in \code{\link{chemdose_toc}}.
#' To see which chemicals can be passed into the function, see \code{\link{chemdose_toc}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column named for the coagulant being dosed,
#' and a column named for the set of coefficients to use.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, Water. Default is "coagulated_water".
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param coeff String specifying the Edwards coefficients to be used from "Alum", "Ferric", "General Alum", "General Ferric", or "Low DOC" or
#' named vector of coefficients, which must include: k1, k2, x1, x2, x3, b
#'
#' @seealso \code{\link{chemdose_toc}}
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
#'   chemdose_ph_chain(alum = 30) %>%
#'   chemdose_toc_chain(input_water = "dosed_chem_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     ferricchloride = seq(1, 12, 1),
#'     coeff = "Ferric"
#'   ) %>%
#'   chemdose_toc_chain(input_water = "balanced_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_toc_chain(input_water = "balanced_water", alum = 40, coeff = "General Alum")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(ferricchloride = seq(1, 12, 1)) %>%
#'   chemdose_toc_chain(input_water = "balanced_water", coeff = "Ferric")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export
#'
#' @returns A data frame containing a water class column with updated DOC, TOC, and UV254 concentrations.

chemdose_toc_chain <- function(df, input_water = "defined_water", output_water = "coagulated_water",
                               alum = 0, ferricchloride = 0, ferricsulfate = 0, coeff = "Alum") {
  ID <- NULL # Quiet RCMD check global variable note
  dosable_chems <- tibble(alum, ferricchloride, ferricsulfate)

  chem_inputs_arg <- dosable_chems %>%
    select_if(~ any(. > 0))

  chem_inputs_col <- df %>%
    subset(select = names(df) %in% names(dosable_chems)) %>%
    # add row number for joining
    mutate(ID = row_number())


  if (length(chem_inputs_col) - 1 == 0 & length(chem_inputs_arg) == 0) {
    warning("No chemical dose found. Create dose column, enter a dose argument, or check availbility of chemical in the chemdose_ph function.")
  }

  if (length(chem_inputs_col) > 1 & length(chem_inputs_arg) > 0) {
    stop("Coagulants were dosed as both a function argument and a data frame column. Choose one input method.")
  }
  if (length(chem_inputs_col) > 2 | length(chem_inputs_arg) > 1) {
    stop("Multiple coagulants dosed. Choose one coagulant.")
  }

  chem_doses <- chem_inputs_col %>%
    cross_join(chem_inputs_arg)
  chem2 <- dosable_chems %>%
    subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
    cross_join(chem_doses)

  if (length(df$coeff) > 0) {
    coeff <- tibble(coeff = df$coeff) %>%
      mutate(ID = row_number())
    chem3 <- chem2 %>%
      left_join(coeff, by = "ID")
  } else if (length(coeff) == 1) {
    chem3 <- chem2 %>%
      mutate(coeff = list(coeff))
  } else if (is.numeric(coeff) & length(coeff) == 6) {
    chem3 <- chem2 %>%
      mutate(coeff = list(coeff))
  } else {
    stop("coeffs must be specified with a string or named vector. See documentation for acceptable formats.")
  }

  output <- df %>%
    subset(select = !names(df) %in% c("alum", "ferricchloride", "ferricsulfate", "coeff")) %>%
    mutate(ID = row_number()) %>%
    left_join(chem3, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        alum = alum,
        ferricchloride = ferricchloride,
        ferricsulfate = ferricsulfate,
        coeff = coeff
      ),
      chemdose_toc
    )) %>%
    select(!any_of(names(dosable_chems)), any_of(names(chem_doses)))
}
