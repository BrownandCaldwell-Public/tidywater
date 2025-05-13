#' @title Determine TOC removal from coagulation
#'
#' @description This function applies the Edwards (1997) model to a water created by [define_water] to determine coagulated
#' DOC. Coagulated UVA is from U.S. EPA (2001) equation 5-80. Note that the models rely on pH of coagulation. If
#' only raw water pH is known, utilize [chemdose_ph] first.
#' For a single water use `chemdose_toc`; for a dataframe use `chemdose_toc_chain`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#' For most arguments in the `_chain` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
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
#' @returns `chemdose_toc` returns a single water class object with an updated DOC, TOC, and UV254 concentration.
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


#' @rdname chemdose_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain]. The df may include a column named for the coagulant being dosed,
#' and a column named for the set of coefficients to use.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, Water. Default is "coagulated_water".
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
#'   chemdose_toc_chain(input_water = "defined_water", alum = 30)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(FerricDose = seq(1, 12, 1)) %>%
#'   chemdose_toc_chain(ferricchloride = FerricDose, coeff = "Ferric")
#'
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(ferricchloride = seq(1, 12, 1)) %>%
#'   chemdose_toc_chain(coeff = "Ferric")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns `chemdose_toc_chain` returns a data frame containing a water class column with updated DOC, TOC, and UV254 concentrations.

chemdose_toc_chain <- function(df, input_water = "defined_water", output_water = "coagulated_water",
                               alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col",
                               coeff = "use_col") {
  # This allows for the function to process unquoted column names without erroring
  alum <- tryCatch(alum, error = function(e) enquo(alum))
  ferricchloride <- tryCatch(ferricchloride, error = function(e) enquo(ferricchloride))
  ferricsulfate <- tryCatch(ferricsulfate, error = function(e) enquo(ferricsulfate))
  coeff <- tryCatch(coeff, error = function(e) enquo(coeff))

  validate_water_helpers(df, input_water)
  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list(
    "alum" = alum, "ferricchloride" = ferricchloride,
    "ferricsulfate" = ferricsulfate,
    "coeff" = coeff
  ))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        # This logic needed for any argument that has a default
        alum = if (final_names$alum %in% names(.)) !!sym(final_names$alum) else (rep(0, nrow(.))),
        ferricchloride = if (final_names$ferricchloride %in% names(.)) !!sym(final_names$ferricchloride) else (rep(0, nrow(.))),
        ferricsulfate = if (final_names$ferricsulfate %in% names(.)) !!sym(final_names$ferricsulfate) else (rep(0, nrow(.))),
        coeff = if (final_names$coeff %in% names(.)) !!sym(final_names$coeff) else (rep("Alum", nrow(.)))
      ),
      chemdose_toc
    ))
}
