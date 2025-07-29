#' @title Determine TOC removal from coagulation
#'
#' @description This function applies the Edwards (1997) model to a water created by [define_water] to determine coagulated
#' DOC. Model assumes all particulate TOC is removed; therefore TOC = DOC in output.
#' Coagulated UVA is from U.S. EPA (2001) equation 5-80. Note that the models rely on pH of coagulation. If
#' only raw water pH is known, utilize [chemdose_ph] first.
#' For a single water use `chemdose_toc`; for a dataframe use `chemdose_toc_df`.
#' Use `pluck_cols = TRUE` to get values from the output water as new dataframe columns.
#' For most arguments in the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @param water Source water object of class "water" created by [define_water]. Water must include ph, doc, and uv254
#' @param alum Amount of hydrated aluminum sulfate added in mg/L: Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Amount of ferric chloride added in mg/L: FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param coeff String specifying the Edwards coefficients to be used from "Alum", "Ferric", "General Alum", "General Ferric", or "Low DOC" or
#' data frame of coefficients, which must include: k1, k2, x1, x2, x3, b
#' @param caoh2 Option to add caoh2 in mg/L to soften the water. Will predict DOC, TOC, UV254 using a modified equation (see reference list). Defaults to zero.
#'
#' @seealso [chemdose_ph]
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
#' dosed_water <- chemdose_ph(water, alum = 10, h2so4 = 10) %>%
#'   chemdose_toc(alum = 10, coeff = data.frame(
#'     x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068
#'   ))
#'
#' @export
#'
#' @returns `chemdose_toc` returns a single water class object with an updated DOC, TOC, and UV254 concentration.
#'
chemdose_toc <- function(water, alum = 0, ferricchloride = 0, ferricsulfate = 0, coeff = "Alum", caoh2 = 0) {
  validate_water(water, c("ph", "doc", "uv254"))

  if (is.character(coeff)) {
    edwardscoeff <- tidywater::edwardscoeff
    coeff <- subset(edwardscoeff, edwardscoeff$ID == coeff)
    if (nrow(coeff) != 1) {
      stop("coeff must be one of 'Alum', 'Ferric', 'General Alum', 'General Ferric', or 'Low DOC' or coefficients can be manually specified with a vector.")
    }
  } else if (is.data.frame(coeff)) {
    expected_cols <- c("k1", "k2", "x1", "x2", "x3", "b")
    if (any(is.na(coeff)) || !all(expected_cols %in% colnames(coeff))) {
      stop("coeff must be specified as a data frame and include 'k1', 'k2', 'x1', 'x2', 'x3', and 'b' or choose coefficients from Edwards model using a string.")
    }
  } else {
    stop("coeff must be specified with a string or data frame. See documentation for acceptable formats.")
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
  if (caoh2 > 0) {
    water <- chemdose_ph(water, caoh2 = caoh2)
    
    removed <- (4.657*10^-4) * water@toc^1.3843 * water@ph^2.2387 * caoh2^0.1707 * (1 + coag2)^2.4402
    removed <- removed / 0.87 # apply correction factor
    
    if (coag == 0) {
      water@doc <- water@doc
      water@uv254 <- water@uv254
    } else {
      water@uv254 <- 0.01685 * removed^0.8367 * calc_suva(water@doc, water@uv254)^1.2501
      water@doc <- water@doc - removed
      water@toc <- water@doc - removed
    }
  } else {
    nonadsorb <- water@doc * (coeff$k1 * calc_suva(water@doc, water@uv254) + coeff$k2)
    
    sterm <- (1 - calc_suva(water@doc, water@uv254) * coeff$k1 - coeff$k2)
    xterm <- (coeff$x1 * water@ph + coeff$x2 * water@ph^2 + coeff$x3 * water@ph^3)
    b <- coeff$b
    
    # Rearrangement of equation from wolfram alpha
    adsorb <- (sqrt(b^2 * (water@doc * sterm - coag * xterm)^2 + 2 * b * (coag * xterm + water@doc * sterm) + 1) -
                 b * coag * xterm + b * water@doc * sterm - 1) /
      (2 * b) 
    
    if (coag == 0) {
      water@doc <- water@doc
      water@uv254 <- water@uv254
    } else {
      water@doc <- nonadsorb + adsorb
      water@toc <- nonadsorb + adsorb
      water@uv254 <- 5.716 * water@uv254^1.0894 * coag2^0.306 * water@ph^-.9513
    }
  }

  return(water)
}


#' @rdname chemdose_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column named for the coagulant being dosed,
#' and a column named for the set of coefficients to use.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "coagulated".
#' @param pluck_cols Extract water slots modified by the function (doc, toc, uv254) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#' \donttest{
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(FerricDose = seq(1, 12, 1)) %>%
#'   chemdose_toc_df(ferricchloride = FerricDose, coeff = "Ferric")
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(ferricchloride = seq(1, 12, 1)) %>%
#'   chemdose_toc_df(coeff = "Ferric", pluck_cols = TRUE)
#' }
#'
#' @export
#'
#' @returns `chemdose_toc_df` returns a data frame containing a water class column with updated DOC, TOC, and UV254
#' concentrations. Optionally, it also adds columns for each of those slots individually.
#'
chemdose_toc_df <- function(df, input_water = "defined", output_water = "coagulated",
                            pluck_cols = FALSE, water_prefix = TRUE,
                            alum = "use_col", ferricchloride = "use_col", ferricsulfate = "use_col", caoh2 = "use_col",
                            coeff = "use_col") {
  # This allows for the function to process unquoted column names without erroring
  alum <- tryCatch(alum, error = function(e) enquo(alum))
  ferricchloride <- tryCatch(ferricchloride, error = function(e) enquo(ferricchloride))
  ferricsulfate <- tryCatch(ferricsulfate, error = function(e) enquo(ferricsulfate))
  caoh2 <- tryCatch(caoh2, error = function(e) enquo(caoh2))
  coeff <- tryCatch(coeff, error = function(e) enquo(coeff))

  validate_water_helpers(df, input_water)
  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list(
    "alum" = alum, "ferricchloride" = ferricchloride,
    "ferricsulfate" = ferricsulfate,
    "caoh2" = caoh2,
    "coeff" = coeff
  ))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df, final_names,
    list(alum = 0, ferricchloride = 0, ferricsulfate = 0, coeff = "Alum")
  )
  df <- defaults_added$data

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    chemdose_toc(
      water = df[[input_water]][[i]],
      alum = df[[final_names$alum]][i],
      ferricchloride = df[[final_names$ferricchloride]][i],
      ferricsulfate = df[[final_names$ferricsulfate]][i],
      caoh2 = df[[final_names$caoh2]][i],
      coeff = df[[final_names$coeff]][i]
    )
  })

  output <- df[, !names(df) %in% defaults_added$defaults_used]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("toc", "doc", "uv254"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  return(output)
}
