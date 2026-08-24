#' @title Determine TOC removal from biofiltration using Terry & Summers BDOC model or Peterson & Summers TOC model
#'
#' @description This function applies the Terry model to a water created by [define_water] to determine biofiltered
#' DOC (mg/L). When the reuse argument is TRUE, the Peterson and Summers (2021) model is used to determine biofiltered TOC (mg/L).
#' All particulate TOC is assumed to be removed so TOC = DOC.
#' For a single water use `biofilter_toc`; for a dataframe use `biofilter_toc_df`.
#' Use `pluck_cols = TRUE` to get values from the output water as new dataframe columns.
#' For most arguments in the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @param water Source water object of class "water" created by [define_water].
#' @param ebct The empty bed contact time (min) used for the biofilter.
#' @param ozonated Logical; TRUE if the water is ozonated (default), FALSE otherwise.
#' @param ozone_dose The ozone dose (mg/L) applied to the water. Only used if `reuse = TRUE`.
#' @param reuse Logical; TRUE if the water is effluent from a reuse treatment process, FALSE otherwise. 
#' If TRUE, the Peterson and Summers (2021) model is used to determine TOC removal. If FALSE, the Terry and Summers (2018) model is used to determine BDOC removal.
#'
#'
#' @source Terry and Summers 2018, Peterson and Summers 2021
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1) %>%
#'   biofilter_toc(ebct = 10, ozonated = FALSE)
#'
#' @returns  `biofilter_toc` returns water class object with modeled DOC removal from biofiltration.
#' @export
#'
biofilter_toc <- function(water, ebct, ozonated = TRUE, ozone_dose = 0, reuse = FALSE) {
  if (!is.logical(ozonated)) {
    cli::cli_abort("{.arg ozonated} must be {.code TRUE} or {.code FALSE}.")
  }

  temp <- water@temp

  validate_water(water, "doc")

  if (reuse == TRUE) {
    # Peterson and Summers (2021)
    spec_ozone <- ozone_dose / water@toc
    if (spec_ozone < 0 | spec_ozone > 1.2) {
      cli::cli_warn("Peterson and Summers model was validated for specific ozone range of 0 - 1.2. Model accuracy may be reduced outside this range.")
    }
    if (ebct < 5 | ebct > 30) {
      cli::cli_warn("Peterson and Summers model was validated for EBCT range of 5 - 30 min. Model accuracy may be reduced outside this range.")
    }

    # assumes media type associated with 0.0873 is BAC
    toc_removal_percent <- 0.0873 + 0.0527 * spec_ozone + 0.0393 * ebct + 0.0139 * spec_ozone * ebct + 0.1323 
    # equation is labeled as TOC Removal (%) but I'm a little unsure whether this output is 80 or 0.80 for example. Right now, assuming output is 80 as in 80%
    TOC_removed <- water@toc * (toc_removal_percent / 100) # this model was for total organic carbon removal, not biodegradable

    # Update water object with new TOC and DOC values
    water@toc <- water@toc - TOC_removed
    water@doc <- max(water@doc - TOC_removed, 0) # Ensure DOC does not go below 0, or a better way to handle this?
    water@bdoc <- NA_real_ # Peterson and Summers only calcs TOC so not updating BDOC, or can assume a fraction?

  } else {
    # Terry and Summers (2018)

    # Define BDOC fractions
    BDOC_fraction_nonozonated <- 0.2
    BDOC_fraction_ozonated <- 0.3

    # Determine BDOC fraction and rate constant k' based on temperature and ozonation
    if (ozonated) {
      BDOC_fraction <- BDOC_fraction_ozonated
      if (temp <= 10) {
        k <- 0.03
      } else if (temp <= 20) {
        k <- 0.06
      } else {
        k <- 0.15
      }
    } else {
      BDOC_fraction <- BDOC_fraction_nonozonated
      if (temp <= 10) {
        k <- 0.03
      } else if (temp <= 20) {
        k <- 0.09
      } else {
        k <- 0.11
      }
    }

    # Calculate BDOC influent concentration
    BDOC_inf <- BDOC_fraction * water@doc

    # Calculate BDOC effluent concentration using the exponential decay model
    BDOC_eff <- BDOC_inf * exp(-k * ebct)

    # Calculate TOC removal percentage
    BDOC_removed <- (BDOC_inf - BDOC_eff)

    # Update water object with new TOC and DOC values
    doc_eff <- water@doc - BDOC_removed
    water@doc <- doc_eff
    water@toc <- doc_eff
    water@bdoc <- BDOC_eff
  }

  return(water)
  }


#' @rdname biofilter_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column indicating the EBCT or whether the water is ozonated.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "biofiltered".
#' @param pluck_cols Extract water slots modified by the function (doc, toc, bdoc) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   biofilter_toc_df(input_water = "defined", ebct = c(10, 15), ozonated = FALSE)
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(
#'     BiofEBCT = c(10, 10, 10, 15, 15, 15, 20, 20, 20, 25, 25, 25),
#'     ozonated = c(rep(TRUE, 6), rep(FALSE, 6))
#'   ) %>%
#'   biofilter_toc_df(input_water = "defined", ebct = BiofEBCT)
#'
#' @export
#'
#' @returns `biofilter_toc_df` returns a data frame containing a water class column with updated DOC, TOC, and BDOC
#' concentrations. Optionally, it also adds columns for each of those slots individually.

biofilter_toc_df <- function(
  df,
  input_water = "defined",
  output_water = "biofiltered",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  ebct = "use_col",
  ozonated = "use_col",
  ozone_dose = "use_col",
  reuse = "use_col"
) {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  ozonated <- tryCatch(ozonated, error = function(e) enquo(ozonated))
  ozone_dose <- tryCatch(ozone_dose, error = function(e) enquo(ozone_dose))
  reuse <- tryCatch(reuse, error = function(e) enquo(reuse))

  arguments <- construct_helper(df, list("ebct" = ebct, "ozonated" = ozonated, "ozone_dose" = ozone_dose, "reuse" = reuse))
  final_names <- arguments$final_names
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(df, final_names, list(ozonated = TRUE, ozone_dose = 0, reuse = FALSE))
  df <- defaults_added$data

  warning_counts <- list()
  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    withCallingHandlers(
      biofilter_toc(
        water = df[[input_water]][[i]],
        ebct = df[[final_names$ebct]][i],
        ozonated = df[[final_names$ozonated]][i],
        ozone_dose = df[[final_names$ozone_dose]][i],
        reuse = df[[final_names$reuse]][i]
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) +1L
        invokeRestart("muffleWarning")
      }
    )
  })

  for(msg in names(warning_counts)){
    cli::cli_warn("{msg} ({warning_counts[[msg]]] row{?s} affected.)")
  }

  output <- df[, !names(df) %in% defaults_added$defaults_used]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("toc", "doc", "bdoc"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  return(output)
}
