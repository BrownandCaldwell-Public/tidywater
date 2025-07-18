#' @title Determine TOC removal from biofiltration using Terry & Summers BDOC model
#'
#' @description This function applies the Terry model to a water created by [define_water] to determine biofiltered
#' DOC (mg/L). All particulate TOC is assumed to be removed so TOC = DOC.
#' For a single water use `biofilter_toc`; for a dataframe use `biofilter_toc_chain`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#' For most arguments in the `_chain` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param water Source water object of class "water" created by [define_water].
#' @param ebct The empty bed contact time (min) used for the biofilter.
#' @param ozonated Logical; TRUE if the water is ozonated (default), FALSE otherwise.
#'
#' @source Terry and Summers 2018
#' @examples
#' library(tidywater)
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1) %>%
#'   biofilter_toc(ebct = 10, ozonated = FALSE)
#'
#' @returns  `biofilter_toc` returns water class object with modeled DOC removal from biofiltration.
#' @export
#'
biofilter_toc <- function(water, ebct, ozonated = TRUE) {
  if (!is.logical(ozonated)) {
    stop("ozonate must be set to TRUE or FALSE.")
  }

  temp <- water@temp

  validate_water(water, "doc")

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

  return(water)
}


#' @rdname biofilter_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain]. The df may include a column indicating the EBCT or whether the water is ozonated.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "biofiltered".
#' @param pluck_cols Extract water slots modified by the function (doc, toc, bdoc) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   biofilter_toc_chain(input_water = "defined_water", ebct = c(10, 15), ozonated = FALSE)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   dplyr::mutate(
#'     BiofEBCT = c(10, 10, 10, 15, 15, 15, 20, 20, 20, 25, 25, 25),
#'     ozonated = c(rep(TRUE, 6), rep(FALSE, 6))
#'   ) %>%
#'   biofilter_toc_chain(input_water = "defined_water", ebct = BiofEBCT)
#'
#' @export
#'
#' @returns `biofilter_toc_chain` returns a data frame containing a water class column with updated DOC, TOC, and BDOC
#' concentrations. Optionally, it also adds columns for each of those slots individually.

biofilter_toc_chain <- function(df, input_water = "defined", output_water = "biofiltered",
                                ebct = "use_col", ozonated = "use_col") {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  ozonated <- tryCatch(ozonated, error = function(e) enquo(ozonated))

  arguments <- construct_helper(df, list("ebct" = ebct, "ozonated" = ozonated))

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(df, final_names, list(ozonated = TRUE))
  df <- defaults_added$data

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    chemdose_toc(
      water = df[[input_water]][[i]],
      ebct = df[[final_names$ebct]][i],
      ozonated = df[[final_names$ozonated]][i]
    )
  })

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
