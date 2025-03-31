#' @title Determine TOC removal from biofiltration using Terry & Summers BDOC model
#'
#' @description This function applies the Terry model to a water created by [define_water] to determine biofiltered
#' DOC (mg/L).
#' For a single water use `biofilter_toc`; for a dataframe where you want to output a water for continued modeling use
#' `biofilter_toc_chain`; for a dataframe where you want to output water parameters as columns use `biofilter_toc_once`
#' (note subsequent tidywater modeling functions will only work if `_chain` is used because a `water` is required).
#' For most arguments, the `_chain` and `_once` helpers
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
    if (temp <= 10) {
      k <- 0.03
      BDOC_fraction <- BDOC_fraction_ozonated
    } else if (temp <= 20) {
      k <- 0.06
      BDOC_fraction <- BDOC_fraction_ozonated
    } else {
      k <- 0.15
      BDOC_fraction <- BDOC_fraction_ozonated
    }
  } else {
    if (temp <= 10) {
      k <- 0.03
      BDOC_fraction <- BDOC_fraction_nonozonated
    } else if (temp <= 20) {
      k <- 0.09
      BDOC_fraction <- BDOC_fraction_nonozonated
    } else {
      k <- 0.11
      BDOC_fraction <- BDOC_fraction_nonozonated
    }
  }

  # Calculate BDOC influent concentration
  BDOC_inf <- BDOC_fraction * water@doc

  # Calculate BDOC effluent concentration using the exponential decay model
  BDOC_eff <- BDOC_inf * exp(-k * ebct)

  # Calculate TOC removal percentage
  BDOC_removed <- (BDOC_inf - BDOC_eff)

  # Update water object with new TOC and DOC values
  water@toc <- water@toc - BDOC_removed
  water@doc <- water@toc - BDOC_removed
  water@bdoc <- BDOC_eff
  water@applied_treatment <- paste(water@applied_treatment, "_biofilter", sep = "")

  return(water)
}


#' @rdname biofilter_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain]. The df may include a column indicating the EBCT or whether the water is ozonated.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
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
#'   biofilter_toc_once(input_water = "defined_water", ebct = 10, ozonated = FALSE)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(
#'     BiofEBCT = rep(c(10, 15, 20), 4),
#'     ozonated = c(rep(TRUE, 6), rep(FALSE, 6))
#'   ) %>%
#'   biofilter_toc_once(input_water = "defined_water", ebct = BiofEBCT)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns `biofilter_toc_once` returns a data frame with updated DOC, TOC, and BDOC concentrations as columns.

biofilter_toc_once <- function(df, input_water = "defined_water", ebct = "use_col", ozonated = "use_col") {
  biofiltered_water <- biofilter <- NULL # Quiet RCMD check global variable note

  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  ozonated <- tryCatch(ozonated, error = function(e) enquo(ozonated))

  output <- df %>%
    biofilter_toc_chain(
      input_water = input_water, output_water = "biofiltered_water",
      ebct, ozonated
    ) %>%
    mutate(biofilter = furrr::future_map(biofiltered_water, convert_water)) %>%
    unnest(biofilter) %>%
    select(-biofiltered_water)
}

#' @rdname biofilter_toc
#' @param output_water name of the output column storing updated parameters with the class, Water. Default is "biofiltered_water".
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
#'   biofilter_toc_chain(input_water = "defined_water", ebct = 10, ozonated = FALSE)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(
#'     BiofEBCT = c(10, 10, 10, 15, 15, 15, 20, 20, 20, 25, 25, 25),
#'     ozonated = c(rep(TRUE, 6), rep(FALSE, 6))
#'   ) %>%
#'   biofilter_toc_chain(input_water = "defined_water", ebct = BiofEBCT)
#'
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   biofilter_toc_chain(input_water = "defined_water", ebct = c(10, 20))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export
#'
#' @returns `biofilter_toc_chain` returns a data frame containing a water class column with updated DOC, TOC, and UV254 water slots.

biofilter_toc_chain <- function(df, input_water = "defined_water", output_water = "biofiltered_water",
                                ebct = "use_col", ozonated = "use_col") {
  # This allows for the function to process unquoted column names without erroring
  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  ozonated <- tryCatch(ozonated, error = function(e) enquo(ozonated))

  arguments <- construct_helper(df, list("ebct" = ebct, "ozonated" = ozonated))

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        ebct = !!as.name(arguments$final_names$ebct),
        # This logic needed for any argument that has a default
        ozonated = ifelse(exists(as.name(arguments$final_names$ozonated), where = .),
          !!as.name(arguments$final_names$ozonated), TRUE
        )
      ),
      biofilter_toc
    ))
}
