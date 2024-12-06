#' @title Determine TOC removal from biofiltration using Terry & Summers BDOC model
#'
#' @description This function applies the Terry model to a water created by \code{\link{define_water}} to determine biofiltered
#' DOC (mg/L).
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}.
#' @param ebct The empty bed contact time (min) used for the biofilter
#' @param ozonated Logical; TRUE if the water is ozonated (default), FALSE otherwise
#'
#' @source Terry and Summers 2018
#' @examples
#' library(tidywater)
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1) %>%
#'   biofilter_toc(ebct = 10, ozonated = FALSE)
#'
#' @returns  A water class object with modeled DOC removal from biofiltration.
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

#' Apply `biofilter_toc` function and output a data frame
#'
#' This function allows \code{\link{biofilter_toc}} to be added to a piped data frame.
#' Its output is a data frame with updated TOC, DOC, and BDOC
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}}.
#'
#' If the input data frame has column(s) named "ebct" or "ozonated", the function use those as arguments. Note:
#' The function can use either a column or the direct function arguments, not both.
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
#' @param ebct The empty bed contact time (min) used for the biofilter
#' @param ozonated Logical; TRUE if the water is ozonated (default), FALSE otherwise
#'
#' @seealso \code{\link{biofilter_toc}}
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
#'     ebct = c(10,15,20),
#'     ozonated = c(rep(TRUE, 3), rep(FALSE, 3))
#'   ) %>%
#'   biofilter_toc_once(input_water = "defined_water")
#'
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   biofilter_toc_once(input_water = "defined_water", ebct = c(10,20))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns A data frame with an updated DOC, TOC, and BDOC concentration.

biofilter_toc_once <- function(df, input_water = "defined_water", ebct = 0, ozonated = TRUE) {
  biofiltered_water <- biofilter <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    biofilter_toc_chain(
      input_water = input_water, output_water = "biofiltered_water",
      ebct, ozonated
    ) %>%
    mutate(biofilter = furrr::future_map(biofiltered_water, convert_water)) %>%
    unnest(biofilter) %>%
    select(-biofiltered_water)
}

#' Apply `biofilter_toc` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{biofilter_toc}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' TOC, DOC, and UV254 will be updated based on input chemical doses.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has column(s) named "ebct" or "ozonated", the function use those as arguments. Note:
#' The function can use either a column or the direct function arguments, not both.
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
#' @param ebct The empty bed contact time (min) used for the biofilter
#' @param ozonated Logical; TRUE if the water is ozonated (default), FALSE otherwise
#'
#' @seealso \code{\link{biofilter_toc}}
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
#'     ebct = c(10,15,20)
#'     ozonated = c(rep(TRUE, 3), rep(FALSE, 3))
#'   ) %>%
#'   biofilter_toc_once(input_water = "defined_water", output_water = "biof")
#'
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   biofilter_toc_once(input_water = "defined_water", ebct = c(10,20))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export
#'
#' @returns A data frame containing a water class column with updated DOC, TOC, and UV254 concentrations.

biofilter_toc_chain <- function(df, input_water = "defined_water", output_water = "biofiltered_water",
                                ebct = 0, ozonated = TRUE) {
  ID <- NULL # Quiet RCMD check global variable note
  inputs_arg <- tibble(ebct) %>%
    select_if(~ any(. > 0))

  inputs_col <- df %>%
    subset(select = names(df) %in% c("ebct")) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(inputs_col) < 1 & length(inputs_arg) == 0) {
    warning("EBCT argument missing. Add as a column or function argument.")
  }

  if (("ebct" %in% colnames(inputs_arg) & "ebct" %in% colnames(inputs_col))) {
    stop("EBCT was added as both a function argument and a data frame column. Choose one input method.")
  }

  arguments <- inputs_col %>%
    cross_join(inputs_arg)

  output <- df %>%
    subset(select = !names(df) %in% c("ebct")) %>%
    mutate(
      ID = row_number(),
      ozonated = ozonated
    ) %>%
    left_join(arguments, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        ebct = ebct,
        ozonated = ozonated
      ),
      biofilter_toc
    ))
}



