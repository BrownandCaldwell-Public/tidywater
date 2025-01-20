# PAC modeling
# Used for predicting DOC concentration

#' @title Calculate DOC Concentration in PAC system
#'
#' @description Calculates DOC concentration multiple linear regression model found in 2-METHYLISOBORNEOL AND NATURAL ORGANIC MATTER
#' ADSORPTION BY POWDERED ACTIVATED CARBON by HYUKJIN CHO (2007)
#' Required arguments include an object of class "water"
#' created by \code{\link{define_water}} initial DOC concentration, amount of PAC added to system, contact time with PAC, type of PAC
#'
#' water must contain DOC or TOC value.
#'
#' @details The function will calculate DOC concentration by PAC adsorption in drinking water treatment.
#' UV254 concentrations are predicted based on a linear relationship with DOC.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source CHO(2007)
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param dose Applied PAC dose (mg/L). Model results are valid for doses concentrations between 5 and 30 mg/L.
#' @param time Contact time (minutes). Model results are valid for reaction times between 10 and 1440 minutes
#' @param type Type of PAC applied, either "bituminous", "lignite", "wood".
#'
#' @examples
#' water <- define_water(toc = 2.5, uv254 = .05, doc = 1.5) %>%
#'   pac_toc(dose = 15, time = 50, type = "wood")
#'
#' @export
#'
#' @returns A water class object with post-PAC predicted DOC and UV254.
pac_toc <- function(water, dose, time, type = "bituminous") {
  validate_water(water, c("doc"))
  if (missing(dose) | !is.numeric(dose)) {
    stop("PAC dose must be specified as a number.")
  }
  if (missing(time) | !is.numeric(time)) {
    stop("Reaction time must be specified as a number.")
  }

  doc <- water@doc
  uv254 <- water@uv254
  toc <- water@toc

  # warnings for bounds of PAC dose, time, defined doc in tidywater etc.
  if (dose < 5 | dose > 30) {
    warning("PAC Dose is outside the model bounds of 5 to 30 mg/L")
  }
  if (time < 10 | time > 1440) {
    warning("Duration is outside the model bounds of 10 to 1440 min")
  }
  if (dose <= 0) {
    warning("No PAC added. Final water will equal input water.")
  }


  # more warnings
  if (!is.na(water@toc) & water@toc < water@doc) {
    warning("TOC of input water less than DOC. TOC will be set equal to DOC.")
  }
  if (is.na(water@toc)) {
    warning("Input water TOC not specified. Output water TOC will be NA.")
  }

  if (doc < 1 || doc > 5) {
    warning("DOC concentration is outside the model bounds of 1 to 5 mg/L")
  }


  # Calculate toc
  org_carbon_undissolved <- toc - doc
  # make case insensitive
  type <- tolower(type)
  if (dose == 0 | time == 0) {
    warning("No PAC added. Final water will equal input water.")
    result <- doc
  } else if (type == "bituminous") {
    result <- .1561 + .9114 * doc - .0263 * dose - .002 * time
  } else if (type == "lignite") {
    result <- .4078 + .8516 * doc - .0225 * dose - .002 * time
  } else if (type == "wood") {
    result <- .3653 + .8692 * doc - .0151 * dose - .0025 * time
  } else {
    stop("Invalid PAC type. Choose either 'Bituminous', 'Wood' or 'Lignite' ")
  }


  # Predict DOC concentration via UV absorbance

  # UVA can be a good indicator to predict DOC concentration by PAC adsorption
  # can be predicted through relationship of DOC and UVA removal --> dimensionless unit (C/C0)

  UVA <- .0376 * result - .041

  toc_new <- result + org_carbon_undissolved

  water@doc <- result
  water@uv254 <- UVA
  water@toc <- toc_new

  return(water)
}

#' Apply `pac_toc`function within a data frame and output a data frame
#'
#' PAC = powdered activated carbon
#'
#' This function allows \code{\link{pac_toc}} to be added to a piped data frame.
#' Its output is a data frame containing a water with updated TOC, DOC, and UV254.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}}.
#'
#' If the input data frame has a dose, time or type column, the function will use those columns. Note:
#' The function can only take dose, time, and type inputs as EITHER a column or from the function arguments, not both.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#' for the option to use parallel processing and speed things up. To initialize parallel processing, use
#' `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#' `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#' shorter run times will not benefit from parallel processing.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source CHO(2007)
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include columns named for the dose, time, and type
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param dose Applied PAC dose (mg/L). Model results are valid for doses concentrations between 5 and 30 mg/L.
#' @param time Contact time (minutes). Model results are valid for reaction times between 10 and 1440 minutes
#' @param type Type of PAC applied, either "bituminous", "lignite", "wood".
#'
#' @seealso \code{\link{pac_toc}}
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   pac_toc_once(input_water = "raw", dose = 10, time = 20)
#'
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   mutate(dose = seq(5, 60, 5), time = 30) %>%
#'   pac_toc_once(input_water = "raw")
#'
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   mutate(time = 8) %>%
#'   pac_toc_once(
#'     input_water = "raw", dose = 6, type = "wood"
#'   )
#'
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   pac_toc_once(input_water = "raw", dose = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns A data frame with an updated DOC, TOC, and UV254 concentration.

pac_toc_once <- function(df, input_water = "defined_water",
                         dose = 0, time = 0, type = "bituminous") {
  temp_pac <- temp_df <- toc <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    pac_toc_chain(
      input_water = input_water, output_water = "temp_pac",
      dose, time, type
    ) %>%
    mutate(toc = furrr::future_map(temp_pac, convert_water)) %>%
    unnest(toc) %>%
    select(-temp_pac)
}

#' Apply `pac_toc` within a data frame and output a column of `water` class to be chained to other tidywater functions
#' PAC = powdered activated carbon
#'
#' This function allows \code{\link{pac_toc}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}}.
#'
#' If the input data frame has a dose, time or type column, the function will use those columns. Note:
#' The function can only take dose, time, and type inputs as EITHER a column or from the function arguments, not both.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#' for the option to use parallel processing and speed things up. To initialize parallel processing, use
#' `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#' `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#' shorter run times will not benefit from parallel processing.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source CHO(2007)
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include columns named for the dose, time, and type
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "disinfected_water".
#' @param dose Applied PAC dose (mg/L). Model results are valid for doses concentrations between 5 and 30 mg/L.
#' @param time Contact time (minutes). Model results are valid for reaction times between 10 and 1440 minutes
#' @param type Type of PAC applied, either "bituminous", "lignite", "wood".
#'
#' @seealso \code{\link{pac_toc}}
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   pac_toc_chain(input_water = "raw", dose = 10, time = 20)
#'
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   mutate(dose = seq(11, 22, 1), time = 30) %>%
#'   pac_toc_chain(input_water = "raw")
#'
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   mutate(time = 8) %>%
#'   pac_toc_chain(
#'     input_water = "raw", dose = 6, type = "wood"
#'   )
#'
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   pac_toc_chain(input_water = "raw", dose = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' @import dplyr
#'
#' @export
#'
#' @returns A data frame containing a water class column with updated DOC, TOC, and UV254 concentrations.

pac_toc_chain <- function(df, input_water = "defined_water", output_water = "pac_water",
                          dose = 0, time = 0, type = "bituminous") {
  ID <- NULL # Quiet RCMD check global variable note

  arguments <- construct_helper(df, list("dose" = dose, "time" = time), list("type" = type))

  output <- df %>%
    subset(select = !names(df) %in% c("dose", "time", "type")) %>%
    mutate(
      ID = row_number()
    ) %>%
    left_join(arguments, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        dose = dose,
        time = time,
        type = type
      ),
      pac_toc
    ))
}
