# Bromate formation
# This function predicts bromate formation from ozonation

#' @title Calculate bromate formation
#'
#' @description Calculates bromate (BrO3-, ug/L) formation based on selected model. Required arguments include an object of class "water"
#' created by \code{\link{define_water}} ozone dose, reaction time, and desired model.
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including bromide, DOC or UV254 (depending on the model), pH, alkalinity (depending on the model), and
#' optionally, ammonia (added when defining water using the `tot_nh3` argument.)
#'
#'
#' @source Ozekin (1994), Sohn et al (2004), Song et al (1996), Galey et al (1997), Siddiqui et al (1994)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param dose Applied ozone dose (mg/L as O3). Results typically valid for 1-10 mg/L, but varies depending on model.
#' @param time Reaction time (minutes). Results typically valid for 1-120 minutes, but varies depending on model.
#' @param model Model to apply. One of c("Ozekin", "Sohn", "Song", "Galey", "Siddiqui")
#' @examples
#' example_dbp <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   ozonate_bromate(dose = 1.5, time = 5, model = "Ozekin")
#' example_dbp <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   ozonate_bromate(dose = 3, time = 15, model = "Sohn")
#'
#' @export
#' @returns A water class object with calculated bromate (ug/L).
#'
ozonate_bromate <- function(water, dose, time, model = "Ozekin") {
  ammonia <- NULL # Quiet RCMD check global variable note
  validate_water(water, c("br", "ph"))
  if (missing(dose)) {
    stop("Ozone dose must be specified in mg/L.")
  }
  if (missing(time) & model != "Siddiqui") {
    stop("Reaction time in minutes required for all models except 'Siddiqui'")
  }
  if (!model %in% c("Ozekin", "Sohn", "Song", "Galey", "Siddiqui")) {
    stop("model must be one of 'Ozekin', 'Sohn', 'Song', 'Galey', 'Siddiqui'.")
  }

  # Other parameters depend on model
  if (is.na(water@alk) & model %in% c("Sohn", "Song")) {
    stop("Alkalinity required for selected model. Use one of 'Ozekin', 'Galey', 'Siddiqui' instead or add alkalinity when defining water.")
  }
  if (is.na(water@doc) & model != "Sohn") {
    stop("DOC required for selected model. Use 'Sohn' to use UV254 instead or add DOC when defining water.")
  }
  if (is.na(water@uv254) & model == "Sohn") {
    stop("UV254 required for Sohn model. Use a different model or add UV254 when defining water.")
  }

  # Bromide should be in ug/L for these models
  br <- convert_units(water@br, "br", "M", "ug/L")

  doc <- ifelse(is.na(water@doc), 0, water@doc)
  uv254 <- ifelse(is.na(water@uv254), 0, water@uv254)
  ph <- water@ph
  alk <- ifelse(is.na(water@alk), 0, water@alk)
  nh4 <- ifelse(is.na(water@nh4), 0, convert_units(water@nh4, "nh4", "M", "mg/L N"))
  temp <- water@temp
  # TODO add warnings for parameters outside model ranges *****************************************************************
  mod <- model
  # All models must match this form.
  solve_bro3 <- subset(tidywater::bromatecoeffs, model == mod & ammonia == ifelse(nh4 == 0, F, T))

  if (nrow(solve_bro3) == 0 & nh4 == 0) {
    stop("Selected model not applicable to waters with no ammonia. Select one of 'Ozekin', 'Sohn', 'Galey', 'Siddiqui',
         specify nh4 in define_water, or dose it with chemdose_ph.")
  } else if (nrow(solve_bro3) == 0 & nh4 > 0) {
    stop("Selected model not applicable to water with ammonia. Select one of 'Ozekin', 'Sohn', 'Song' or change nh4 to 0.")
  }

  # bro3 = A * br^a * doc^b * uv254^c * ph^d * alk^e * dose^f * time^g * nh4^h * temp^i * I^(temp - 20)
  water@bro3 <- solve_bro3$A * br^solve_bro3$a * doc^solve_bro3$b * uv254^solve_bro3$c * ph^solve_bro3$d *
    alk^solve_bro3$e * dose^solve_bro3$f * time^solve_bro3$g * nh4^solve_bro3$h * temp^solve_bro3$i * solve_bro3$I^(temp - 20)

  return(water)
}


#' Apply `ozonate_bromate`function within a data frame and output a data frame
#'
#' This function allows \code{\link{ozonate_bromate}} to be added to a piped data frame.
#' Its output is a data frame containing a bro3 column.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}}.
#'
#' If the input data frame has a dose column (dose) or time column (time), the function will use those columns. Note:
#' The function can only take dose and time inputs as EITHER a column or from the function arguments, not both.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#' for the option to use parallel processing and speed things up. To initialize parallel processing, use
#' `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#' `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#' shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_once}}. The df may include a column named for the applied chlorine dose (cl2),
#' and a column for time.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param dose Applied ozone dose (mg/L as O3). Results typically valid for 1-10 mg/L, but varies depending on model.
#' @param time Reaction time (minutes). Results typically valid for 1-120 minutes, but varies depending on model.
#' @param model Model to apply. One of c("Ozekin", "Sohn", "Song", "Galey", "Siddiqui")
#'
#' @seealso \code{\link{ozonate_bromate}}
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain("raw") %>%
#'   ozonate_bromate_once(input_water = "raw", dose = 3, time = 8)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain("raw") %>%
#'   mutate(
#'     dose = seq(1, 5, .5),
#'     time = 10
#'   ) %>%
#'   ozonate_bromate_once(input_water = "raw")
#'
#' example_df <- water_df %>%
#'   mutate(br = 80) %>%
#'   define_water_chain("raw") %>%
#'   mutate(time = 8) %>%
#'   ozonate_bromate_once(
#'     input_water = "raw", dose = 6, model = "Sohn"
#'   )
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   ozonate_bromate_once(input_water = "defined_water", dose = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns A data frame with updated bromate.

ozonate_bromate_once <- function(df, input_water = "defined_water", dose = 0, time = 0,
                                 model = "Ozekin") {
  temp_o3 <- ozone <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    ozonate_bromate_chain(
      input_water = input_water, output_water = "temp_o3",
      dose, time, model
    ) %>%
    mutate(ozone = furrr::future_map(temp_o3, convert_water)) %>%
    unnest(ozone) %>%
    select(-temp_o3)
}

#' Apply `ozonate_bromate` within a data frame and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{ozonate_bromate}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' bro3 slot will be updated.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}}.
#'
#' If the input data frame has a dose column (dose) or time column (time), the function will use those columns. Note:
#' The function can only take dose and time inputs as EITHER a column or from the function arguments, not both.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column named for the applied ozone dose (dose),
#' and a column for time.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "ozonated_water".
#' @param dose Applied ozone dose (mg/L as O3). Results typically valid for 1-10 mg/L, but varies depending on model.
#' @param time Reaction time (minutes). Results typically valid for 1-120 minutes, but varies depending on model.
#' @param model Model to apply. One of c("Ozekin", "Sohn", "Song", "Galey", "Siddiqui")
#'
#' @seealso \code{\link{ozonate_bromate}}
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   ozonate_bromate_chain(dose = 4, time = 8)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   mutate(
#'     dose = seq(1, 3, .5),
#'     time = 30
#'   ) %>%
#'   ozonate_bromate_chain()
#'
#' example_df <- water_df %>%
#'   mutate(br = 80) %>%
#'   define_water_chain() %>%
#'   mutate(time = 8) %>%
#'   ozonate_bromate_chain(
#'     dose = 6, model = "Sohn"
#'   )
#'
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   ozonate_bromate_chain(dose = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns A data frame containing a water class column with updated chlorine residuals.

ozonate_bromate_chain <- function(df, input_water = "defined_water", output_water = "ozonated_water",
                                  dose = 0, time = 0, model = "Ozekin") {
  ID <- NULL # Quiet RCMD check global variable note

  inputs_arg <- cross_join(tibble(dose), tibble(time)) %>%
    select_if(~ any(. > 0))

  if(length(model) > 1) {
    inputs_arg <- inputs_arg %>%
      cross_join(tibble(model))
  }

  inputs_col <- df %>%
    subset(select = names(df) %in% c("dose", "time", "model")) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(inputs_col) < 3 & length(inputs_arg) == 0) {
    warning("Arguments missing. Add them as a column or function argument.")
  }

  if (("dose" %in% colnames(inputs_arg) & "dose" %in% colnames(inputs_col)) |
      ("time" %in% colnames(inputs_arg) & "time" %in% colnames(inputs_col)) |
      ("model" %in% colnames(inputs_arg) & "model" %in% colnames(inputs_col))) {
    stop("Argument was applied as both a function argument and a data frame column. Choose one input method.")
  }

  arguments <- inputs_col %>%
    cross_join(inputs_arg)

  output <- df %>%
    subset(select = !names(df) %in% c("dose", "time", "model")) %>%
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
        model = model
      ),
      ozonate_bromate
    ))
}
