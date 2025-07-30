# Bromate formation
# This function predicts bromate formation from ozonation

#' @title Calculate bromate formation
#'
#' @description Calculates bromate (BrO3-, ug/L) formation based on selected model. Required arguments include an object of class "water"
#' created by [define_water] ozone dose, reaction time, and desired model.
#' The function also requires additional water quality parameters defined in [define_water]
#' including bromide, DOC or UV254 (depending on the model), pH, alkalinity (depending on the model), and
#' optionally, ammonia (added when defining water using the `tot_nh3` argument.)
#' For a single water use `ozonate_bromate`; for a dataframe use `ozonate_bromate_df`.
#' Use `pluck_cols = TRUE` to get values from the output water as new dataframe columns.
#' For most arguments in the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @source Ozekin (1994), Sohn et al (2004), Song et al (1996), Galey et al (1997), Siddiqui et al (1994)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by [define_water]
#' @param dose Applied ozone dose (mg/L as O3). Results typically valid for 1-10 mg/L, but varies depending on model.
#' @param time Reaction time (minutes). Results typically valid for 1-120 minutes, but varies depending on model.
#' @param model Model to apply. One of c("Ozekin", "Sohn", "Song", "Galey", "Siddiqui")
#' @examples
#' example_dbp <- define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
#'   ozonate_bromate(dose = 1.5, time = 5, model = "Ozekin")
#' example_dbp <- define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
#'   ozonate_bromate(dose = 3, time = 15, model = "Sohn")
#'
#' @export
#' @returns `ozonate_bromate` returns a single water class object with calculated bromate (ug/L).
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

#' @rdname ozonate_bromate
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column named for the applied chlorine dose (cl2),
#' and a column for time in minutes.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "ozonated".
#' @param pluck_cols Extract water slots modified by the function (bro3) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#' @examples
#'
#' example_df <- water_df %>%
#'   dplyr::slice_head(n = 6) %>%
#'   dplyr::mutate(br = 50) %>%
#'   define_water_df() %>%
#'   dplyr::mutate(
#'     dose = c(seq(.5, 3, .5)),
#'     OzoneTime = 30
#'   ) %>%
#'   ozonate_bromate_df(time = OzoneTime, model = "Sohn", pluck_cols = TRUE)
#'
#' @export
#'
#' @returns `ozonate_bromate_df` returns a data frame containing a water class column with updated bro3
#' concentration. Optionally, it also adds columns for each of those slots individually.

ozonate_bromate_df <- function(df, input_water = "defined", output_water = "ozonated", pluck_cols = FALSE, water_prefix = TRUE,
                               dose = "use_col", time = "use_col", model = "use_col") {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  dose <- tryCatch(dose, error = function(e) enquo(dose))
  time <- tryCatch(time, error = function(e) enquo(time))
  model <- tryCatch(model, error = function(e) enquo(model))

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list("dose" = dose, "time" = time, "model" = model))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df, final_names,
    list(model = "Ozekin")
  )
  df <- defaults_added$data

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    ozonate_bromate(
      water = df[[input_water]][[i]],
      dose = df[[final_names$dose]][i],
      time = df[[final_names$time]][i],
      model = df[[final_names$model]][i]
    )
  })

  output <- df[, !names(df) %in% defaults_added$defaults_used]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("bro3"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  return(output)
}
