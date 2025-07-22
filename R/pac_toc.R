# PAC modeling
# Used for predicting DOC concentration

#' @title Calculate DOC Concentration in PAC system
#'
#' @description Calculates DOC concentration multiple linear regression model found in 2-METHYLISOBORNEOL AND NATURAL ORGANIC MATTER
#' ADSORPTION BY POWDERED ACTIVATED CARBON by HYUKJIN CHO (2007).
#' Assumes all particulate TOC is removed when PAC is removed; therefore TOC = DOC in output.
#' For a single water use `pac_toc`; for a dataframe use `pac_toc_df`.
#' Use `pluck_cols = TRUE` to get values from the output water as new dataframe columns.
#' For most arguments in the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' water must contain DOC or TOC value.
#'
#' @details The function will calculate DOC concentration by PAC adsorption in drinking water treatment.
#' UV254 concentrations are predicted based on a linear relationship with DOC.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source CHO(2007)
#' @param water Source water object of class "water" created by [define_water]
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
#' @returns `pac_toc` returns a water class object with updated DOC, TOC, and UV254 slots.
#'
pac_toc <- function(water, dose, time, type = "bituminous") {
  pactype <- NULL # Quiet RCMD check global variable note
  validate_water(water, c("doc"))
  if (missing(dose) | !is.numeric(dose) | dose < 0) {
    stop("PAC dose must be specified as a non-negative number.")
  }
  if (missing(time) | !is.numeric(time) | time < 0) {
    stop("Reaction time must be specified as a non-negative number.")
  }

  doc <- water@doc
  uv254 <- water@uv254
  toc <- water@toc

  # warnings and errors for bounds of PAC dose, time.
  # High dose/time not allowed because model form results in negative DOC.
  if (dose < 5) warning("PAC dose is less than model bound of 5 mg/L")
  if (dose > 30) stop("PAC model does not work for PAC dose >30. Adjust dose argument.")

  if (time < 10) warning("Time is less than model bounds of 10 min")
  if (time > 60) stop("PAC model does not work for time > 60 mins. Adjust time argument.")

  # water warnings
  if (!is.na(water@toc) & water@toc < water@doc) {
    warning("TOC of input water less than DOC. TOC will be set equal to DOC.")
  }
  if (is.na(water@toc)) {
    warning("Input water TOC not specified. Output water TOC will be NA.")
  }

  if (doc < 1.3 || doc > 5.4) {
    warning("DOC concentration is outside the model bounds of 1.3 to 5.4 mg/L")
  }

  # make case insensitive
  type <- tolower(type)
  if (!type %in% c("bituminous", "wood", "lignite")) {
    stop("Invalid PAC type. Choose either 'bituminous', 'wood' or 'lignite'.")
  }

  coeffs <- subset(tidywater::pactoccoeffs, pactype == type)

  if (dose == 0 | time == 0) {
    warning("No PAC added or reaction time is zero. Final water will equal input water.")
    remaining <- 1
  } else if (doc < 1.3) {
    # Because of the form of the equation, DOC<1.3 results in negative DOC. Assume same % removal as DOC0=1.3
    remaining <- (coeffs$A + coeffs$a * 1.3 - coeffs$b * dose - coeffs$c * time) / doc
  } else {
    remaining <- (coeffs$A + coeffs$a * doc - coeffs$b * dose - coeffs$c * time) / doc
  }
  result <- remaining * doc

  # Predict DOC concentration via UV absorbance

  # UVA can be a good indicator to predict DOC concentration by PAC adsorption
  # can be predicted through relationship of DOC and UVA removal --> dimensionless unit (C/C0)

  UVA <- .0376 * result - .041

  water@doc <- result
  water@toc <- result
  water@uv254 <- UVA

  return(water)
}

#' @rdname pac_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include columns named for the dose, time, and type
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "paced". Pronouced P.A.ceed (not ideal we know).
#' @param pluck_cols Extract water slots modified by the function (doc, toc, uv254) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df("raw") %>%
#'   dplyr::mutate(dose = seq(11, 22, 1), PACTime = 30) %>%
#'   pac_toc_df(input_water = "raw", time = PACTime, type = "wood", pluck_cols = TRUE)
#'
#' @export
#'
#' @returns `pac_toc_df` returns a data frame containing a water class column with updated DOC, TOC, and UV254
#' concentrations. Optionally, it also adds columns for each of those slots individually.

pac_toc_df <- function(df, input_water = "defined", output_water = "paced", pluck_cols = FALSE, water_prefix = TRUE,
                          dose = "use_col", time = "use_col", type = "use_col") {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  dose <- tryCatch(dose, error = function(e) enquo(dose))
  time <- tryCatch(time, error = function(e) enquo(time))
  type <- tryCatch(type, error = function(e) enquo(type))

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list("dose" = dose, "time" = time, "type" = type))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df, final_names,
    list(type = "bituminous")
  )
  df <- defaults_added$data

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    pac_toc(
      water = df[[input_water]][[i]],
      dose = df[[final_names$dose]][i],
      time = df[[final_names$time]][i],
      type = df[[final_names$type]][i]
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
