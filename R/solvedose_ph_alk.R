#' @title Calculate a desired chemical dose for a target pH
#'
#' @description Calculates the required amount of a chemical to dose based on a target pH and existing water quality.
#' The function takes an object of class "water", and user-specified chemical and target pH
#' and returns a numeric value for the required dose in mg/L.
#' For a single water, use `solvedose_ph`; to apply the model to a dataframe, use `solvedose_ph_df`.
#' For most arguments, the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details
#'
#' `solvedose_ph` uses [stats::uniroot()] on [chemdose_ph] to match the required dose for the requested pH target.
#'
#' @param water Source water of class "water" created by [define_water]
#' @param target_ph The final pH to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include:
#' acids: "hcl", "h2so4", "h3po4", "co2"; bases: "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
#'
#' @seealso [chemdose_ph], [solvedose_alk]
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 10)
#'
#' # Calculate required dose of lime to reach pH 8
#' solvedose_ph(water, target_ph = 8, chemical = "caoh2")
#'
#' @export
#' @returns  `solvedose_ph` returns a numeric value for the required chemical dose.
#'
solvedose_ph <- function(water, target_ph, chemical) {
  validate_water(water, c("ph", "alk"))
  if (missing(target_ph)) {
    stop("No target pH defined. Enter a target pH for the chemical dose.")
  }

  if ((target_ph > 14 | target_ph < 1) & !is.na(target_ph)) {
    stop("Target pH should be between 1-14.")
  }

  if (!(chemical %in% c(
    "hcl", "h2so4", "h3po4", "co2",
    "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
  ))) {
    stop("Selected chemical addition not supported.")
  }

  # This is the function to minimize
  match_ph <- function(root_dose, chemical, target_ph, water) {
    hcl <- ifelse(chemical == "hcl", root_dose, 0)
    h2so4 <- ifelse(chemical == "h2so4", root_dose, 0)
    h3po4 <- ifelse(chemical == "h3po4", root_dose, 0)

    naoh <- ifelse(chemical == "naoh", root_dose, 0)
    na2co3 <- ifelse(chemical == "na2co3", root_dose, 0)
    nahco3 <- ifelse(chemical == "nahco3", root_dose, 0)
    caoh2 <- ifelse(chemical == "caoh2", root_dose, 0)
    mgoh2 <- ifelse(chemical == "mgoh2", root_dose, 0)
    co2 <- ifelse(chemical == "co2", root_dose, 0)

    waterfin <- suppressWarnings(
      chemdose_ph(water,
        hcl = hcl, h2so4 = h2so4, h3po4 = h3po4,
        naoh = naoh, na2co3 = na2co3, nahco3 = nahco3,
        caoh2 = caoh2, mgoh2 = mgoh2, co2 = co2
      )
    )

    phfin <- waterfin@ph

    if (chemical %in% c("naoh", "na2co3", "nahco3", "caoh2", "mgoh2")) {
      (phfin - target_ph)
    } else {
      (target_ph - phfin)
    }
  }

  # Target pH can't be met
  if ((chemical %in% c("naoh", "na2co3", "nahco3", "caoh2", "mgoh2") &
    target_ph < water@ph) |
    (chemical == "co2" & (target_ph < 6.5)) |
    (chemical %in% c("hcl", "h2so4", "h3po4", "co2") &
      target_ph > water@ph) |
    is.na(target_ph)) {
    warning("Target pH cannot be reached with selected chemical. NA returned.")
    return(NA)
  } else {
    chemdose <- stats::uniroot(match_ph,
      interval = c(0, 200),
      chemical = chemical, target_ph = target_ph, water = water,
      extendInt = "up"
    )
    round(chemdose$root, 1)
  }
}


#' @title Calculate a desired chemical dose for a target alkalinity
#'
#' @description This function calculates the required amount of a chemical to dose based on a target alkalinity and existing water quality.
#' Returns numeric value for dose in mg/L. Uses uniroot on the chemdose_ph function.
#' For a single water, use `solvedose_alk`; to apply the model to a dataframe, use `solvedose_alk_df`.
#' For most arguments, the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details
#' `solvedose_alk` uses [stats::uniroot()] on [chemdose_ph] to match the required dose for the requested alkalinity target.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param target_alk The final alkalinity in mg/L as CaCO3 to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include:
#' acids: "hcl", "h2so4", "h3po4", "co2", bases: "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
#'
#' @seealso [solvedose_ph]
#'
#' @examples
#' dose_required <- define_water(ph = 7.9, temp = 22, alk = 100, 80, 50) %>%
#'   solvedose_alk(target_alk = 150, "naoh")
#' @export
#'
#' @returns `solvedose_alk` returns a numeric value for the required chemical dose.
#'
solvedose_alk <- function(water, target_alk, chemical) {
  validate_water(water, c("ph", "alk"))
  if (missing(target_alk)) {
    stop("No target alkalinity defined. Enter a target alkalinity (mg/L CaCO3) for the chemical dose.")
  }

  if ((chemical %in% c(
    "hcl", "h2so4", "h3po4", "co2",
    "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
  )) == FALSE) {
    stop("Selected chemical addition not supported.")
  }

  # This is the function to minimize
  match_alk <- function(root_dose, chemical, target_alk, water) {
    hcl <- ifelse(chemical == "hcl", root_dose, 0)
    h2so4 <- ifelse(chemical == "h2so4", root_dose, 0)
    h3po4 <- ifelse(chemical == "h3po4", root_dose, 0)

    naoh <- ifelse(chemical == "naoh", root_dose, 0)
    na2co3 <- ifelse(chemical == "na2co3", root_dose, 0)
    nahco3 <- ifelse(chemical == "nahco3", root_dose, 0)
    caoh2 <- ifelse(chemical == "caoh2", root_dose, 0)
    mgoh2 <- ifelse(chemical == "mgoh2", root_dose, 0)
    co2 <- ifelse(chemical == "co2", root_dose, 0)

    waterfin <- suppressWarnings(
      chemdose_ph(water,
        hcl = hcl, h2so4 = h2so4, h3po4 = h3po4,
        naoh = naoh, na2co3 = na2co3, nahco3 = nahco3,
        caoh2 = caoh2, mgoh2 = mgoh2, co2 = co2
      )
    )
    alkfin <- waterfin@alk

    (target_alk - alkfin)
  }

  # Target alkalinity can't be met
  if ((chemical %in% c("naoh", "na2co3", "nahco3", "caoh2", "mgoh2") &
    target_alk <= water@alk) |
    (chemical %in% c("hcl", "h2so4", "h3po4", "co2") &
      target_alk >= water@alk) |
    is.na(target_alk)) {
    warning("Target alkalinity cannot be reached with selected chemical. NA returned.")
    return(NA)
  } else {
    chemdose <- stats::uniroot(match_alk, interval = c(0, 1000), chemical = chemical, target_alk = target_alk, water = water)
    round(chemdose$root, 1)
  }
}

#' @rdname solvedose_ph
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input. Default is "defined".
#' @param output_column name of the output column storing doses in mg/L. Default is "dose".
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   solvedose_ph_df(input_water = "defined", target_ph = 8.8, chemical = "naoh")
#'
#' @export
#' @returns `solvedose_ph_df` returns a data frame containing the original data frame and columns for target pH, chemical dosed, and required chemical dose.

solvedose_ph_df <- function(df, input_water = "defined", output_column = "dose", target_ph = "use_col", chemical = "use_col") {
  validate_water_helpers(df, input_water)

  # This allows for the function to process unquoted column names without erroring
  target_ph <- tryCatch(target_ph, error = function(e) enquo(target_ph))
  chemical <- tryCatch(chemical, error = function(e) enquo(chemical))

  arguments <- construct_helper(df, list("target_ph" = target_ph, "chemical" = chemical))
  final_names <- arguments$final_names
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }

  df[[output_column]] <- sapply(seq_len(nrow(df)), function(i) {
    solvedose_ph(
      water = df[[input_water]][[i]],
      chemical = df[[final_names$chemical]][i],
      target_ph = df[[final_names$target_ph]][i]
    )
  })

  return(df)
}

#' @rdname solvedose_alk
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_column name of the output column storing doses in mg/L. Default is "dose_required".
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(finAlk = seq(100, 210, 10)) %>%
#'   solvedose_alk_df(chemical = "na2co3", target_alk = finAlk)
#'
#' @export
#'
#' @returns `solvedose_alk_df` returns a data frame containing the original data frame and columns for target alkalinity, chemical dosed, and required chemical dose.

solvedose_alk_df <- function(df, input_water = "defined", output_column = "dose", target_alk = "use_col", chemical = "use_col") {
  validate_water_helpers(df, input_water)

  # This allows for the function to process unquoted column names without erroring
  target_alk <- tryCatch(target_alk, error = function(e) enquo(target_alk))
  chemical <- tryCatch(chemical, error = function(e) enquo(chemical))

  arguments <- construct_helper(df, list("target_alk" = target_alk, "chemical" = chemical))
  final_names <- arguments$final_names
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }

  df[[output_column]] <- sapply(seq_len(nrow(df)), function(i) {
    solvedose_alk(
      water = df[[input_water]][[i]],
      chemical = df[[final_names$chemical]][i],
      target_alk = df[[final_names$target_alk]][i]
    )
  })

  return(df)
}
