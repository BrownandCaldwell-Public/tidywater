#' @title Calculate a desired chemical dose for a target pH
#'
#' @description \code{solvedose_ph} calculates the required amount of a chemical to dose based on a target pH and existing water quality.
#' The function takes an object of class "water" created by \code{\link{define_water}}, and user-specified chemical and target pH
#' and returns a numeric value for the required dose in mg/L.
#'
#' \code{solvedose_ph} uses \code{uniroot} on \code{\link{chemdose_ph}} to match the required dose for the requested pH target.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param target_ph The final pH to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include:
#' acids: "hcl", "h2so4", "h3po4", "co2"; bases: "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
#'
#' @seealso \code{\link{define_water}}, \code{\link{chemdose_ph}}
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 10)
#'
#' # Calculate required dose of lime to reach pH 8
#' solvedose_ph(water, target_ph = 8, chemical = "caoh2")
#'
#' @export
#' @returns  A numeric value for the required chemical dose.
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

    waterfin <- chemdose_ph(water,
      hcl = hcl, h2so4 = h2so4, h3po4 = h3po4,
      naoh = naoh, na2co3 = na2co3, nahco3 = nahco3,
      caoh2 = caoh2, mgoh2 = mgoh2, co2 = co2
    )

    phfin <- waterfin@ph

    (target_ph - phfin)
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
    chemdose <- stats::uniroot(match_ph, interval = c(0, 1000), chemical = chemical, target_ph = target_ph, water = water)
    round(chemdose$root, 1)
  }
}


#' @title Calculate a desired chemical dose for a target alkalinity
#'
#' @description This function calculates the required amount of a chemical to dose based on a target alkalinity and existing water quality.
#' Returns numeric value for dose in mg/L. Uses uniroot on the chemdose_ph function.
#'
#' @param water Source water of class "water" created by \code{\link{define_water}}
#' @param target_alk The final alkalinity in mg/L as CaCO3 to be achieved after the specified chemical is added.
#' @param chemical The chemical to be added. Current supported chemicals include:
#' acids: "hcl", "h2so4", "h3po4", "co2", bases: "naoh", "na2co3", "nahco3", "caoh2", "mgoh2"
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' dose_required <- define_water(ph = 7.9, temp = 22, alk = 100, 80, 50) %>%
#'   solvedose_alk(target_alk = 150, "naoh")
#' @export
#'
#' @returns  A numeric value for the required chemical dose.
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

    waterfin <- chemdose_ph(water,
      hcl = hcl, h2so4 = h2so4, h3po4 = h3po4,
      naoh = naoh, na2co3 = na2co3, nahco3 = nahco3,
      caoh2 = caoh2, mgoh2 = mgoh2, co2 = co2
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

#' Apply `solvedose_ph` to a dataframe and create a new column with numeric dose
#'
#' This function allows \code{\link{solvedose_ph}} to be added to a piped data frame.
#' Its output is a chemical dose in mg/L.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has column(s) named "target_ph" or "chemical", the function will use the column(s)
#' as function argument(s). If these columns aren't present, specify "target_ph" or "chemical" as function arguments.
#' The chemical names must match the chemical names as displayed in \code{\link{solvedose_ph}}.
#' To see which chemicals can be dosed, see \code{\link{solvedose_ph}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_column name of the output column storing doses in mg/L. Default is "dose_required".
#' @param target_ph set a goal for pH using the function argument or a data frame column
#' @param chemical select the chemical to be used to reach the desired pH using function argument or data frame column
#' @seealso \code{\link{solvedose_ph}}
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
#'   balance_ions_chain() %>%
#'   mutate(
#'     target_ph = 10,
#'     chemical = rep(c("naoh", "mgoh2"), 6)
#'   ) %>%
#'   solvedose_ph_once()
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   solvedose_ph_once(input_water = "balanced_water", target_ph = 8.8, chemical = "naoh")
#'
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_ph = seq(9, 10.1, .1)) %>%
#'   solvedose_ph_once(chemical = "naoh")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_ph = seq(9, 10.1, .1)) %>%
#'   solvedose_ph_once(chemical = "naoh")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export
#' @returns A data frame containing the original data frame and columns for target pH, chemical dosed, and required chemical dose.

solvedose_ph_once <- function(df, input_water = "defined_water", output_column = "dose_required", target_ph = NULL, chemical = NULL) {
  dose <- NULL # Quiet RCMD check global variable note
  dosable_chems <- tibble(
    hcl = 0, h2so4 = 0, h3po4 = 0,
    co2 = 0,
    naoh = 0, caoh2 = 0, mgoh2 = 0,
    na2co3 = 0, nahco3 = 0,
    cl2 = 0, naocl = 0, caocl2 = 0,
    alum = 0, ferricchloride = 0, ferricsulfate = 0
  )

  chem <- df %>%
    filter(chemical %in% names(dosable_chems))

  if (length(chemical) > 0) {
    if (!chemical %in% names(dosable_chems)) {
      stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_ph.")
    }
  }

  if (length(chem$chemical) > 0 & !all(unique(df$chemical) %in% names(dosable_chems))) {
    stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_ph.")
  }

  if ("target_ph" %in% names(df) & length(target_ph) > 0) {
    stop("Target pH was set as both a function argument and a data frame column. Remove your target pH from one of these inputs.")
  }

  if ("chemical" %in% names(df) & length(chemical) > 0) {
    stop("Chemical was set as both a function argument and a data frame column. Remove your chemical from one of these inputs.")
  }

  output <- chem %>%
    mutate(
      target_ph = target_ph,
      chemical = chemical
    ) %>%
    mutate(dose = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        chemical = chemical,
        target_ph = target_ph
      ),
      solvedose_ph
    )) %>%
    mutate(!!output_column := as.numeric(dose)) %>%
    select(-dose)
}

#' Apply `solvedose_alk` to a dataframe and create a new column with numeric dose
#'
#' This function allows \code{\link{solvedose_alk}} to be added to a piped data frame.
#' Its output is a chemical dose in mg/L.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has column(s) named "target_alk" or "chemical", the function will use the column(s)
#' as function argument(s). If these columns aren't present, specify "target_alk" or "chemical" as function arguments.
#' The chemical names must match the chemical names as displayed in \code{\link{solvedose_alk}}.
#' To see which chemicals can be dosed, see \code{\link{solvedose_alk}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_column name of the output column storing doses in mg/L. Default is "dose_required".
#' @param target_alk set a goal for alkalinity using the function argument or a data frame column
#' @param chemical select the chemical to be used to reach the desired alkalinity using function argument or data frame column
#' @seealso \code{\link{solvedose_alk}}
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
#'   balance_ions_chain() %>%
#'   mutate(
#'     target_alk = 300,
#'     chemical = rep(c("naoh", "na2co3"), 6)
#'   ) %>%
#'   solvedose_alk_once()
#'
#' # When the selected chemical can't raise the alkalinity, the dose_required will be NA
#' # Eg,soda ash can't bring the alkalinity to 100 when the water's alkalinity is already at 200.
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   solvedose_alk_once(input_water = "defined_water", target_alk = 100, chemical = "na2co3")
#'
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_alk = seq(100, 210, 10)) %>%
#'   solvedose_alk_once(chemical = "na2co3")
#'
#' \dontrun{
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_alk = seq(100, 210, 10)) %>%
#'   solvedose_alk_once(chemical = "na2co3")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns A data frame containing the original data frame and columns for target alkalinity, chemical dosed, and required chemical dose.

solvedose_alk_once <- function(df, input_water = "defined_water", output_column = "dose_required", target_alk = NULL, chemical = NULL) {
  dose <- NULL # Quiet RCMD check global variable note
  dosable_chems <- tibble(
    hcl = 0, h2so4 = 0, h3po4 = 0,
    co2 = 0,
    naoh = 0, caoh2 = 0, mgoh2 = 0,
    na2co3 = 0, nahco3 = 0
  )

  chem <- df %>%
    filter(chemical %in% names(dosable_chems))

  if (length(chemical) > 0) {
    if (!chemical %in% names(dosable_chems)) {
      stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_alk")
    }
  }

  if (length(chem$chemical) > 0 & !all(unique(df$chemical) %in% names(dosable_chems))) {
    stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_alk")
  }

  if ("target_alk" %in% names(df) & length(target_alk) > 0) {
    stop("Target alkalinity was set as both a function argument and a data frame column. Remove your target alkalinity from one of these inputs.")
  }

  if ("chemical" %in% names(df) & length(chemical) > 0) {
    stop("Chemical was set as both a function argument and a data frame column. Remove your chemical from one of these inputs.")
  }

  output <- chem %>%
    mutate(
      target_alk = target_alk,
      chemical = chemical
    ) %>%
    mutate(dose = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        chemical = chemical,
        target_alk = target_alk
      ),
      solvedose_alk
    )) %>%
    mutate(!!output_column := as.numeric(dose)) %>%
    select(-dose)
}
