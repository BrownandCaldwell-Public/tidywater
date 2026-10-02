#' @title OH radical scavenging function
#'
#' @description The background hydroxyl radical scavenging term is a measure of the degradation rate of non-target constituents in a water. The term is defined as the 
#' sum of the reaction rate of the constituent multiplied by the concentration of the constituent: `sum(k_i * [C]i)` where `k_i` is the reaction rate of a contaminant and `[C]i` its concentration.
#' Supporting Information in Wang et al. 2019 provides the definition and additional methods for calculating background scavenging.
#' For a data frame of waters, use `scavenge_oh_df`, which adds the scavenging term as a new column.
#'
#' @details Rate constants used in the code:
#' * DOC: 2.5e4 L/mg C/s (Larson and Zepp 1988; 3.0e8 M_C^-1 s^-1 divided by 12,011 mg C/mol)
#' * HCO3-: 8.5e6 M^-1 s^-1 (Buxton et al. 1988)
#' * CO3 2-: 3.9e8 M^-1 s^-1 (Buxton et al. 1988)
#' * NO2-: 1.0e10 M^-1 s^-1 (Coddington et al. 1999; Vinge et al. 2020)
#' * NO3-: 1.0e5 M^-1 s^-1 (Buxton et al. 1988)
#' * H2O2: 2.7e7 M^-1 s^-1 (Buxton et al. 1988)
#'
#' Other reported DOC rate constants, for use as `kdoc_adjust`. Sources report these per mole of carbon
#' (M_C^-1 s^-1); they are converted to L/mg C/s by dividing by 12,011 mg C/mol. Brezonik and
#' Katsoyiannis values are as compiled in Wols and Hofman-Caris (2012), Table 2.
#' * 2.41e4 (2.9e8 M_C^-1 s^-1): lake and river water, Brezonik and Fulkerson-Brekken (1998)
#' * 2.25e4 (2.7e8 M_C^-1 s^-1): Lake Zurich, Katsoyiannis et al. (2011)
#' * 1.67e4 (2.0e8 M_C^-1 s^-1): Lake Jonsvatnet, Katsoyiannis et al. (2011)
#' * 1.75e4 (2.1e8 M_C^-1 s^-1): Lake Greifensee, Katsoyiannis et al. (2011)
#' * 2.91e4 (3.5e8 M_C^-1 s^-1): effluent organic matter, Katsoyiannis et al. (2011)
#' * 1.67e4 (2.0e8 M_C^-1 s^-1): model value, Wols et al. (2014), Table 3
#' * 5.29e4 (6.35e8 M_C^-1 s^-1): midpoint of the 3.5e8 to 9.2e8 range cited by Vinge et al. (2021)
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source Brezonik and Fulkerson-Brekken (1998)
#' @source Buxton et al. (1988)
#' @source Coddington et al. (1999)
#' @source Katsoyiannis et al. (2011)
#' @source Larson and Zepp (1988)
#' @source Vinge et al. (2020)
#' @source Vinge et al. (2021)
#' @source Wang et al. (2019)
#' @source Wols and Hofman-Caris (2012)
#' @source Wols et al. (2014)
#' 
#' @param water Source water of tidywater 'water' object type 
#' @param dose Hydrogen peroxide (h2o2) dose applied in mg/L, defaults to 0 
#' @param kdoc_adjust Optional input for a different k_DOC rate constant for fitting data, defaults to 2.5e4 L/mg C/s. See Details for other reported values.
#' 
#' @examples
#' water1 <- define_water(ph=7, doc=3, alk = 70, no3 = 4)
#' h2o2_dose<-20
#' 
#' background_scavenging <- scavenge_oh(water1, h2o2_dose)
#' 
#' background_SC_adjusted <- scavenge_oh(water1, h2o2_dose, 48065)
#'
#' @export
#' 
#' @returns `scavenge_oh` returns the total background hydroxyl radical scavenging term in units of s^-1.
#' 

scavenge_oh <- function(water, dose = 0, kdoc_adjust = 2.5e4) {

  # Should still work with DI water, so will not check any specific water slots
  validate_water(water, c())
  dose_M <- convert_units(dose, "h2o2", "mg/L", "M")

  # List of each constituent k value to access easier and for clarity.
  k <- list(
    doc = kdoc_adjust, #L/mg s-1
    hco3 = 8.50E+06, # M-1 s-1
    co3 = 3.90E+08, # M-1 s-1
    no2 = 1.00E+10, # M-1 s-1
    no3 = 1.00E+05, # M-1 s-1
    h2o2 = 2.70E+07 # M-1 s-1
  )

  k_S <- list(
    doc = k$doc * water@doc, # s-1
    no3 = k$no3 * water@no3, # s-1
    no2 = k$no2 * water@no2, # s-1
    h2o2 = k$h2o2 * dose_M, # s-1
    hco3 = k$hco3 * water@hco3, # s-1
    co3 = k$co3 * water@co3 # s-1
  )

  # Warn when a major scavenger is missing. no2 and no3 are usually NA and contribute little, so they are not reported.
  missing_slots <- intersect(names(k_S)[vapply(k_S, anyNA, logical(1))], c("doc", "hco3", "co3"))
  if (length(missing_slots) > 0) {
    tidywater_warn("Missing {.val {missing_slots}} treated as 0 in the OH radical scavenging term.")
  }

  # turn each NA value to 0 because NA's will mess up execution and 0's just won't contribute anything.
  k_S <- lapply(k_S, function(x) {
    x[is.na(x)] <- 0
    return(x)
  })

  # sum all of the scavenging terms
  SC <- Reduce(`+`, k_S)
  return(SC)
}

#' @rdname scavenge_oh
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include columns named for the H2O2 dose and the DOC rate constant.
#' @param input_water name of the column of water class data to be used as the input. Default is "defined".
#' @param output_column name of the output column storing the OH radical scavenging term in s^-1. Default is
#' "oh_scavenging".
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   scavenge_oh_df(dose = 10)
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(h2o2 = seq(2, 24, 2)) %>%
#'   scavenge_oh_df(dose = h2o2, kdoc_adjust = 1.67e4, output_column = "sc")
#'
#' @export
#' @returns `scavenge_oh_df` returns a data frame containing the original data frame and a column with the
#' background hydroxyl radical scavenging term in s^-1.

scavenge_oh_df <- function(
  df,
  input_water = "defined",
  output_column = "oh_scavenging",
  dose = "use_col",
  kdoc_adjust = "use_col"
) {
  # This allows for the function to process unquoted column names without erroring
  dose <- tryCatch(dose, error = function(e) enquo(dose))
  kdoc_adjust <- tryCatch(kdoc_adjust, error = function(e) enquo(kdoc_adjust))

  validate_water_helpers(df, input_water)

  arguments <- construct_helper(df, list("dose" = dose, "kdoc_adjust" = kdoc_adjust))
  final_names <- arguments$final_names
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(df, final_names, list(dose = 0, kdoc_adjust = 2.5e4))
  df <- defaults_added$data

  warning_counts <- list()

  df[[output_column]] <- vapply(
    seq_len(nrow(df)),
    function(i) {
      withCallingHandlers(
        scavenge_oh(
          water = df[[input_water]][[i]],
          dose = df[[final_names$dose]][i],
          kdoc_adjust = df[[final_names$kdoc_adjust]][i]
        ),
        warning = function(w) {
          msg <- conditionMessage(w)
          warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) + 1L
          invokeRestart("muffleWarning")
        }
      )
    },
    numeric(1)
  )

  for (msg in names(warning_counts)) {
    tidywater_warn("{msg} ({warning_counts[[msg]]} row{?s} affected.)")
  }

  # Keep the result even if output_column shares a name with a defaulted argument
  output <- df[, !names(df) %in% setdiff(defaults_added$defaults_used, output_column)]
  return(output)
}
