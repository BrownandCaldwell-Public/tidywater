#' @title Calculate pH for water in an open system
#'
#' @description Calculates the new water quality (pH, alkalinity, pH dependent ions) for a water in an open system where CO2(aq) is at equilibrium with atmospheric CO2.
#' The function takes an object of class "water" and the partial pressure of CO2, then returns a water class object with updated water slots.
#' For a single water, use `opensys_ph`; to apply the model to a dataframe, use `opensys_ph_df`.
#' For most arguments, the `_df helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details
#'
#' `opensys_ph` uses the equilibrium concentration of CO2(aq) to determine the concentrations of carbonate species in the water and the pH. Carbonate alkalinity
#' and DIC are also updated given these new values.
#' 
#' @source Snoeyink & Jenkins (1980)
#'
#' @param water Source water of class "water" created by [define_water]
#' @param partialpressure Partial pressure of CO2 in the air in atm. Default is 10^-3.5 atm, which is approximately Pco2 at sea level.
#'
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 5) %>%
#'   opensys_ph()
#'
#' @export
#' @returns  A water with updated pH/alk/etc.
#'

opensys_ph <- function(water, partialpressure = 10^-3.42) {
  validate_water(water, slots = c("ph", "alk"))
  
  # kh <- 10^-1.468 # Henry's Law constant for CO2
  # co2_M <- kh * partialpressure
  # 
  # discons <- tidywater::discons
  # k1co3 <- K_temp_adjust(discons["k1co3", ]$deltah, discons["k1co3", ]$k, water@temp)
  # k2co3 <- K_temp_adjust(discons["k2co3", ]$deltah, discons["k2co3", ]$k, water@temp)
  # 
  # output_water <- water
  # output_water@ph <- - 0.5 * log10(k1co3 * kh * partialpressure) # proton balance and Henry's Law equation
  # output_water@h <- 10^-output_water@ph
  # output_water@oh <- 10^-14 / 10^-output_water@ph
  # 
  # alpha0 <- calculate_alpha0_carbonate(output_water@h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as H2CO3
  # alpha1 <- calculate_alpha1_carbonate(output_water@h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as HCO3-
  # alpha2 <- calculate_alpha2_carbonate(output_water@h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as CO32-
  # 
  # output_water@h2co3 <- co2_M
  # output_water@tot_co3 <- output_water@h2co3 / alpha0
  # output_water@hco3 <- alpha1 * output_water@tot_co3
  # output_water@co3 <- alpha2 * output_water@tot_co3
  # output_water@dic <- output_water@tot_co3 * tidywater::mweights$dic * 1000
  # output_water@alk_eq <- (output_water@hco3 + 2 * output_water@co3 + output_water@oh - output_water@h)
  # output_water@alk <- convert_units(output_water@alk_eq, formula = "caco3", startunit = "eq/L", endunit = "mg/L CaCO3")
  # 
  # return(output_water)
  
  opensys_fn <- function(par, water, co2_M, ...) {
    h <- 10^par[1]
    tot_co3 <- 10^par[2]
    
    ks <- correct_k(water)
    gamma1 <- calculate_activity(1, water@is, water@temp)
    charge <- water@kw / (h * gamma1^2) +
      water@tot_po4 * (calculate_alpha1_phosphate(h, ks) +
                         2 * calculate_alpha2_phosphate(h, ks) +
                         3 * calculate_alpha3_phosphate(h, ks)) +
      tot_co3 * (calculate_alpha1_carbonate(h, ks) +
                   2 * calculate_alpha2_carbonate(h, ks)) +
      water@free_chlorine * calculate_alpha1_hypochlorite(h, ks) +
      (water@tot_nh3 * calculate_alpha1_ammonia(h, ks)) -
      (water@alk_eq + water@oh) -
      3 * water@po4 - 2 * water@hpo4 - water@h2po4 - water@ocl + water@nh4
    
    h2co3 <- tot_co3 * calculate_alpha0_carbonate(h, ks)
    
    h2co3_diff_norm <- (h2co3 - co2_M) / 10^-5
    objective_value <- charge^2 + h2co3_diff_norm^2
    return(objective_value)
  }
  
  co2_M <- 10^-1.468 * partialpressure # 10^-1.468 is Henry's Constant for CO2
  results <- optim(par = c(log10(water@h), log10(co2_M)), fn = opensys_fn, water = water, co2_M = co2_M)
  h <- 10^(results$par[1])
  tot_co3 <- 10^(results$par[2])
  ph <- -log10(h)
  output <- water
  output@ph <- ph
  output@h <- h 
  output@oh <- output@kw / h
  output@tot_co3 <- tot_co3
  
  ks <- correct_k(water)
  alpha1 <- calculate_alpha1_carbonate(h, ks)
  alpha2 <- calculate_alpha2_carbonate(h, ks)
  output@h2co3 <- tot_co3 * calculate_alpha0_carbonate(h, ks)
  output@hco3 <- tot_co3 * alpha1
  output@co3 <- tot_co3 * alpha2
  
  carb_alk_eq <- output@tot_co3 * (alpha1 + 2 * alpha2) + output@oh - output@h
  output@alk_eq <- carb_alk_eq
  output@alk <- convert_units(output@alk_eq, "caco3", "eq/L", "mg/L CaCO3")
  
  return(output)
}

#' @rdname opensys_ph
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "opensys".
#' @param pluck_cols Extract water slots modified by the function (ph, alk) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   opensys_ph_df(
#'     input_water = "defined", output_water = "opensys",
#'     partialpressure = 10^-4, pluck_cols = TRUE
#'   )
#'
#' @export
#' @returns `opensys_ph_df` returns a data frame containing a water class column with updated ph and alk (and pH dependent ions).
#' Optionally, it also adds columns for each of those slots individually.

opensys_ph_df<- function(df, input_water = "defined", output_water = "opensys",
                                 pluck_cols = FALSE, water_prefix = TRUE,
                                 partialpressure = "use_col") {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  partialpressure <- tryCatch(partialpressure, error = function(e) enquo(partialpressure))

  arguments <- construct_helper(df, list("partialpressure" = partialpressure))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }

  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df, final_names,
    list(partialpressure = 10^-3.42)
  )
  df <- defaults_added$data
  
  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    opensys_ph(
      water = df[[input_water]][[i]],
      partialpressure = df[[final_names$partialpressure]][i]
    )
  })
  
  output <- df[, !names(df) %in% defaults_added$defaults_used]
  output <- df
  
  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("ph", "alk"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }
  
  return(output)
}
