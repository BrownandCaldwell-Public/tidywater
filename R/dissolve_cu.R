# Calculate dissolved copper

#' @title Calculate Dissolved Copper Concentration
#' @description
#' This function takes a water defined by defined_water and output a column of dissolved copper.
#' For a single water, use `dissolve_cu`; to apply the model to a dataframe use `dissolve_cu_chain`.
#'
#' @details Dissolved copper is a function of pH, DIC, and PO4. Output units are in mg/L. You may need to calculate DIC upsteam of this function
#' using calculate_dic.
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#' for the option to use parallel processing and speed things up. To initialize parallel processing, use
#' `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#' `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#' shorter run times will not benefit from parallel processing.
#'
#' @source Lytle et al (2018)
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph and dic
#'
#' @examples
#'
#' example_cu <- define_water(ph = 7.5, alk = 125) %>%
#'   dissolve_cu()
#' @export
#'
#' @returns `dissolve_cu` returns a column containing dissolved copper concentration in mg/L.
#'

dissolve_cu <- function(water, dic) {
  validate_water(water, c("ph", "dic"))
  validate_args(num_args = list("ph" = ph, "dic" = dic, "tot_po4" = tot_po4))

  po4 <- convert_units(water@tot_po4, "h3po4", "M", "mg/L")

  cu <- 56.68 * (exp(-0.77 * water@ph)) * exp(-0.20 * po4) * (dic^0.59)
  data.frame(cu)


}


#' @rdname dissolve_cu_once
#'
#' @title Calculate Dissolved Copper Concentration
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_chain]
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#'
#'@examples
#' library(dplyr)
#' cu_calc <- water_df %>%
#'   define_water_chain() %>%
#'   dissolve_cu_once()
#'
#' @returns `solvect_chlorine_once` returns a data frame containing the original data frame and a column for dissolved copper in mg/L.
#'
#' #' @import dplyr
#' @export
#'


dissolve_cu_once <- function(df, input_water = "defined_water", dic) {

  output <- df %>%
    mutate(calc = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        dic = dic
      ),
      dissolve_cu
    )) %>%
    unnest_wider(calc)
}


#testing
water <- water_df %>%
  mutate(dic = 30) %>%
  define_water_chain() %>%
  dissolve_cu_once()









