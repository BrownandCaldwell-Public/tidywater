#' @title Determine ozone decay
#'
#' @description This function applies the ozone decay model to a `water`
#' from U.S. EPA (2001) equation 5-128.
#' For a single water, use `solveresid_o3`; to apply the model to a dataframe, use `solveresid_o3_once`.
#' For most arguments, the `_once` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @param water Source water object of class `water` created by [define_water]
#' @param dose Applied ozone dose in mg/L
#' @param time Ozone contact time in minutes
#'
#' @source U.S. EPA (2001)
#' @source See reference list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @examples
#' ozone_resid <- define_water(7, 20, 100, doc = 2, toc = 2.2, uv254 = .02, br = 50) %>%
#'   solveresid_o3(dose = 2, time = 10)
#'
#' @export
#' @returns `solveresid_o3` returns a numeric value for the residual ozone.
solveresid_o3 <- function(water, dose, time) {
  validate_water(water, c("ph", "temp", "alk", "doc", "uv254", "br"))

  doc <- water@doc
  ph <- water@ph
  temp <- water@temp
  uv254 <- water@uv254
  suva <- water@uv254 / water@doc * 100
  alk <- water@alk
  br <- water@br

  # Model from WTP model
  o3demand <- 0.995 * dose^1.312 * (dose / uv254)^-.386 * suva^-.184 * (time)^.068 * alk^.023 * ph^.229 * temp^.087
  o3residual <- dose - o3demand
  o3residual

  # residual <- A * exp(k * time)
  # residual
}


#' @rdname solveresid_o3
#' @param df a data frame containing a water class column, which has already been computed using \code{\link{define_water_chain}}
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param output_column name of the output column storing doses in mg/L. Default is "dose_required".
#'
#' @examples
#' library(dplyr)
#' ozone_resid <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   solveresid_o3_once(dose = 2, time = 10)
#'
#' ozone_resid <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   mutate(
#'     dose = seq(1, 12, 1),
#'     time = seq(2, 24, 2)
#'   ) %>%
#'   solveresid_o3_once()
#'
#' @import dplyr
#' @export
#' @returns `solveresid_o3_once` returns a data frame containing the original data frame and columns for ozone dosed, time, and ozone residual.

solveresid_o3_once <- function(df, input_water = "defined_water", output_column = "o3resid",
                               dose = "use_col", time = "use_col") {
  ID <- NULL # Quiet RCMD check global variable note

  # This allows for the function to process unquoted column names without erroring
  time <- tryCatch(time, error = function(e) enquo(time))
  dose <- tryCatch(dose, error = function(e) enquo(dose))

  arguments <- construct_helper(df, list("time" = time, "dose" = dose))

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(!!output_column := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        time = !!as.name(arguments$final_names$time),
        dose = !!as.name(arguments$final_names$dose)
      ),
      solveresid_o3
    ))
}
