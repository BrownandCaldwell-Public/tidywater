#' @title Determine ozone decay
#'
#' @description This function applies the ozone decay model to a water created by \code{\link{define_water}}
#' from U.S. EPA (2001) equation 5-128.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}.
#' @param dose Applied ozone dose in mg/L
#' @param time Ozone contact time in minutes
#'
#' @source U.S. EPA (2001)
#' @source See reference list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @examples
#' ozone_resid <- define_water(7, 20, 100, doc = 2, toc = 2.2, uv254 = .02, br = 50) %>%
#'   solveresid_o3(dose = 2, time = 10)
#'
#' @export
#'
solveresid_o3 <- function(water, dose, time) {
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

#' Apply `solveresid_o3` to a data frame and create a new column with residual ozone dose
#'
#' This function allows \code{\link{solveresid_o3}} to be added to a piped data frame.
#' Once additional column will be added to the data frame; the residual ozone dose (mg/L)
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}.
#' @param dose Applied ozone dose in mg/L
#' @param time Ozone contact time in minutes
#'
#' @examples
#' ozone_resid <- water_df %>% 
#'   mutate(br =50) %>% 
#'   define_water_chain() %>% 
#'   solveresid_o3_once(dose = 2, time = 10)
#'
#' ozone_resid <- water_df %>% 
#'   mutate(br =50) %>% 
#'   define_water_chain() %>% 
#'   mutate(dose = seq(1,12, 1),
#'          time = seq(2,24, 2)) %>% 
#'   solveresid_o3_once()
#' @export
#'
solveresid_o3_chain <- function(df, input_water = "defined_water",
                                dose = 0, time = 0) {
  inputs_arg <- tibble(dose, time) %>%
    select_if(~ any(. > 0))

  inputs_col <- df %>%
    subset(select = names(df) %in% c("dose", "time")) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(inputs_col) < 2 & length(inputs_arg) == 0) {
    warning("Dose and time arguments missing. Add them as a column or function argument.")
  }

  if (("dose" %in% colnames(inputs_arg) & "dose" %in% colnames(inputs_col)) | ("time" %in% colnames(inputs_arg) & "time" %in% colnames(inputs_col))) {
    stop("Dose and/or time were dosed as both a function argument and a data frame column. Choose one input method.")
  }

  dose_time <- inputs_col %>%
    cross_join(inputs_arg)

  output <- df %>%
    subset(select = !names(df) %in% c("dose", "time")) %>%
    mutate(
      ID = row_number()
    ) %>%
    left_join(dose_time, by = "ID") %>%
    select(-ID) %>%
    mutate(o3resid = furrr::future_pmap_dbl(
      list(
        water = !!as.name(input_water),
        dose = dose,
        time = time
      ),
      solveresid_o3
    ))
}
