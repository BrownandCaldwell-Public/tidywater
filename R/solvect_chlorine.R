# CT Calculations

#' Determine disinfection credit from chlorine.
#'
#' @description This function takes a water defined by [define_water] and other disinfection parameters
#' and outputs a data frame of the required CT (`ct_required`), actual CT (`ct_actual`), and giardia log removal (`glog_removal`).
#' For a single water, use `solvect_chlorine`; to apply the model to a dataframe, use `solvect_chlorine_df`.
#' For most arguments, the `_df` helpers
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details CT actual is a function of time, chlorine residual, and baffle factor, whereas CT required is a function of
#' pH, temperature, chlorine residual, and the standard 0.5 log removal of giardia requirement.  CT required is an
#' empirical regression equation developed by Smith et al. (1995) to provide conservative estimates for CT tables
#' in USEPA Disinfection Profiling Guidance.
#' Log removal is a rearrangement of the CT equations.
#'
#' @source Smith et al. (1995)
#' @source USEPA (2020)
#' @source USEPA (1991)
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}. Water must include ph and temp
#' @param time Retention time of disinfection segment in minutes.
#' @param residual Minimum chlorine residual in disinfection segment in mg/L as Cl2.
#' @param baffle Baffle factor - unitless value between 0 and 1.
#' @param free_cl_slot Defaults to "residual_only", which uses the residual argument. If "slot_only", the model will use the
#' free_chlorine slot in the input water. "sum_with_residual", will use the sum of the residual argument and the free_chlorine slot.
#'
#' @examples
#'
#' example_ct <- define_water(ph = 7.5, temp = 25) %>%
#'   solvect_chlorine(time = 30, residual = 1, baffle = 0.7)
#' @export
#'
#' @returns `solvect_chlorine` returns a data frame containing required CT (mg/L*min), actual CT (mg/L*min), giardia log removal, and virus log removal.

solvect_chlorine <- function(water, time, residual, baffle, free_cl_slot = "residual_only") {
  if (free_cl_slot == "slot_only") {
    validate_water(water, c("ph", "temp", "free_chlorine"))
    residual <- water@free_chlorine
  } else if (free_cl_slot == "sum_with_residual") {
    validate_water(water, c("ph", "temp", "free_chlorine"))
    residual <- residual + water@free_chlorine
  } else {
    validate_water(water, c("ph", "temp"))
  }

  validate_args(num_args = list("time" = time, "residual" = residual, "baffle" = baffle))

  ph <- water@ph
  temp <- water@temp

  ct_actual <- residual * time * baffle

  if (temp < 12.5) {
    ct_required <- (.353 * .5) * (12.006 + exp(2.46 - .073 * temp + .125 * residual + .389 * ph))
    giardia_log_removal <- ct_actual / (12.006 + exp(2.46 - .073 * temp + .125 * residual + .389 * ph)) * 1 / .353
  } else {
    ct_required <- (.361 * 0.5) * (-2.216 + exp(2.69 - .065 * temp + .111 * residual + .361 * ph))
    giardia_log_removal <- ct_actual / (-2.216 + exp(2.69 - .065 * temp + .111 * residual + .361 * ph)) / .361
  }

  # determine virus log removal based on EPA Guidance Manual Table E-7
  if (ph < 6 | ph > 10) {
    vlog_removal <- NA_real_
    warning("pH is out of range: virus log removal calculation only valid when pH is between 6-10.")
  } else {
    if (ph > 9 & ph < 10) {
      ph <- round(ph)
      warning("Virus log removal estimated to closest pH in EPA Guidance Manual Table E-7")
    }
    tempr <- tidywater::vlog_removalcts$temp_value[which.min(abs(tidywater::vlog_removalcts$temp_value - temp))]
    if (temp != tempr) {
      warning("Virus log removal estimated to closest temperature in EPA Guidance Manual Table E-7")
    }

    # Determine ph_range key
    ph_key <- if (ph >= 6 && ph <= 9) "6-9" else if (ph == 10) "10" else NULL

    # Filter the relevant rows
    vlog_table <- subset(
      tidywater::vlog_removalcts,
      tidywater::vlog_removalcts$ph_range == ph_key & tidywater::vlog_removalcts$temp_value == tempr
    )
    # Extract correct ct_range
    ct_labels <- vlog_table$ct_range
    get_breaks <- function(ranges) {
      nums <- unlist(strsplit(ranges, "-"))
      as.numeric(nums)
    }
    breaks <- sort(unique(unlist(lapply(ct_labels, get_breaks))))
    breaks <- c(breaks, Inf) # Add Inf for the upper bound
    ct_category <- cut(ct_actual, breaks = breaks, labels = ct_labels, right = FALSE)

    vlog_removal <- vlog_table[vlog_table$ct_range == as.character(ct_category), "vlog_removal"]

    if (any(is.na(vlog_removal))) {
      vlog_removal <- NA_real_
      warning("pH or contact time out of range for virus log removal calculation. See EPA Guidance Manual Table E-7 for valid ranges.")
    }
  }

  tibble("ct_required" = ct_required, "ct_actual" = ct_actual, "glog_removal" = giardia_log_removal, "vlog_removal" = vlog_removal)
}


#' @rdname solvect_chlorine
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_df]
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param water_prefix name of the input water used for the calculation will be appended to the start of output columns. Default is TRUE.
#'
#' @examples
#' ct_calc <- water_df %>%
#'   define_water_df() %>%
#'   solvect_chlorine_df(residual = 2, time = 10, baffle = .5)
#'
#' chlor_resid <- water_df %>%
#'   dplyr::mutate(br = 50) %>%
#'   define_water_df() %>%
#'   dplyr::mutate(
#'     residual = seq(1, 12, 1),
#'     time = seq(2, 24, 2),
#'     baffle = 0.7
#'   ) %>%
#'   solvect_chlorine_df()
#'
#' @returns `solvect_chlorine_df` returns a data frame containing the original data frame and columns for required CT, actual CT, and giardia log removal.
#' @export

solvect_chlorine_df <- function(df, input_water = "defined",
                                  time = "use_col", residual = "use_col", baffle = "use_col",
                                  free_cl_slot = "residual_only",
                                  water_prefix = TRUE) {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  time <- tryCatch(time, error = function(e) enquo(time))
  residual <- tryCatch(residual, error = function(e) enquo(residual))
  baffle <- tryCatch(baffle, error = function(e) enquo(baffle))

  arguments <- construct_helper(df, list("time" = time, "residual" = residual, "baffle" = baffle))
  final_names <- arguments$final_names
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }

  ct_df <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    solvect_chlorine(
      water = df[[input_water]][[i]],
      time = df[[final_names$time]][i],
      residual = df[[final_names$residual]][i],
      baffle = df[[final_names$baffle]][i],
      free_cl_slot = free_cl_slot
    )
  }))

  if(water_prefix) {
    names(ct_df) <- paste0(input_water, "_", names(ct_df))
  }

  output <- cbind(df, ct_df)
  return(output)

}
