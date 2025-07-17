#' @title Determine if TOC removal meets Stage 1 DBP Rule requirements
#' @description This function takes raw water alkalinity, raw water TOC, and finished water TOC.
#' It then calculates the TOC removal percentage and checks compliance with the Stage 1 DBP Rule.
#'
#' @details The function prints the input parameters and the calculated removal
#' percentage for TOC. It checks compliance with regulations considering the raw
#' TOC, alkalinity, and removal percentage. If the conditions are met, it prints
#' "In compliance"; otherwise, it prints "Not in compliance" and stops execution
#' with an error message.

#' @param alk_raw Raw water alkalinity (mg/L as calcium carbonate).
#' @param toc_raw Raw water total organic carbon (mg/L).
#' @param toc_finished Finished water total organic carbon (mg/L).
#'
#' @examples
#' regulate_toc(50, 5, 2)
#'
#' @export
#'
#' @returns A data frame containing the TOC removal compliance status.

# See link here for regulations https://github.com/BrownandCaldwell/tidywater/issues/328
regulate_toc <- function(alk_raw, toc_raw, toc_finished) {
  required_compliance <- NA
  removal <- (toc_raw - toc_finished) / toc_raw * 100

  if (removal <= 0){
    warning("Finished water TOC is greater than or equal raw TOC. No removal ocurred.")
    return(tibble::tibble(
      toc_compliance_status = "Not Calculated",
      toc_removal_percent = "Not Calculated"
    ))
  }

  if (toc_raw <= 2 ){
    warning("Raw water TOC < 2 mg/L. No regulation applies.")
    return(tibble::tibble(
      toc_compliance_status = "Not Calculated",
      toc_removal_percent = "Not Calculated"))
  }

  match_row <- with(tidywater::toc_compliance_table,
                    toc_raw > toc_min & toc_raw <= toc_max &
                      alk_raw > alk_min & alk_raw <= alk_max
  )

  required_compliance <- tidywater::toc_compliance_table$required_compliance[match_row]

  if (length(required_compliance) > 0 && !is.na(required_compliance) && removal >= required_compliance) {
    return(data.frame(
      toc_compliance_status = "In Compliance",
      toc_removal_percent = round(removal, 1)
                          ))
  } else {
    return(data.frame(
      toc_compliance_status = "Not Compliant",
      toc_removal_percent = as.character(round(removal, 1)),
      comment = paste0("Only ", round(removal, 1), "% TOC removed, requires minimum ", required_compliance, "% Compliance")
    )
    )
  }
}

#' @rdname regulate_toc
#'
#' @param df a data frame optionally containing columns for raw water alkalinity, raw water TOC, and finished water TOC
#'
#'  @examples
#'
#' regulated <- water_df %>%
#'   select(toc_raw = toc, alk_raw = alk) %>%
#'   regulate_toc_once(toc_finished = seq(0,1.2, 0.1))
#'
#' regulated <- water_df %>%
#'   define_water_chain() %>%
#'   chemdose_ph_chain(alum = 30, output_water = "dosed") %>%
#'   chemdose_toc_chain("dosed") %>%
#'   pluck_water(c("coagulated_water", "defined_water"), c("toc", "alk")) %>%
#'   select(toc_finished = coagulated_water_toc, toc_raw = defined_water_toc, alk_raw = defined_water_alk) %>%
#'   regulate_toc_once()
#'
#' @export
#'
#' @returns A data frame with compliance status, removal percent, and optional note columns.

regulate_toc_once <- function(df, alk_raw = "use_col", toc_raw = "use_col", toc_finished = "use_col") {
  calc <- NULL # Quiet RCMD check global variable note

  alk_raw <- tryCatch(alk_raw, error = function(e) enquo(alk_raw))
  toc_raw <- tryCatch(toc_raw, error = function(e) enquo(toc_raw))
  toc_finished <- tryCatch(toc_finished, error = function(e) enquo(toc_finished))

  arguments <- construct_helper(df, all_args = list(
    "alk_raw" = alk_raw, "toc_raw" = toc_raw, "toc_finished" = toc_finished
    ))
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }

  output <- df %>%
    mutate(calc = furrr::future_pmap(
      list(
        alk_raw = if (final_names$alk_raw %in% names(.)) !!sym(final_names$alk_raw) else rep(0, nrow(.)),
        toc_raw = if (final_names$toc_raw %in% names(.)) !!sym(final_names$toc_raw) else rep(0, nrow(.)),
        toc_finished = if (final_names$toc_finished %in% names(.)) !!sym(final_names$toc_finished) else rep(0, nrow(.))
      ),
      regulate_toc
    )) %>%
    tidyr::unnest_wider(calc)

}
