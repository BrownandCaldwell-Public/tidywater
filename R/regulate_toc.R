#' @title Determine if TOC removal meets Stage 1 DBP Rule requirements
#' @description This function takes raw water alkalinity, raw water TOC, and finished water TOC.
#' It then calculates the TOC removal percentage and checks compliance with the Stage 1 DBP Rule.
#'
#' @details The function prints the input parameters and the calculated removal
#' percentage for TOC. It checks compliance with regulations considering the raw
#' TOC, alkalinity, and removal percentage. If the conditions are met, it prints
#' "In compliance"; otherwise, it prints "Not in compliance" and stops execution
#' with an error message.

#' @param raw_water A water object of class "water" created by [define_water()] containing raw water parameters. If provided, the function will use the `alk` and `toc` slots from this object as inputs. If not provided, `alk_raw` and `toc_raw` must be specified.
#' @param treated_water A water object of class "water" created by [define_water()] containing treated water parameters. If provided, the function will use the `toc` slot from this object as input for finished water TOC. If not provided, `toc_finished` must be specified.
#' @param alk_raw Raw water alkalinity (mg/L as calcium carbonate).
#' @param toc_raw Raw water total organic carbon (mg/L).
#' @param toc_finished Finished water total organic carbon (mg/L).
#' @param skip_alternate Logical value indicating whether to skip alternative compliance checks. By default, alternative compliance will be checked.
#'
#' @examples
#' regulate_toc(alk_raw=50, toc_raw=5, toc_finished=2)
#'
#' @export
#'
#' @returns A data frame containing the TOC removal compliance status.
#' 
#' @examples
#' regulate_toc(raw_water = define_water(alk = 50, toc = 5), treated_water = define_water(toc = 2), skip_alternate = TRUE)
#' 
#' @export
#' 
#' @returns A data frame containing the TOC removal compliance status, with alternative compliance checks skipped.
# See link here for regulations https://github.com/BrownandCaldwell/tidywater/issues/328
regulate_toc <- function(raw_water = "default", treated_water = "default", alk_raw, toc_raw, toc_finished, skip_alternate = FALSE) {
  required_compliance <- NA_real_

  # Initialize high so alternate checks do not trigger unless valid values are set
  suva_raw <- 100
  suva_treated <- 100
  tthm <- 100
  haa5 <- 100

  # If water objects are provided, use them as inputs
  if (!missing(raw_water) && !missing(treated_water)) {
    validate_water(raw_water, c("alk", "toc"))
    validate_water(treated_water, c("toc"))

    alk_raw <- raw_water@alk
    toc_raw <- raw_water@toc
    toc_finished <- treated_water@toc

    if (!skip_alternate) {
      # DBP-based alternate compliance checks
      if (is.na(treated_water@tthm) || is.na(treated_water@haa5)) {
        warning(
          "Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied."
        )
      } else {
        tthm <- treated_water@tthm
        haa5 <- treated_water@haa5
      }

      # SUVA checks
      if (is.na(raw_water@uv254) || is.na(raw_water@doc) || raw_water@doc == 0) {
        warning(
          "Raw water SUVA is missing or cannot be calculated. Alternative compliance based on SUVA (sect 141.135 (a)(2)(v)) will not be applied."
        )
      } else {
        suva_raw <- (raw_water@uv254 / raw_water@doc) * 100
      }

      if (is.na(treated_water@uv254) || is.na(treated_water@doc) || treated_water@doc == 0) {
        warning(
          "Treated water SUVA is missing or cannot be calculated. Alternative compliance based on SUVA (sect 141.135 (a)(2)(vi)) will not be applied."
        )
      } else {
        suva_treated <- (treated_water@uv254 / treated_water@doc) * 100
      }
    }
  }

  # Calculate removal percentage
  removal <- (toc_raw - toc_finished) / toc_raw * 100

  # Guard rail for non-removal cases
  if (removal <= 0) {
    warning("Finished water TOC is greater than or equal to raw TOC. No removal occurred.")
    return(data.frame(
      toc_compliance_status = "Not Calculated",
      toc_removal_percent = "Not Calculated"
    ))
  }

  # Alternative compliance
  if (!skip_alternate) {
    if (toc_raw <= 2) {
      warning("Raw water TOC < 2 mg/L, TOC is low enough (sect 141.135 (a)(2)(i)).")
      return(data.frame(
        toc_compliance_status = "In Compliance (Alternate see warning)",
        toc_removal_percent = "Not Calculated"
      ))
    } else if (toc_finished <= 2) {
      warning("Finished water TOC < 2 mg/L, TOC is low enough (sect 141.135 (a)(2)(ii))")
      return(data.frame(
        toc_compliance_status = "In Compliance (Alternate see warning)",
        toc_removal_percent = "Not Calculated"
      ))
    } else if (toc_raw < 4 && alk_raw > 60 && (0.001 * tthm < 0.04 || 0.001 * haa5 < 0.03)) {
      warning(
        "Raw water TOC < 4 mg/L, alkalinity > 60 mg/L, and DBPs (either TTHM < 0.04 mg/L or HAA5 < 0.03 mg/L); DBPs are low enough (sect 141.135 (a)(2)(iii))"
      )
      return(data.frame(
        toc_compliance_status = "In Compliance (Alternate see warning)",
        toc_removal_percent = "Not Calculated"
      ))
    } else if (0.001 * tthm < 0.04 && 0.001 * haa5 < 0.03) {
      warning("DBPs (TTHM < 0.04 mg/L and HAA5 < 0.03 mg/L); DBPs are low enough (sect 141.135 (a)(2)(iv)).")
      return(data.frame(
        toc_compliance_status = "In Compliance (Alternate see warning)",
        toc_removal_percent = "Not Calculated"
      ))
    } else if (suva_raw < 2) {
      warning("SUVA < 2. Alternative compliance based on SUVA; SUVA is low enough (sect 141.135 (a)(2)(v)).")
      return(data.frame(
        toc_compliance_status = "In Compliance (Alternate see warning)",
        toc_removal_percent = "Not Calculated"
      ))
    } else if (suva_treated < 2) {
      warning("SUVA < 2. Alternative compliance based on SUVA; SUVA is low enough (sect 141.135 (a)(2)(vi)).")
      return(data.frame(
        toc_compliance_status = "In Compliance (Alternate see warning)",
        toc_removal_percent = "Not Calculated"
      ))
    }
  }

  # Standard compliance
  match_row <- with(
    tidywater::toc_compliance_table,
    toc_raw > toc_min & toc_raw <= toc_max & alk_raw > alk_min & alk_raw <= alk_max
  )

  required_compliance <- tidywater::toc_compliance_table$required_compliance[match_row]

  if (length(required_compliance) > 0 && !is.na(required_compliance[1]) && removal >= required_compliance[1]) {
    return(data.frame(
      toc_compliance_status = "In Compliance",
      toc_removal_percent = as.character(round(removal, 1))
    ))
  }

  min_req <- if (length(required_compliance) > 0) required_compliance[1] else NA_real_

  data.frame(
    toc_compliance_status = "Not Compliant",
    toc_removal_percent = paste0(as.character(round(removal, 1)), "%"),
    comment = paste0("Minimum removal required: ", min_req, "%")
  )
}
#' @rdname regulate_toc
#'
#' @param df a data frame optionally containing columns for raw water alkalinity, raw water TOC, and finished water TOC
#'
#' @examples
#'
#' regulated <- water_df %>%
#'   dplyr::select(toc_raw = toc, alk_raw = alk) %>%
#'   regulate_toc_df(toc_finished = seq(0, 1.2, 0.1))
#'
#' regulated <- water_df %>%
#'   define_water_df() %>%
#'   chemdose_ph_df(alum = 30, output_water = "dosed") %>%
#'   chemdose_toc_df("dosed") %>%
#'   pluck_water(c("coagulated", "defined"), c("toc", "alk")) %>%
#'   dplyr::select(toc_finished = coagulated_toc, toc_raw = defined_toc, alk_raw = defined_alk) %>%
#'   regulate_toc_df()
#'
#' @export
#'
#' @returns A data frame with compliance status, removal percent, and optional note columns.

regulate_toc_df <- function(df, alk_raw = "use_col", toc_raw = "use_col", toc_finished = "use_col") {
  calc <- NULL # Quiet RCMD check global variable note

  alk_raw <- tryCatch(alk_raw, error = function(e) enquo(alk_raw))
  toc_raw <- tryCatch(toc_raw, error = function(e) enquo(toc_raw))
  toc_finished <- tryCatch(toc_finished, error = function(e) enquo(toc_finished))

  arguments <- construct_helper(
    df,
    all_args = list(
      "alk_raw" = alk_raw,
      "toc_raw" = toc_raw,
      "toc_finished" = toc_finished
    )
  )
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }

  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df,
    final_names,
    list(
      alk_raw = 0,
      toc_raw = 0,
      toc_finished = 0
    )
  )
  df <- defaults_added$data

  toc_list <- lapply(seq_len(nrow(df)), function(i) {
    regulate_toc(
      alk_raw = df[[final_names$alk_raw]][i],
      toc_raw = df[[final_names$toc_raw]][i],
      toc_finished = df[[final_names$toc_finished]][i]
    )
  })

  all_cols <- unique(unlist(lapply(toc_list, names)))
  toc_list_aligned <- lapply(toc_list, function(x) {
    x <- as.data.frame(x)
    missing <- setdiff(all_cols, names(x))
    for (col in missing) {
      x[[col]] <- NA
    }
    x[all_cols] # Reorder columns
  })

  toc_df <- do.call(rbind, toc_list_aligned)
  output <- cbind(df, toc_df)
  return(output)
}
