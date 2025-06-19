#' @title Determine if TOC meets requirements
#' @description This function takes input parameters for raw water including TOC,
#' pH, and alkalinity, and calculates the removal percentage for TOC. It then
#' checks compliance with regulations based on these inputs.
#'
#' @details The function prints the input parameters and the calculated removal
#' percentage for TOC. It checks compliance with regulations considering the raw
#' TOC, alkalinity, and removal percentage. If the conditions are met, it prints
#' "In compliance"; otherwise, it prints "Not in compliance" and stops execution
#' with an error message.
#' @param raw_toc Numeric value representing the raw TOC (mg/L).
#'
#' @param ph Numeric value representing the pH of the water.
#'
#' @param alk Numeric value representing the alkalinity (mg/L as calcium carbonate).
#'
#' @param final_toc Numeric value representing the final TOC (mg/L).
#'
#' @examples
#' regulate_toc(5, 7, 60, 2)
#'
#' @export
#'
#' @returns The function return "In compliance" if the conditions are met,
#' otherwise it stops execution with an error message.
#'
#'

# See link here for regulations https://github.com/BrownandCaldwell/tidywater/issues/328
regulate_toc <- function(water, raw_toc) {
  ph <- water@ph
  alk <- water@alk
  final_toc <- water@toc


  # Note from Libby: I don't think we need these print lines. tends to clutter up the console

  #Bengu: deleted the print lines

  #Calculate removal percentage for TOC:
  removal <- (raw_toc - final_toc) / raw_toc * 100
  # Note from Libby:  instead of printing this message let's add it to the dataframe output (Bengu:I added it to the dataframe output)
  required_compliance <- NA

  #Checking compliance considering inputs:

  if (raw_toc > 2 & raw_toc <= 4) {
    if (alk <= 60) required_compliance <- 35
    else if (alk > 60 & alk <= 120) required_compliance <- 25
    else if (alk > 120) required_compliance <- 15
  } else if (raw_toc > 4 & raw_toc <= 8) {
    if (alk <= 60) required_compliance <- 45
    else if (alk > 60 & alk <= 120) required_compliance <- 35
    else if (alk > 120) required_compliance <- 25
  } else if (raw_toc > 8) {
    if (alk <= 60) required_compliance <- 50
    else if (alk > 60 & alk <= 120) required_compliance <- 40
    else if (alk > 120) required_compliance <- 30
  }


  if (!is.na(required_compliance) & removal >= required_compliance) {
    return(tibble::tibble(toc_compliance_status = "In Compliance", toc_removal_percent = round(removal, 1)
                          # Note from Libby: add another column below showing the TOC removal percent (Bengu: column is added)

                          ))
  } else {
    return(tibble::tibble(
      toc_compliance_status = "Not Compliant", toc_removal_percent = round(removal, 1),
      # Note from Libby: add another column (same as the new column you made above), and add this note. (Bengu: column is added))
        new_col = paste0("Only ", round(removal, 1), "% TOC removed, requires minimum ", required_compliance, "% Compliance")
    ))
  }
}

# library(tidywater)
# library(tibble)

#test the function with raw parameters:

# water <- define_water(ph = 7, alk = 55, temp = 20, toc = 2, uv254 = 0.1, cond = 50) %>%
#    chemdose_toc(alum = 50) %>%
#   dissolve_pb()
#
#  test <- regulate_toc(water = water, raw_toc = 5)



library(dplyr)
library(tidyr)
library(furrr)
library(purrr)
library(devtools)


#' @rdname regulate_toc
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_chain]
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param raw_toc_col name of the column containing raw TOC values.
#' @param output_col_status name of the output column storing the compliance status. Default is "toc_compliance_status".
#' @param output_col_percent name of the output column storing TOC removal percent. Default is "toc_removal_percent".
#' @param output_col_note name of the output column storing compliance note. Default is "new_col".
#' @param water_prefix whether to prefix output columns with the input_water name. Default is TRUE.
#'
#' @export
#'
#' @returns A data frame with compliance status, removal percent, and optional note columns.

regulate_toc_once <- function(df, input_water = "defined_water", raw_toc_col = "raw_toc",
                              output_col_status = "toc_compliance_status",
                              output_col_percent = "toc_removal_percent",
                              output_col_note = "new_col", water_prefix = TRUE) {
  calc <- NULL # Quiet RCMD check global variable note
  
  validate_water_helpers(df, input_water)
  
  output <- df %>%
    mutate(calc = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        raw_toc = !!as.name(raw_toc_col)
      ),
      regulate_toc
    )) %>%
    tidyr::unnest_wider(calc)
  
  if (water_prefix) {
    output <- output %>%
      rename(
        !!paste(input_water, output_col_status, sep = "_") := toc_compliance_status,
        !!paste(input_water, output_col_percent, sep = "_") := toc_removal_percent,
        !!paste(input_water, output_col_note, sep = "_") := new_col
      )
  } else {
    output <- output %>%
      rename(
        !!output_col_status := toc_compliance_status,
        !!output_col_percent := toc_removal_percent,
        !!output_col_note := new_col
      )
  }
  
  return(output)
}


devtools::load_all()

test2 <- water_df %>%
  define_water_chain() %>%
  regulate_toc_once()





