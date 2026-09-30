#' @title Determine TOC removal from biofiltration using Terry & Summers BDOC model or Peterson & Summers TOC model
#'
#' @description This function applies the Terry model to a water created by [define_water] to determine biofiltered
#' DOC (mg/L). The Terry & Summers (2018) model does not adjust for ozone dose but assumes a constant biodegradable fraction based on 
#' whether or not the water was ozonated. When the reuse argument is TRUE, the Peterson and Summers (2021) model is used to determine biofiltered TOC (mg/L).
#' The Peterson & Summers (2021) model does not adjust for temperature impacts due to a lack of collected temperature data in the metanalysis literature. 
#' Peterson & Summers (2021) is also a TOC-basis model, so the output water object's BDOC slot will not be updated.
#' If a lab-measured ATP concentration is known, the Terry et al. (2024) model can be used to determine biofiltered DOC (mg/L) for both drinking water and reuse.
#' The Terry et al. (2024) model has similar bdoc assumptions as Terry & Summers (2018), but the model ATP value can be adjusted for biofilter water temperature.
#' For all three models, all particulate TOC is assumed to be removed so TOC = DOC.
#' For a single water use `biofilter_toc`; for a dataframe use `biofilter_toc_df`.
#' Use `pluck_cols = TRUE` to get values from the output water as new dataframe columns.
#' For most arguments in the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @param water Source water object of class "water" created by [define_water].
#' @param ebct The empty bed contact time (min) used for the biofilter.
#' @param ozonated Logical; TRUE if the water is ozonated (default), FALSE otherwise.
#' @param ozone_dose The ozone dose (mg/L) applied to the water. Only used if `reuse = TRUE`.
#' @param reuse Logical; TRUE if the water is effluent from a reuse treatment process, FALSE otherwise. 
#' If TRUE, the Peterson and Summers (2021) model is used to determine TOC removal. If FALSE, the Terry and Summers (2018) model is used to determine BDOC removal.
#' @param media_type The type of media used in the biofilter. Default is "anthracite", other options are "gac" and "sand". Only used if `reuse = TRUE`.
#' @param atp The measured adenosine triphosphate (ATP) concentration at the top of the biofilter (ng/mL at 22 degrees Celcius). Optional argument that will use Terry et al. (2024) model for both drinking water and reuse if nonzero value is provided. (This will also ignore ozone dose and media type for reuse models.) Defaults to 0.
#'
#'
#' @source Terry and Summers 2018
#' @source Peterson and Summers 2021
#' @source Terry et al. 2024
#' 
#' @examples
#' water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1) %>%
#'   biofilter_toc(ebct = 10, ozonated = FALSE)
#'
#' @returns  `biofilter_toc` returns water class object with modeled DOC removal from biofiltration.
#' @export
#'
biofilter_toc <- function(water, ebct, ozonated = TRUE, ozone_dose = 0, reuse = FALSE, media_type = "anthracite", atp = 0) {
  if (!is.logical(ozonated)) {
    cli::cli_abort("{.arg ozonated} must be {.code TRUE} or {.code FALSE}.")
  }

  temp <- water@temp

  validate_water(water, "doc")

  if (reuse == TRUE & atp == 0) {
    # Peterson and Summers (2021)
    spec_ozone <- ozone_dose / water@toc
    if (spec_ozone < 0 | spec_ozone > 1.2) {
      cli::cli_warn("Peterson and Summers model was validated for an ozone to TOC ratio of 0 - 1.2. Model accuracy may be reduced outside this range.")
    }
    if (ebct < 5 | ebct > 30) {
      cli::cli_warn("Peterson and Summers model was validated for EBCT range of 5 - 30 min. Model accuracy may be reduced outside this range.")
    }

    # assumes media type associated with 0.0873 is BAC
    media <- ifelse(media_type == "gac", 1, 0)
    scaled_ozone <- (spec_ozone - 0.48) / 0.44 # mean and std dev values taken from Table 2
    scaled_ebct <- (ebct - 14.4) / 8.5

    toc_removal_percent <- 0.0873 * media + 0.0527 * scaled_ozone + 0.0393 * scaled_ebct + 0.0139 * scaled_ozone * scaled_ebct + 0.1323
    TOC_removed <- water@toc * toc_removal_percent

    # Update water object with new TOC and DOC values
    water@toc <- water@toc - TOC_removed
    water@doc <- max(water@doc - TOC_removed, 0)
    water@bdoc <- water@bdoc # model built on TOC basis so BDOC is not updated

  } else if (reuse == FALSE & atp == 0) {
    # Terry and Summers (2018)

    # Define BDOC fractions
    BDOC_fraction_nonozonated <- 0.2
    BDOC_fraction_ozonated <- 0.3

    # Determine BDOC fraction and rate constant k' based on temperature and ozonation
    if (ozonated) {
      BDOC_fraction <- BDOC_fraction_ozonated
      if (temp <= 10) {
        k <- 0.03
      } else if (temp <= 20) {
        k <- 0.06
      } else {
        k <- 0.15
      }
    } else {
      BDOC_fraction <- BDOC_fraction_nonozonated
      if (temp <= 10) {
        k <- 0.03
      } else if (temp <= 20) {
        k <- 0.09
      } else {
        k <- 0.11
      }
    }

    # Calculate BDOC influent concentration
    BDOC_inf <- BDOC_fraction * water@doc

    # Calculate BDOC effluent concentration using the exponential decay model
    BDOC_eff <- BDOC_inf * exp(-k * ebct)

    # Calculate TOC removal percentage
    BDOC_removed <- (BDOC_inf - BDOC_eff)

    # Update water object with new TOC and DOC values
    doc_eff <- water@doc - BDOC_removed
    water@doc <- doc_eff
    water@toc <- doc_eff
    water@bdoc <- BDOC_eff
  } else if (atp > 0) {
    # Terry et al. (2024) model for both drinking water and reuse when ATP data is available
    if (ebct < 5 | ebct > 30) {
      cli::cli_warn("Terry et al. model was validated for EBCT range of 5 - 30 min. Model accuracy may be reduced outside this range.")
    }
    
    if (reuse == FALSE) {
      k <- 1.8E-04 # modeled k'' for drinking water from Table 2
      bdoc_fraction <- 0.2
    } else if (reuse == TRUE & ozonated == TRUE) {
      k <- 0.40E-04 # modeled k'' for wastewater effluent
      bdoc_fraction <- 0.6
    } else if (reuse == TRUE & ozonated == FALSE) {
      k <- 0.40E-04
      bdoc_fraction <- 0.4
    }
    water@bdoc <- bdoc_fraction * water@doc # Update BDOC slot with initial concentration
    
    # adjust for temp in ATP measurement, assumes lab temp of 22C
    atp_filter <- atp * (0.034 * temp + 0.203) # Eqn 5
    x_total <- atp_filter * (0.132 * (ebct * log(ebct) - ebct) + 0.706 * ebct) # Eqn 7

    bdoc0 <- bdoc_fraction * exp(-k * x_total) # Ceff/Cinf from Eqn 9

    # Update water object with new TOC and DOC values
    bdoc_rem <- water@bdoc * (1 - bdoc0)
    water@doc <- water@doc - bdoc_rem
    water@toc <- water@toc - bdoc_rem
    water@bdoc <- water@bdoc * bdoc0
  } else {
    cli::cli_abort("Invalid combination of arguments. Please check your inputs.")
  }

  return(water)
  }


#' @rdname biofilter_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column indicating the EBCT or whether the water is ozonated.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated water class object. Default is "biofiltered".
#' @param pluck_cols Extract water slots modified by the function (doc, toc, bdoc) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   biofilter_toc_df(input_water = "defined", ebct = c(10, 15), ozonated = FALSE)
#'
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(
#'     BiofEBCT = c(10, 10, 10, 15, 15, 15, 20, 20, 20, 25, 25, 25),
#'     ozonated = c(rep(TRUE, 6), rep(FALSE, 6))
#'   ) %>%
#'   biofilter_toc_df(input_water = "defined", ebct = BiofEBCT)
#'
#' @export
#'
#' @returns `biofilter_toc_df` returns a data frame containing a water class column with updated DOC, TOC, and BDOC
#' concentrations. Optionally, it also adds columns for each of those slots individually.

biofilter_toc_df <- function(
  df,
  input_water = "defined",
  output_water = "biofiltered",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  ebct = "use_col",
  ozonated = "use_col",
  ozone_dose = "use_col",
  reuse = "use_col",
  media_type = "use_col",
  atp = "use_col"
) {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  ozonated <- tryCatch(ozonated, error = function(e) enquo(ozonated))
  ozone_dose <- tryCatch(ozone_dose, error = function(e) enquo(ozone_dose))
  reuse <- tryCatch(reuse, error = function(e) enquo(reuse))
  media_type <- tryCatch(media_type, error = function(e) enquo(media_type))
  atp <- tryCatch(atp, error = function(e) enquo(atp))

  arguments <- construct_helper(df, list("ebct" = ebct, "ozonated" = ozonated, "ozone_dose" = ozone_dose, "reuse" = reuse, "media_type" = media_type, "atp" = atp))
  final_names <- arguments$final_names
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(df, final_names, list(ozonated = TRUE, ozone_dose = 0, reuse = FALSE, media_type = "anthracite", atp = 0))
  df <- defaults_added$data

  warning_counts <- list()
  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    withCallingHandlers(
      biofilter_toc(
        water = df[[input_water]][[i]],
        ebct = df[[final_names$ebct]][i],
        ozonated = df[[final_names$ozonated]][i],
        ozone_dose = df[[final_names$ozone_dose]][i],
        reuse = df[[final_names$reuse]][i],
        media_type = df[[final_names$media_type]][i],
        atp = df[[final_names$atp]][i]
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) +1L
        invokeRestart("muffleWarning")
      }
    )
  })

  for(msg in names(warning_counts)){
    tidywater_warn("{msg} ({warning_counts[[msg]]} row{?s} affected.)")
  }

  output <- df[, !names(df) %in% defaults_added$defaults_used]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("toc", "doc", "bdoc"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  return(output)
}
