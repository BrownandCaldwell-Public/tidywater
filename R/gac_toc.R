#' GAC model for TOC removal
#'
#' @title Calculate TOC Concentration in GAC system
#'
#' @description Calculates TOC concentration after passing through GAC treatment according to the model developed in
#' "Modeling TOC Breakthrough in Granular Activated Carbon Adsorbers" by Zachman and Summers (2010), or the logistics curve approach in EPA WTP Model v. 2.0 Manual (2001).
#' For a single water use `gac_toc`; for a dataframe use `gac_toc_df`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#' For most arguments in the `_df` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' Water must contain DOC or TOC value.
#'
#' @details The function will calculate TOC concentration by GAC adsorption in drinking water treatment.
#' UV254 concentrations are predicted based on a linear relationship with DOC from WTP Model Equation 5-93 and 5-94.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source Zachman and Summers (2010)
#' @source U.S. EPA (2001)
#'
#' @param water Source water object of class "water" created by [define_water]
#' @param ebct Empty bed contact time (minutes). Model results are valid for 10 or 20 minutes. Defaults to 10 minutes.
#' @param model Specifies which GAC TOC removal model to apply. Options are Zachman and WTP. Defaults to Zachman.
#' @param media_size Size of GAC filter mesh. Model includes 12x40 and 8x30 mesh sizes. Defaults to 12x40.
#' @param bed_vol Bed volume of GAC filter to predict effluent TOC for.
#' @param pretreat Specifies the level of pretreatment prior to GAC treatment. Defaults to "coag".
#' Other option is coagulant, ozonation, and biotreatment, called "o3biof".
#'
#' @examples
#' water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5) %>%
#'   gac_toc(media_size = "8x30", ebct = 20, model = "Zachman", bed_vol = 15000)
#'
#' @export
#'
#' @returns `gac_toc` returns a water class object with updated DOC, TOC, and UV254 slots.
#'

gac_toc <- function(water, ebct = 10, model = "Zachman", media_size = "12x40", bed_vol, pretreat = "coag") {
  validate_water(water, c("ph", "doc"))
  breakthrough_df <- gacrun_toc(water, ebct, model, media_size)

  if (missing(bed_vol)) {
    stop("Bed volume is a required argument to predict final water quality. Use `gacrun_toc` for complete breakthrough curve.")
  }

  bv_index <- which.min(abs(breakthrough_df$bv - bed_vol)) # retrieves index of bv that's closest to bv arg
  output_water <- water
  output_water@toc <- breakthrough_df$x_norm[bv_index] * water@toc # calculate toc from predicted breakthrough curve
  output_water@doc <- 0.95 * output_water@toc

  # UV254 removal from WTP manual model:
  if (pretreat == "coag") {
    # if only coagulation before gac, 0.1 < x_norm*toc_inf < 14.7
    output_water@uv254 <- 0.0195 * output_water@toc - 0.0077
  } else {
    # if coagulation, ozonation, and biotreatment before gac, 0.1 < x_norm*toc_inf < 6.1
    output_water@uv254 <- 0.0014 * output_water@toc - 0.00141
  }

  return(output_water)
}


#' @rdname gac_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include columns named for the media_size, ebct, and bed volume.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "gaced".
#' @param pluck_cols Extract primary water slots modified by the function into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @examples
#' \donttest{
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   dplyr::mutate(
#'     model = "WTP",
#'     media_size = "8x30",
#'     ebct = 10,
#'     bed_vol = rep(c(12000, 15000, 18000), 4)
#'   ) %>%
#'   gac_toc_df()
#'
#' example_df <- water_df %>%
#'   define_water_df("raw") %>%
#'   dplyr::mutate(
#'     model = "WTP",
#'     bed_vol = 15000
#'   ) %>%
#'   gac_toc_df(input_water = "raw")
#'
#' }
#' @export
#'
#' @returns `gac_toc_df` returns a data frame containing a water class column with updated DOC, TOC, and UV254 slots

gac_toc_df <- function(df, input_water = "defined", output_water = "gaced", model = "use_col", pluck_cols = FALSE, water_prefix = TRUE,
                          media_size = "use_col", ebct = "use_col", bed_vol = "use_col", pretreat = "use_col") {
  validate_water_helpers(df, input_water)
  # This allows for the function to process unquoted column names without erroring
  model <- tryCatch(model, error = function(e) enquo(model))
  media_size <- tryCatch(media_size, error = function(e) enquo(media_size))
  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  bed_vol <- tryCatch(bed_vol, error = function(e) enquo(bed_vol))
  pretreat <- tryCatch(pretreat, error = function(e) enquo(pretreat))

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(
    df, list(
      "model" = model, "media_size" = media_size, "ebct" = ebct,
      "bed_vol" = bed_vol, "pretreat" = pretreat
    )
  )
  final_names <- arguments$final_names

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df, final_names,
    list(model = "Zachman", media_size = "12x40", ebct = "10", pretreat = "coag")
  )
  df <- defaults_added$data

  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    gac_toc(
      water = df[[input_water]][[i]],
      model = df[[final_names$model]][i],
      media_size = df[[final_names$media_size]][i],
      ebct = as.numeric(df[[final_names$ebct]][i]),
      bed_vol = df[[final_names$bed_vol]][i],
      pretreat = df[[final_names$pretreat]][i]
    )
  })

  output <- df[, !names(df) %in% defaults_added$defaults_used]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("toc", "doc", "uv254"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  return(output)
}
