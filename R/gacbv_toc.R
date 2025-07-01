#' GAC model for TOC removal
#' 
#' @title Calculate TOC Concentration in GAC system
#'
#' @description Calculates TOC concentration after passing through GAC treatment according to the model developed in 
#' "Modeling TOC Breakthrough in Granular Activated Carbon Adsorbers" by Zachman and Summers (2010), or the WTP model 
#' For a single water use `gac_toc`; for a dataframe use `gac_toc_chain`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#' For most arguments in the `_chain` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' Water must contain DOC or TOC value.
#'
#' @details The function will calculate TOC concentration by GAC adsorption in drinking water treatment.
#' UV254 concentrations are predicted based on a linear relationship with DOC.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source Zachman and Summers (2010)
#' 
#' @param water Source water object of class "water" created by [define_water]
#' @param ebct Empty bed contact time (minutes). Model results are valid for 10 or 20 minutes.
#' @param model Specifies which GAC TOC removal model to apply. Options are Zachman and WTP.
#' @param media_size Size of GAC filter mesh. Model includes 12x40 and 8x30 mesh sizes.
#' @param option Specifies the type of output: plot produces the breakthrough curve, fin_water outputs a water with updated toc/doc, and
#' bvs will calculate bed volume required for target final doc. Argument can take multiple inputs.
#' @param bed_vol Optional input to calculate breakthrough for a given bed volume
#' @param target_doc Optional input to set a target DOC concentration and calculate necessary bed volume
#'
#' @examples
#' water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5) %>%
#'   gac_toc(media_size = "8x30", ebct = 20, model = "Zachman", option = "plot")
#'
#' @export
#'
#' @returns `gac_toc` returns a water class object with updated DOC, TOC, and UV254 slots.
#'

gac_toc <- function(water, ebct = 10, model, media_size = "12x40", target_doc) {
  validate_water(water, c("ph", "toc"))
  breakthrough_df <- gacrun_toc(water, ebct, model, media_size)
  
  if (missing(target_doc)) {
    stop("Target DOC is a required argument to predict bed volumes.")
  }
    
  x_index <- sapply(target_doc/0.95, function(x) which.min(abs(breakthrough_df$x_norm-x))) # should work with input of multiple target DOCs
  output_bv <- breakthrough_df$bv[x_index]
  
  return(output_bv)
}

# Currently gac_toc_chain function is copied from pac_toc_chain -- may not be implemented
#' @rdname gac_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain]. The df may include columns named for the media_size, ebct, and bed volume.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "gac_water".
#'
#' @examples
#'
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   mutate()
#'
#' \donttest{
#' # Initialize parallel processing
#' library(furrr)
#'# plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain("raw") %>%
#'   gac_toc_chain(input_water = "raw")
#'
#' # Optional: explicitly close multisession processing
#'# plan(sequential)
#' }
#' @import dplyr
#'
#' @export
#'
#' @returns `gac_toc_chain` returns a data frame containing a water class column with updated DOC, TOC, and UV254 slots

# gac_toc_chain <- function(df, input_water = "defined_water", output_water = "gac_water",
#                           media_size = "use_col", ebct = "use_col", bed_vol = "use_col") {
#   validate_water_helpers(df, input_water)
#   # This allows for the function to process unquoted column names without erroring
#   media_size <- tryCatch(media_size, error = function(e) enquo(media_size))
#   ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
#   bed_vol <- tryCatch(bed_vol, error = function(e) enquo(bed_vol))
# 
#   # This returns a dataframe of the input arguments and the correct column names for the others
#   arguments <- construct_helper(df, all_args = list("media_size" = media_size, "ebct" = ebct, "bed_vol" = bed_vol))
# 
#   # Only join inputs if they aren't in existing dataframe
#   if (length(arguments$new_cols) > 0) {
#     df <- df %>%
#       cross_join(as.data.frame(arguments$new_cols))
#   }
#   output <- df %>%
#     mutate(!!output_water := furrr::future_pmap(
#       list(
#         water = !!as.name(input_water),
#         media_size = !!as.name(arguments$final_names$media_size),
#         ebct = !!as.name(arguments$final_names$ebct),
#         bed_vol = !!as.name(arguments$final_names$bed_vol)
#       ),
#       gac_toc(option = "fin_water")
#     ))
# }
