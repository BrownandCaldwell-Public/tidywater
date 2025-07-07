#' GAC model for TOC removal
#' 
#' @title Calculate bed volume to achieve target DOC
#'
#' @description Calculates GAC filter bed volumes to achieve target effluent DOC according to the model developed in 
#' "Modeling TOC Breakthrough in Granular Activated Carbon Adsorbers" by Zachman and Summers (2010), or the BC WTP Model v. 2.0 Manual.
#' For a single water use `gacbv_toc`; for a dataframe use `gacbv_toc_once`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#' For most arguments in the `_once` helper
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' Water must contain DOC or TOC value.
#'
#' @details The function will calculate bed volume required to achieve given target DOC values.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source Zachman and Summers (2010)
#' @source BC WTP Model v. 2.0 Manual (2001)
#' 
#' @param water Source water object of class "water" created by [define_water]
#' @param ebct Empty bed contact time (minutes). Model results are valid for 10 or 20 minutes.
#' @param model Specifies which GAC TOC removal model to apply. Options are Zachman and WTP.
#' @param media_size Size of GAC filter mesh. Model includes 12x40 and 8x30 mesh sizes.
#' @param target_doc Optional input to set a target DOC concentration and calculate necessary bed volume
#'
#' @examples
#' water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5)
#' bed_volume <- gacbv_toc(water, media_size = "8x30", ebct = 20, model = "Zachman", target_doc = 0.8)
#'
#' @export
#'
#' @returns `gacbv_toc` returns a data frame of bed volumes that achieve the target DOC.
#'

gacbv_toc <- function(water, ebct = 10, model, media_size = "12x40", target_doc) {
  validate_water(water, c("ph", "toc"))
  breakthrough_df <- gacrun_toc(water, ebct, model, media_size)
  
  if (missing(target_doc)) {
    stop("Target DOC is a required argument to predict bed volumes.")
  }
    
  x_index <- sapply(target_doc/0.95, function(x) which.min(abs(breakthrough_df$x_norm-x))) # should work with input of multiple target DOCs
  output_bv <- breakthrough_df$bv[x_index]
  
  return(output_bv)
}

#' @rdname gacbv_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_chain] The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(model = "WTP",
#'          media_size = "8x30",
#'          ebct = 10, 
#'          target_doc = rep(c(0.5, 0.8, 1), 4)) %>%
#'   gacbv_toc_once()
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns `gacbv_toc_once` returns a data frame with columns for bed volumes.
#'

gacbv_toc_once <- function(df, input_water = "defined_water", model = "use_col",
                         media_size = "use_col", ebct = "use_col", target_doc = "use_col") {
  validate_water_helpers(df, input_water)
  bed_volume <- NULL # Quiet RCMD check global variable note
  
  # This allows for the function to process unquoted column names without erroring
  model <- tryCatch(model, error = function(e) enquo(model))
  media_size <- tryCatch(media_size, error = function(e) enquo(media_size))
  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  target_doc <- tryCatch(target_doc, error = function(e) enquo(target_doc))
  
  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list("model" = model, "media_size" = media_size, "ebct" = ebct, "target_doc" = target_doc))
  
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(bed_volume := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        model = !!as.name(arguments$final_names$model),
        media_size = !!as.name(arguments$final_names$media_size),
        ebct = !!as.name(arguments$final_names$ebct),
        target_doc = !!as.name(arguments$final_names$target_doc)
      ),
      gacbv_toc
    ))
  
  return(output)
}
