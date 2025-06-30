#' GAC model for TOC removal
#' 
#' @title Calculate TOC Concentration in PAC system
#'
#' @description Calculates TOC concentration after passing through GAC treatment according to the model developed in 
#' "Modeling TOC Breakthrough in Granular Activated Carbon Adsorbers" by Zachman and Summers (2010).
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
#' @param RT Blended run time of the GAC filter in days. Common run times range from 0.5 to 3 days.
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

gac_toc <- function(water, ebct = 10, model, media_size = "12x40", RT = 2, 
                    option = "fin_water", bed_vol, target_doc) {
  validate_water(water, c("ph", "toc"))
  
  # check that media_size, ebct, and option are inputted correctly
  if (media_size != "12x40" && media_size != "8x30") {
    stop("GAC media size must be either 12x40 or 8x30.")
  }
  
  if (ebct != 10 && ebct != 20) {
    stop("Model only applies for GAC reactors with ebct of 10 or 20 minutes.")
  }
  
  if (!all(option %in% c("fin_water", "plot", "bvs"))) {
    stop("Please choose from the three output options: fin_water, plot, and bvs.")
  }
  
  if (model == "Zachman") {
    x_norm <- seq(20, 70, 0.5) # x_norm represents the normalized effluent TOC concentration
    ### Implementation with the Zachman and Summers model
    # Equations for A and BV according to Zachman and Summers 2010
    if (media_size == "12x40" && ebct == 10) {
      A = 196*x_norm^2-5589*x_norm+252922
    } else if (media_size == "12x40" && ebct == 20) {
      A = 164*x_norm^2-1938*x_norm+245064
    } else if (media_size == "8x30" && ebct == 10) {
      A = 178*x_norm^2-6208*x_norm+238321
    } else {
      A = 202*x_norm^2-5995*x_norm+261914
    }
    
    bv <- A * water@toc^-1 * water@ph^-1.5 
  } else if (model == "WTP") {
    ### Implementation from the WTP Model v. 2.0 Manual
    #inputs: ph_inf, toc_inf, RT, ebct (10 or 20)
    # ebct = 10: 1.51 < toc_inf < 11.5 ; 6.07 < ph_inf < 9.95
    # ebct = 20: 1.51 < toc_inf < 11.5 ; 6.14 < ph_inf < 9.95 
    ph_base <- 7.93 # mean pH for which the model was developed
    ebct_adj <- ebct * (1 - 0.044 * (ph_base - water@ph))
    
    A0 <- water@toc * ((-1.148 * 10^-3 * ebct_adj) + 1.208 * 10^-1) - 2.710 * 10^-6 * ebct_adj + 1.097 * 10^-5
    Af <- water@toc * ((3.244 * 10^-3 * ebct_adj) + 5.383 * 10^-1) + 1.033 * 10^-5 * ebct_adj + 1.759 * 10^-5
    D <- water@toc * ((-1.079 * 10^-5 * ebct_adj) + 4.457 * 10^-4) + 1.861 * 10^-5 * ebct_adj - 2.809 * 10^-4
    B = 100
    bv <- 1440 * RT / ebct_adj # is this bed volume?
    
    toc_eff <- A0 + Af + Af * (log(((1+B*exp(-D*bv))/(1+B)))/(D*bv))
    x_norm <- toc_eff / water@toc # ranges 10 to 72.5%
    
  } else {
    stop("Please choose either Zachman or WTP as the model.")
  }
  
  output <- list()
  if ("plot" %in% option) {
    # plot the breakthrough curve
    breakthrough <- data.frame(bv = bv, x_norm = x_norm/100)
    plot <- ggplot(data = breakthrough, aes(bv, x_norm)) +
      geom_point() +
      labs(
        x = "Throughput, Bed Volumes (BV)",
        y = "Normalized TOC Concentration (TOCe/TOC0)"
      ) +
      theme_bc()
    # maybe also add annotation that displays the equations used?
    print(plot)
  }
  
  if ("fin_water" %in% option) {
    if (missing(bed_vol)) {
      stop("Bed volume is a required argument to predict final water quality.")
    }
    
    bv_index <- which.min(abs(bv - bed_vol)) # retrieves index of bv that's closest to bv arg
    output_water <- water
    output_water@toc <- x_norm[bv_index] * water@toc # calculate toc from predicted breakthrough curve
    
    # from WTP manual model:
    # if only coagulation before gac, 0.1 < x_norm*toc_inf < 14.7
    output_water@uv254 <- 0.0195 * output_water@toc - 0.0077
    # if coagulation, ozonation, and biotreatment before gac, 0.1 < x_norm*toc_inf < 6.1
    output_water@uv254 <- 0.0014 * output_water@toc - 0.00141
    
    output <- append(output, output_water)
  }
  
  if ("bvs" %in% option) {
    if (missing(target_doc)) {
      stop("Target DOC is a required argument to predict bed volumes.")
    }
    
    x_index <- sapply(target_doc/0.95, function(x) which.min(abs(x_norm-x))) # should work with input of multiple target DOCs
    output_bv <- bv[x_index]
    output <- append(output, output_bv)
  }
  
  return(output)
  
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
