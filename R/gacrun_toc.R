#' GAC model for TOC removal
#'
#' @title Calculate TOC Concentration in GAC system
#'
#' @description Returns a data frame with a breakthrough curve based on the TOC concentration after passing through GAC treatment, according to the model developed in
#' "Modeling TOC Breakthrough in Granular Activated Carbon Adsorbers" by Zachman and Summers (2010), or the USEPA WTP Model v. 2.0 Manual (2001).
#'
#' Water must contain DOC or TOC value.
#'
#' @details The function will calculate bed volumes and normalized TOC breakthrough (TOCeff/TOCinf) given model type.
#' Both models were developed using data sets from bench-scale GAC treatment studies using bituminous GAC and EBCTs of either 10 or 20 minutes.
#' The specific mesh sizes used to develop the Zachman and Summers model were 12x40 or 8x30.
#' The models were also developed using influent pH and TOC between specific ranges. Refer to the papers included in the references for more details.
#'
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#' @source Zachman and Summers (2010)
#' @source USEPA (2001)
#'
#' @param water Source water object of class "water" created by [define_water]
#' @param ebct Empty bed contact time (minutes). Model results are valid for 10 or 20 minutes. Default is 10 minutes.
#' @param model Specifies which GAC TOC removal model to apply. Options are Zachman and WTP.
#' @param media_size Size of GAC filter mesh. If model is Zachman, can choose between 12x40 and 8x30 mesh sizes, otherwise leave as default. Defaults to 12x40.
#'
#' @examples
#' water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5) %>%
#'   gacrun_toc(media_size = "8x30", ebct = 20, model = "Zachman")
#'
#' @export
#'
#' @returns `gacrun_toc` returns a data frame with bed volumes and breakthrough TOC values.
#'

gacrun_toc <- function(water, ebct = 10, model = "Zachman", media_size = "12x40") {
  validate_water(water, c("ph", "doc"))

  if (model == "Zachman") {
    # check that media_size and ebct are inputted correctly
    if (media_size != "12x40" && media_size != "8x30") {
      stop("GAC media size must be either 12x40 or 8x30.")
    }
    if (ebct != 10 && ebct != 20) {
      stop("Zachman model only apply for GAC reactors with ebct of 10 or 20 minutes.")
    }

    x_norm <- seq(20, 70, 0.5) # x_norm represents the normalized effluent TOC concentration
    ### Implementation with the Zachman and Summers model
    # Equations for A and BV according to Zachman and Summers 2010
    if (media_size == "12x40" && ebct == 10) {
      A <- 196 * x_norm^2 - 5589 * x_norm + 252922
    } else if (media_size == "12x40" && ebct == 20) {
      A <- 164 * x_norm^2 - 1938 * x_norm + 245064
    } else if (media_size == "8x30" && ebct == 10) {
      A <- 178 * x_norm^2 - 6208 * x_norm + 238321
    } else {
      A <- 202 * x_norm^2 - 5995 * x_norm + 261914
    }

    bv <- A * water@toc^-1 * water@ph^-1.5
    x_norm <- x_norm / 100
  } else if (model == "WTP") {
    ### Implementation from the WTP Model v. 2.0 Manual
    # inputs: ph_inf, toc_inf, RT, ebct (10 or 20)
    # ebct = 10: 1.51 < toc_inf < 11.5 ; 6.07 < ph_inf < 9.95
    # ebct = 20: 1.51 < toc_inf < 11.5 ; 6.14 < ph_inf < 9.95
    ph_base <- 7.93 # mean pH for which the model was developed
    ebct_adj <- ebct * (1 - 0.044 * (ph_base - water@ph))

    A0 <- water@toc * ((-1.148 * 10^-3 * ebct_adj) + 1.208 * 10^-1) - 2.710 * 10^-6 * ebct_adj + 1.097 * 10^-5
    Af <- water@toc * ((3.244 * 10^-3 * ebct_adj) + 5.383 * 10^-1) + 1.033 * 10^-5 * ebct_adj + 1.759 * 10^-5
    D <- water@toc * ((-1.079 * 10^-5 * ebct_adj) + 4.457 * 10^-4) + 1.861 * 10^-5 * ebct_adj - 2.809 * 10^-4
    B <- 100
    # bv <- 1440 * RT / ebct_adj
    if (water@toc <= 1.5) {
      bv_max <- 40000
    } else {
      bv_max <- 20000
    }
    bv <- seq(2000, bv_max, 100)

    toc_eff <- A0 + Af / (1 + B * exp(-D * bv))
    x_norm <- toc_eff / water@toc # ranges 10 to 72.5%
  } else {
    stop("Please choose either Zachman or WTP as the model.")
  }

  breakthrough <- data.frame(bv = bv, x_norm = x_norm)
  return(breakthrough)
}

#' @rdname gacrun_toc
#' @param df a data frame containing a water class column, which has already been computed using
#' [define_water_df]. The df may include a column named for the coagulant being dosed,
#' and a column named for the set of coefficients to use.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined".
#' @param water_prefix Append the input_water name to the start of the output columns. Default is TRUE.
#'
#' @examples
#' \donttest{
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   gacrun_toc_df()
#' }
#'
#' @export
#'
#' @returns `gacrun_toc_df` returns a data frame containing columns of the breakthrough curve (breakthrough and bed volume).
#'
gacrun_toc_df <- function(df, input_water = "defined", water_prefix = TRUE,
                            ebct = "use_col", model = "use_col", media_size = "use_col") {
  # This allows for the function to process unquoted column names without erroring
  ebct <- tryCatch(ebct, error = function(e) enquo(ebct))
  model <- tryCatch(model, error = function(e) enquo(model))
  media_size <- tryCatch(media_size, error = function(e) enquo(media_size))
  
  validate_water_helpers(df, input_water)
  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(df, all_args = list(
    "ebct" = ebct, "model" = model,
    "media_size" = media_size
  ))
  final_names <- arguments$final_names
  
  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }
  # Add columns with default arguments
  defaults_added <- handle_defaults(
    df, final_names,
    list(ebct = 10, model = "Zachman", media_size = "12x40")
  )
  df <- defaults_added$data %>%
    transform(ID = seq(1, nrow(df), 1))
  
  bv_df <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    result <- gacrun_toc(
      water = df[[input_water]][[i]],
      ebct = df[[final_names$ebct]][i],
      model = df[[final_names$model]][i],
      media_size = df[[final_names$media_size]][i]
    )
    result$ID <- df$ID[i]
    return(result)
  }))
  
  # Rename columns in bv_df except for 'ID'
  if (water_prefix) {
    names(bv_df)[names(bv_df) != "ID"] <- paste0(input_water, "_", names(bv_df)[names(bv_df) != "ID"])
  }

  output <- merge(bv_df, df, by = "ID", all.x = TRUE)
  output <- output[order(output$ID),]
  output <- output[, !names(output) == "ID"]
  return(output)
}
