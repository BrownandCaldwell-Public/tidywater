#' GAC model for TOC removal
#' 
#' @title Calculate TOC Concentration in GAC system
#'
#' @description Returns a data frame with a breakthrough curve based on the TOC concentration after passing through GAC treatment, according to the model developed in 
#' "Modeling TOC Breakthrough in Granular Activated Carbon Adsorbers" by Zachman and Summers (2010), or the BC WTP Model v. 2.0 Manual.
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
#' @source BC WTP Model v. 2.0 Manual (2001)
#' 
#' @param water Source water object of class "water" created by [define_water]
#' @param ebct Empty bed contact time (minutes). Model results are valid for 10 or 20 minutes.
#' @param model Specifies which GAC TOC removal model to apply. Options are Zachman and WTP.
#' @param media_size Size of GAC filter mesh. Model includes 12x40 and 8x30 mesh sizes.
#'
#' @examples
#' water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5) %>%
#'   gacrun_toc(media_size = "8x30", ebct = 20, model = "Zachman")
#'
#' @export
#'
#' @returns `gacrun_toc` returns a data frame with bed volumes and breakthrough TOC values.
#'

gacrun_toc <- function(water, ebct = 10, model, media_size = "12x40") {
  validate_water(water, c("ph", "toc"))
  
  # check that media_sizeand ebct are inputted correctly
  if (media_size != "12x40" && media_size != "8x30") {
    stop("GAC media size must be either 12x40 or 8x30.")
  }
  
  if (ebct != 10 && ebct != 20) {
    stop("Models only apply for GAC reactors with ebct of 10 or 20 minutes.")
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
    x_norm <- x_norm / 100
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
    # bv <- 1440 * RT / ebct_adj
    bv <- seq(2000, 20000, 100)
    
    toc_eff <- A0 + Af/(1 + B * exp(-D * bv))
    x_norm <- toc_eff / water@toc # ranges 10 to 72.5%
    
  } else {
    stop("Please choose either Zachman or WTP as the model.")
  }
  
  breakthrough <- data.frame(bv = bv, x_norm = x_norm)
  return(breakthrough)
  
}