# Bromate formation
# This function predicts bromate formation from ozonation

#' @title Calculate bromate formation
#'
#' @description Calculates bromate (BrO3-) formation based on selected model. Required arguments include an object of class "water"
#' created by \code{\link{define_water}} ozone dose, reaction time, and desired model.
#' The function also requires additional water quality parameters defined in \code{define_water}
#' including bromide, DOC or UV254 (depending on the model), pH, alkalinity (depending on the model).
#'
#' @details The function will calculate haloacetic acids (HAA) as HAA5, and total trihalomethanes (TTHM).
#' The function returns a new object of class "water" with predicted DBP concentrations.
#' Use \code{summarise_wq} to quickly tabulate the results.
#'
#' @source Ozekin (1994), Sohn et al (2004), Song et al (1996), Galey et al (1997), Siddiqui et al (1994)
#' @source See references list at: \url{https://github.com/BrownandCaldwell/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param dose Applied ozone dose (mg/L as O3). Results typically valid for 1-10 mg/L, but varies depending on model.
#' @param time Reaction time (minutes). Results typically valid for 1-120 minuts, but varies depending on model.
#' @param model Model to apply. One of c("Ozekin", "Sohn", "Song", "Galey", "Siddiqui")
#' @examples
#' example_dbp <- suppressWarnings(define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   ozonate_bromate(dose = 1.5, time = 5, model = "Ozekin")
#' example_dbp <- suppressWarnings(define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50)) %>%
#'   ozonate_bromate(dose = 3, time = 15, model = "Sohn")
#'
#' @export
#'
ozonate_bromate <- function(water, dose, time, model) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }
  if (missing(dose)) {
    stop("Ozone dose must be specified in mg/L.")
  }
  if (missing(time) & model != "Siddiqui") {
    stop("Reaction time in minutes required for all models except 'Siddiqui'")
  }
  if (!model %in% c("Ozekin", "Sohn", "Song", "Galey", "Siddiqui")) {
    stop("model must be one of 'Ozekin', 'Sohn', 'Song', 'Galey', 'Siddiqui'.")
  }

  # Bromide and pH required for all models
  if (is.na(water@br) | is.na(water@ph)) {
    stop("Bromide and pH required for all models. Include both when defining water.")
  }
  # Other parameters depend on model
  if (is.na(water@alk) & model %in% c("Sohn", "Song")) {
    stop("Alkalinity required for selected model. Use one of 'Ozekin', 'Galey', 'Siddiqui' instead or add alkalinity when defining water.")
  }
  if (is.na(water@doc) & model != "Sohn") {
    stop("DOC required for selected model. Use 'Sohn' to use UV254 instead or add DOC when defining water.")
  }
  if (is.na(water@uv254) & model == "Sohn") {
    stop("UV254 required for Sohn model. Use a different model or add UV254 when defining water.")
  }

  # Bromide should be in ug/L for these models
  br <- convert_units(water@br, "br", "M", "ug/L")

  doc <- ifelse(is.na(water@doc), 0, water@doc)
  uv254 <- ifelse(is.na(water@uv254), 0, water@uv254)
  ph <- water@ph
  alk <- ifelse(is.na(water@alk), 0, water@alk)
  nh4 <- 5 # TODO, add to water ***************************************************************************************
  temp <- water@temp
  # TODO add warnings for parameters outside model ranges *****************************************************************
  mod <- model
  # All models must match this form.
  solve_bro3 <- bromatecoeffs %>%
    filter(model == mod & ammonia == ifelse(nh4 == 0, F, T)) %>%
    mutate(bro3 = A * br^a * doc^b * uv254^c * ph^d * alk^e * dose^f * time^g * nh4^h * temp^i * I^(temp - 20))

  if (nrow(solve_bro3) == 0 & nh4 == 0) {
    stop("Selected model not applicable to waters with no ammonia. Select one of 'Ozekin', 'Sohn', 'Galey', 'Siddiqui',
         specify nh4 in define_water, or dose it with chemdose_ph.")
  } else if (nrow(solve_bro3) == 0 & nh4 > 0) {
    stop("Selected model not applicable to water with ammonia. Select one of 'Ozekin', 'Sohn', 'Song' or change nh4 to 0.")
  }

  water@bro3 <- solve_bro3$bro3

  return(water)
}
