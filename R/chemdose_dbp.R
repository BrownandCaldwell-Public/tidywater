# DBP Modeling functions
# These functions predict total trihalomethane (TTHM) and haloacetic acid (HAA) formation

#' @title Calculate DBP formation
#'
#' @description Calculates disinfection byproduct (DBP) formation based on the U.S. EPA's
#' Water Treatment Plant Model (U.S. EPA, 2001). Required arguments include an object of class "water"
#' created by [define_water] chlorine dose, type, reaction time, and treatment applied (if any).
#' The function also requires additional water quality parameters defined in [define_water]
#' including bromide, TOC, UV254, temperature, and pH.
#' For a single water use `chemdose_dbp`; for a dataframe where you want to output a water for continued modeling use
#' `chemdose_dbp_chain`; for a dataframe where you want to output water parameters as columns use `chemdose_dbp_once`
#' (note subsequent tidywater modeling functions will only work if `_chain` is used because a `water` is required).
#' For most arguments, the `_chain` and `_once` helpers
#' "use_col" default looks for a column of the same name in the dataframe. The argument can be specified directly in the
#' function instead or an unquoted column name can be provided.
#'
#' @details The function will calculate haloacetic acids (HAA) as HAA5, and total trihalomethanes (TTHM).
#' Use `summarize_wq(water, params = c("dbps"))` to quickly tabulate the results.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @source TTHMs, raw: U.S. EPA (2001) equation 5-131
#' @source HAAs, raw: U.S. EPA (2001) equation 5-134
#' @source TTHMs, treated: U.S. EPA (2001) equation 5-139
#' @source HAAs, treated: U.S. EPA (2001) equation 5-142
#' @source See references list at: \url{https://github.com/BrownandCaldwell-Public/tidywater/wiki/References}
#'
#' @param water Source water object of class "water" created by \code{\link{define_water}}
#' @param cl2 Applied chlorine dose (mg/L as Cl2). Model results are valid for doses between 1.51 and 33.55 mg/L.
#' @param time Reaction time (hours). Model results are valid for reaction times between 2 and 168 hours.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default), "coag" for
#' water that has been coagulated or softened, and "gac" for water that has been treated by granular activated carbon (GAC).
#' GAC treatment has also been used for estimating formation after membrane treatment with good results.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @param location Location for DBP formation, either in the "plant" (default), or in the distributions system, "ds".
#' @examples
#' example_dbp <- define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
#'   chemdose_dbp(cl2 = 2, time = 8)
#' example_dbp <- define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
#'   chemdose_dbp(cl2 = 3, time = 168, treatment = "coag", location = "ds")
#'
#' @export
#'
#' @returns `chemdose_dbp` returns a single water class object with predicted DBP concentrations.
#'
chemdose_dbp <- function(water, cl2, time, treatment = "raw", cl_type = "chorine", location = "plant") {
  modeled_dbp <- ID <- group <- ID_ind <- percent <- NULL # Quiet RCMD check global variable note
  validate_water(water, c("ph", "temp", "br"))

  toc <- water@toc
  doc <- water@doc
  uv254 <- water@uv254
  temp <- water@temp
  ph <- water@ph
  # Bromide should be in ug/L for these models
  br <- convert_units(water@br, "br", "M", "ug/L")

  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (treatment == "raw") {
    validate_water(water, c("toc"))
  } else if (treatment == "coag" | treatment == "gac") {
    validate_water(water, c("doc", "uv254"))
  } else {
    stop("Treatment must be one of c('raw', 'coag', 'gac'.")
  }

  if (missing(cl2) | missing(time)) {
    stop("Missing value for cl2 or time. Please check the function inputs required to calculate DBP formation.")
  }

  # toc/doc warnings
  if (treatment == "raw" & (toc < 1.2 | toc > 10.6)) {
    warning("TOC is outside the model bounds of 1.2 <= TOC <= 10.6 mg/L.")
  }
  if (treatment == "coag" & (doc < 1.00 | doc > 7.77)) {
    warning("DOC is outside the model bounds of 1.00 <= doc <= 7.77 mg/L for coagulated water.")
  }
  if (treatment == "gac" & (doc < 0.14 | doc > 2.0)) {
    warning("DOC is outside the model bounds of 0.14 <= DOC <= 2.0 mg/L for GAC treated water.")
  }

  # uv254 warnings
  if (treatment == "coag" & (uv254 < 0.016 | uv254 > 0.215)) {
    warning("UV254 is outside the model bounds of 0.016 <= UV254 <= 0.215 cm-1 for coagulated water.")
  }
  if (treatment == "gac" & (uv254 < 0.001 | uv254 > 0.048)) {
    warning("UV254 is outside the model bounds of 0.001 <= UV254 <= 0.048 cm-1 for GAC treated water.")
  }

  # cl2 warnings
  if (treatment == "raw" & (cl2 < 1.51 | cl2 > 33.55)) {
    warning("Chlorine is outside the model bounds of 1.51 <= Cl2 <= 33.55 mg/L for raw water.")
  }
  if (treatment == "coag" & (cl2 < 1.11 | cl2 > 24.75)) {
    warning("Chlorine is outside the model bounds of 1.11 <= Cl2 <= 24.75 mg/L for coagulated water.")
  }
  if (treatment == "gac" & (cl2 < 0.5 | cl2 > 3.0)) {
    warning("Chlorine is outside the model bounds of 0.5 <= Cl2 <= 3.0 mg/L for GAC treated water.")
  }

  # br warnings
  if (treatment == "raw" & (br < 7 | br > 600)) {
    warning("Bromide is outside the model bounds of 7 <= Br <= 600 ug/L for raw water.")
  }
  if (treatment == "coag" & (br < 23 | br > 308)) {
    warning("Bromide is outside the model bounds of 23 <= Br <= 308 ug/L for coagulated water.")
  }
  if (treatment == "gac" & (br < 10 | br > 570)) {
    warning("Bromide is outside the model bounds of 10 <= Br <= 570 ug/L for GAC treated water.")
  }

  # temp warnings
  if (treatment == "raw" & (temp < 15 | temp > 25)) {
    warning("Temperature is outside the model bounds of 15 <= temp <= 25 Celsius for raw water.")
  }
  if (treatment == "coag" & temp != 20) {
    warning("Temperature is outside the model bounds of temp=20 Celsius for coagulated water.")
  }
  if (treatment == "gac" & (temp < 3 | temp > 33)) {
    warning("Temperature is outside the model bounds of 3 <= temp <= 33 Celsius for GAC treated water.")
  }

  # ph warnings
  if (treatment == "raw" & (ph < 6.5 | ph > 8.5)) {
    warning("pH is outside the model bounds of 6.5 <= pH <= 8.5 for raw water.")
  }
  if (treatment == "coag" & ph != 7.5) {
    warning("pH is outside the model bounds of pH = 7.5 for coagulated water")
  }
  if (treatment == "gac" & (ph < 6.7 | ph > 10)) {
    warning("pH is outside the model bounds of 6.7 <= pH <= 10 for GAC treated water.")
  }

  # time warning
  if (time < 2 | time > 168) {
    warning("Reaction time is outside the model bounds of 2 <= time <= 168 hours.")
  }

  # breakpoint warning
  if (water@tot_nh3 > 0) {
    warning("Background ammonia present, chloramines may form.\nUse chemdose_chloramine for breakpoint caclulations.")
  }

  # estimate formation based on level of treatment - results in ug/L
  if (treatment == "raw") {
    predicted_dbp <- subset(tidywater::dbpcoeffs, treatment == "raw")
    # modeled_dbp = A * toc^a * cl2^b * br^c * temp^d * ph^e * time^f
    predicted_dbp$modeled_dbp <- predicted_dbp$A * toc^predicted_dbp$a * cl2^predicted_dbp$b * br^predicted_dbp$c *
      temp^predicted_dbp$d * ph^predicted_dbp$e * time^predicted_dbp$f
  } else {
    treat <- treatment
    predicted_dbp <- subset(tidywater::dbpcoeffs, treatment == treat)
    # modeled_dbp = A * (doc * uv254)^a * cl2^b * br^c * d^(ph - ph_const) * e^(temp - 20) * time^f
    predicted_dbp$modeled_dbp <- predicted_dbp$A * (doc * uv254)^predicted_dbp$a * cl2^predicted_dbp$b *
      br^predicted_dbp$c * predicted_dbp$d^(ph - predicted_dbp$ph_const) * predicted_dbp$e^(temp - 20) * time^predicted_dbp$f
  }

  # apply dbp correction factors based on selected location for "raw" and "coag" treatment (corrections do not apply to "gac" treatment), U.S. EPA (2001) Table 5-7
  if (location == "plant" & treatment != "gac") {
    corrected_dbp_1 <- predicted_dbp %>%
      dplyr::left_join(tidywater::dbp_correction, by = "ID") %>%
      dplyr::mutate(modeled_dbp = modeled_dbp / .data$plant) %>%
      dplyr::select(ID, group, modeled_dbp)
  } else if (location == "ds" & treatment != "gac") {
    corrected_dbp_1 <- predicted_dbp %>%
      dplyr::left_join(tidywater::dbp_correction, by = "ID") %>%
      dplyr::mutate(modeled_dbp = modeled_dbp / .data$ds) %>%
      dplyr::select(ID, group, modeled_dbp)
  } else {
    corrected_dbp_1 <- predicted_dbp %>%
      dplyr::select(ID, group, modeled_dbp)
  }

  # only model tthm and haa5, problems with haa6 and haa9 model outputs being <haa5 with low Br or Cl2
  bulk_dbp <- subset(corrected_dbp_1, corrected_dbp_1$ID %in% c("tthm", "haa5"))
  # proportional corrections following U.S. EPA (2001), section 5.7.3
  individual_dbp <- corrected_dbp_1 %>%
    dplyr::filter(
      !(ID %in% c("tthm", "haa5")),
      !(group %in% c("haa6", "haa9"))
    ) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(
      sum_group = sum(modeled_dbp),
      proportion_group = modeled_dbp / .data$sum_group
    ) %>%
    dplyr::left_join(bulk_dbp, by = "group", suffix = c("_ind", "_bulk")) %>%
    dplyr::mutate(modeled_dbp = .data$proportion_group * .data$modeled_dbp_bulk)

  corrected_dbp_2 <- individual_dbp %>%
    dplyr::select(ID_ind, group, modeled_dbp) %>%
    dplyr::rename(ID = ID_ind) %>%
    rbind(bulk_dbp)

  # estimate reduced formation if using chloramines, U.S. EPA (2001) Table 5-10
  if (cl_type == "chloramine") {
    corrected_dbp_2 <- corrected_dbp_2 %>%
      dplyr::left_join(tidywater::chloramine_conv, by = "ID") %>%
      dplyr::mutate(modeled_dbp = modeled_dbp * percent)
  }
  corrected_dbp_2 <- as.data.frame(corrected_dbp_2)
  rownames(corrected_dbp_2) <- corrected_dbp_2$ID

  water@tthm <- corrected_dbp_2["tthm", ]$modeled_dbp
  water@chcl3 <- corrected_dbp_2["chcl3", ]$modeled_dbp
  water@chcl2br <- corrected_dbp_2["chcl2br", ]$modeled_dbp
  water@chbr2cl <- corrected_dbp_2["chbr2cl", ]$modeled_dbp
  water@chbr3 <- corrected_dbp_2["chbr3", ]$modeled_dbp

  water@haa5 <- corrected_dbp_2["haa5", ]$modeled_dbp
  water@mcaa <- corrected_dbp_2["mcaa", ]$modeled_dbp
  water@dcaa <- corrected_dbp_2["dcaa", ]$modeled_dbp
  water@tcaa <- corrected_dbp_2["tcaa", ]$modeled_dbp
  water@mbaa <- corrected_dbp_2["mbaa", ]$modeled_dbp
  water@dbaa <- corrected_dbp_2["dbaa", ]$modeled_dbp

  return(water)
}

#' @rdname chemdose_dbp
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_once}}. The df may include a column named for the applied chlorine dose (cl2),
#' and a column for time.
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
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_dbp_once(input_water = "balanced_water", cl2 = 4, time = 8)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     cl2 = seq(2, 24, 2),
#'     DBPTime = 30
#'   ) %>%
#'   chemdose_dbp_once(input_water = "balanced_water", time = DBPTime)
#'
#' example_df <- water_df %>%
#'   mutate(br = 80) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(time = 8) %>%
#'   chemdose_dbp_once(
#'     input_water = "balanced_water", cl = 6, treatment = "coag",
#'     location = "ds", cl_type = "chloramine"
#'   )
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export
#'
#' @returns `chemdose_dbp_once` returns a data frame with predicted DBP concentrations as columns.

chemdose_dbp_once <- function(df, input_water = "defined_water", cl2 = "use_col", time = "use_col",
                              treatment = "use_col", cl_type = "use_col", location = "use_col") {
  temp_dbp <- dbps <- NULL # Quiet RCMD check global variable note

  # This allows for the function to process unquoted column names without erroring
  cl2 <- tryCatch(cl2, error = function(e) enquo(cl2))
  time <- tryCatch(time, error = function(e) enquo(time))
  treatment <- tryCatch(treatment, error = function(e) enquo(treatment))
  cl_type <- tryCatch(cl_type, error = function(e) enquo(cl_type))
  location <- tryCatch(location, error = function(e) enquo(location))

  output <- df %>%
    chemdose_dbp_chain(
      input_water = input_water, output_water = "temp_dbp",
      cl2, time, treatment, cl_type, location
    ) %>%
    mutate(dbps = furrr::future_map(temp_dbp, convert_water)) %>%
    unnest(dbps) %>%
    select(-temp_dbp)
}

#' @rdname chemdose_dbp
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "disinfected_water".
#'
#' @examples
#'
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_dbp_chain(input_water = "balanced_water", cl2 = 4, time = 8)
#'
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     cl2 = seq(2, 24, 2),
#'     time = 30
#'   ) %>%
#'   chemdose_dbp_chain(input_water = "balanced_water")
#'
#' example_df <- water_df %>%
#'   mutate(br = 80) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(time = 8) %>%
#'   chemdose_dbp_chain(
#'     input_water = "balanced_water", cl = 6, treatment = "coag",
#'     location = "ds", cl_type = "chloramine"
#'   )
#'
#' \donttest{
#' # Initialize parallel processing
#' plan(multisession, workers = 2) # Remove the workers argument to use all available compute
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_dbp_chain(input_water = "balanced_water", cl2 = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' }
#'
#' @import dplyr
#' @export
#'
#' @returns `chemdose_dbp_chain` returns a data frame containing a water class column with predicted DBP concentrations.

chemdose_dbp_chain <- function(df, input_water = "defined_water", output_water = "disinfected_water",
                               cl2 = "use_col", time = "use_col",
                               treatment = "use_col", cl_type = "use_col", location = "use_col") {
  # This allows for the function to process unquoted column names without erroring
  cl2 <- tryCatch(cl2, error = function(e) enquo(cl2))
  time <- tryCatch(time, error = function(e) enquo(time))
  treatment <- tryCatch(treatment, error = function(e) enquo(treatment))
  cl_type <- tryCatch(cl_type, error = function(e) enquo(cl_type))
  location <- tryCatch(location, error = function(e) enquo(location))

  validate_water_helpers(df, input_water)

  # This returns a dataframe of the input arguments and the correct column names for the others
  arguments <- construct_helper(
    df, list(
      "cl2" = cl2, "time" = time, "treatment" = treatment,
      "cl_type" = cl_type, "location" = location
    )
  )

  # Only join inputs if they aren't in existing dataframe
  if (length(arguments$new_cols) > 0) {
    df <- df %>%
      cross_join(as.data.frame(arguments$new_cols))
  }
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        cl2 = !!as.name(arguments$final_names$cl2),
        time = !!as.name(arguments$final_names$time),
        # This logic needed for any argument that has a default
        treatment = ifelse(exists(as.name(arguments$final_names$treatment), where = .),
          !!as.name(arguments$final_names$treatment), "raw"
        ),
        cl_type = ifelse(exists(as.name(arguments$final_names$cl_type), where = .),
          !!as.name(arguments$final_names$cl_type), "chlorine"
        ),
        location = ifelse(exists(as.name(arguments$final_names$location), where = .),
          !!as.name(arguments$final_names$location), "plant"
        )
      ),
      chemdose_dbp
    ))
}
