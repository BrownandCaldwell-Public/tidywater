#' @title Determine blended water quality from multiple waters based on mass balance and acid/base equilibrium
#'
#' @description This function takes a vector of waters defined by [define_water]
#' and a vector of ratios and outputs a new water object with updated ions and pH.
#' For a single blend use `blend_waters`; for a dataframe use `blend_waters_df`.
#' Use [pluck_water] to get values from the output water as new dataframe columns.
#'
#' @param waters Vector of source waters created by [define_water]. For `df` function, this can include
#' quoted column names and/or existing single water objects unquoted.
#' @param ratios Vector of ratios in the same order as waters. (Blend ratios must sum to 1). For `df` function,
#' this can also be a list of quoted column names.
#'
#' @seealso [define_water]
#'
#' @examples
#' water1 <- define_water(7, 20, 50)
#' water2 <- define_water(7.5, 20, 100, tot_nh3 = 2)
#' blend_waters(c(water1, water2), c(.4, .6))
#'
#' @export
#'
#' @returns `blend_waters` returns a water class object with blended water quality parameters.
#'
blend_waters <- function(waters, ratios) {
  if (length(waters) != length(ratios)) {
    stop("Length of waters vector must equal length of ratios vector.")
  }

  if (!is.list(waters)) {
    stop("Waters must be provided as a vector.")
  }

  if (!is.numeric(ratios)) {
    stop("Ratios must provided as a numeric vector.")
  }

  if (round(sum(ratios), 5) != 1.0) {
    stop("Blend ratios do not sum up to 1")
    # print(sum(ratios)) # this is for checking why the function is breaking
  }

  # Identify slots that are not NA for blending
  s4todata <- function(water) {
    names <- methods::slotNames(water)
    lt <- lapply(names, function(names) methods::slot(water, names))
    as.list(stats::setNames(lt, names))
  }

  parameters <- s4todata(waters[[1]])
  parameters <- names(parameters[!is.na(parameters)])
  otherparams <- c()
  if (length(waters) > 1) {
    for (i in 2:length(waters)) {
      tempparams <- s4todata(waters[[i]])
      tempparams <- names(tempparams[!is.na(tempparams)])
      otherparams <- c(otherparams, tempparams)
    }
    missingn <- setdiff(parameters, otherparams)
    missing1 <- setdiff(otherparams, parameters)
    if (!rlang::is_empty(missingn) | !rlang::is_empty(missing1)) {
      missing <- paste0(c(missingn, missing1), collapse = ", ")
      warning(paste0(
        "The following parameters are missing in some of the waters and will be set to NA in the blend:\n   ", missing,
        "\nTo fix this, make sure all waters provided have the same parameters specified."
      ))
    }
  }

  not_averaged <- c(
    "ph", "kw", "estimated"
  )
  parameters <- setdiff(parameters, not_averaged)

  # Initialize empty blended water
  blended_water <- methods::new("water")
  # Loop through all slots that have a number and blend.
  for (param in parameters) {
    for (i in 1:length(waters)) {
      temp_water <- waters[[i]]
      if (!methods::is(temp_water, "water")) {
        stop("All input waters must be of class 'water'. Create a water using define_water.")
      }
      ratio <- ratios[i]

      if (is.na(methods::slot(blended_water, param))) {
        methods::slot(blended_water, param) <- methods::slot(temp_water, param) * ratio
      } else {
        methods::slot(blended_water, param) <- methods::slot(temp_water, param) * ratio + methods::slot(blended_water, param)
      }
    }
  }

  # Track estimated params
  estimated <- c()

  for (i in 1:length(waters)) {
    # Create character vectors that just add the values from all the waters together
    temp_water <- waters[[i]]
    new_est <- unlist(strsplit(temp_water@estimated, "_"))
    estimated <- c(estimated, new_est)
  }

  # Keep only one of each estimated and paste back into string for the water.
  blended_water@estimated <- paste(unique(estimated), collapse = "_")

  # Calculate new pH, H+ and OH- concentrations
  # Calculate kw from temp
  tempa <- blended_water@temp + 273.15 # absolute temperature (K)
  pkw <- round((4787.3 / (tempa)) + (7.1321 * log10(tempa)) + (0.010365 * tempa) - 22.801, 1) # water equilibrium rate constant temperature conversion from Harned & Hamer (1933)
  blended_water@kw <- 10^-pkw

  # Calculate new pH
  ph <- solve_ph(blended_water)
  gamma1 <- calculate_activity(1, blended_water@is, blended_water@temp)
  h <- (10^-ph) / gamma1
  blended_water@oh <- blended_water@kw / (h * gamma1^2)
  blended_water@h <- h
  blended_water@ph <- ph

  # Correct eq constants
  k <- correct_k(blended_water)

  # Recalculate carbonate, dic, phosphate, ocl, and nh4 speciation given new pH
  alpha0 <- calculate_alpha0_carbonate(h, k) # proportion of total carbonate as H2CO3
  alpha1 <- calculate_alpha1_carbonate(h, k) # proportion of total carbonate as HCO3-
  alpha2 <- calculate_alpha2_carbonate(h, k) # proportion of total carbonate as CO32-
  blended_water@h2co3 <- blended_water@tot_co3 * alpha0
  blended_water@hco3 <- blended_water@tot_co3 * alpha1
  blended_water@co3 <- blended_water@tot_co3 * alpha2

  blended_water@dic <- blended_water@tot_co3 * tidywater::mweights$dic * 1000

  alpha1p <- calculate_alpha1_phosphate(h, k) # proportion of total phosphate as H2PO4-
  alpha2p <- calculate_alpha2_phosphate(h, k) # proportion of total phosphate as HPO42-
  alpha3p <- calculate_alpha3_phosphate(h, k) # proportion of total phosphate as PO43-

  blended_water@h2po4 <- blended_water@tot_po4 * alpha1p
  blended_water@hpo4 <- blended_water@tot_po4 * alpha2p
  blended_water@po4 <- blended_water@tot_po4 * alpha3p

  blended_water@ocl <- blended_water@free_chlorine * calculate_alpha1_hypochlorite(h, k)
  blended_water@nh4 <- blended_water@tot_nh3 * calculate_alpha1_ammonia(h, k)

  if (blended_water@tot_nh3 > 0 &
    (blended_water@free_chlorine > 0 | blended_water@combined_chlorine > 0)) {
    warning("Both chlorine and ammonia are present and may form chloramines.\nUse chemdose_chloramine for breakpoint caclulations.")
  }

  return(blended_water)
}

#' @rdname blend_waters
#'
#' @param df a data frame containing a water class column, which has already been computed using [define_water_df]
#' @param output_water name of output column storing updated parameters with the class, water. Default is "blended_water".
#'
#' @examples
#'
#' example_df <- water_df %>%
#'   dplyr::slice_head(n = 3) %>%
#'   define_water_df() %>%
#'   chemdose_ph_df(naoh = 22) %>%
#'   dplyr::mutate(
#'     ratios1 = .4,
#'     ratios2 = .6
#'   ) %>%
#'   blend_waters_df(
#'     waters = c("defined", "dosed_chem"),
#'     ratios = c("ratios1", "ratios2"), output_water = "Blending_after_chemicals"
#'   )
#'
#' \donttest{
#' waterA <- define_water(7, 20, 100, tds = 100)
#' example_df <- water_df %>%
#'   dplyr::slice_head(n = 3) %>%
#'   define_water_df() %>%
#'   blend_waters_df(waters = c("defined", waterA), ratios = c(.8, .2))
#' }
#'
#' @export
#'
#' @returns `blend_waters_df` returns a data frame with a water class column containing blended water quality

blend_waters_df <- function(df, waters, ratios, output_water = "blended") {
  n <- 0
  water_names <- list()
  for (water in waters) {
    n <- n + 1

    if (!is.character(water)) {
      output <- paste0("merging_water_", n)
      df[[output]] <- list(water)
      water_names[n] <- output
    } else {
      water_names[n] <- water
    }
  }
  water_names <- unlist(water_names)

  for (water_col in waters) {
    if (is.character(water_col)) {
      if (!(water_col %in% colnames(df))) {
        stop(paste("Specified input_water column -", water_col, "- not found. Check spelling or create a water class column using define_water_df()."))
      } else if (!all(sapply(df[[water_col]], function(x) methods::is(x, "water")))) {
        stop(paste("Specified input_water column", water_col, "does not contain water class objects. Use define_water_df() or specify a different column."))
      }
    } else if (!is.character(water_col) & !methods::is(water_col, "water")) {
      stop(paste("Specified input_water column", water_col, "does not contain water class objects. Use define_water_df() or specify a different column."))
    }
  }

  output <- df
  output$waters <- apply(output[, water_names], 1, function(row) as.list(row))

  if (is.numeric(ratios)) {
    output$ratios <- replicate(nrow(output), ratios, simplify = FALSE) # vector of ratios
  } else {
    output$ratios <- lapply(seq_len(nrow(output)), function(i) { # column names
      as.numeric(unlist(output[i, ratios]))
    })
  }

  output[[output_water]] <- lapply(seq_len(nrow(output)), function(i) {
    blend_waters(
      waters = output$waters[[i]],
      ratios = output$ratios[[i]]
    )
  })

  cols_to_remove <- c("waters", "ratios", grep("merging_water_", names(output), value = TRUE))
  output <- output[, !(names(output) %in% cols_to_remove)]

  return(output)
}
