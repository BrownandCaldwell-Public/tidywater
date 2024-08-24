# Helper Functions
# These functions format the base tidywater functions to make more user friendly

# Author: Libby McKenna
# Reviewers: Sierra Johnson 2/27/24


#' Convert `water` class object to a dataframe
#'
#' This converts a `water` class to a dataframe with individual columns for each slot (water quality parameter) in the `water`.
#' This is useful for one-off checks and is applied in all `fn_once` tidywater functions. For typical applications,
#' there may be a `fn_once` tidywater function that provides a more efficient solution.
#'
#'
#' @param water A water class object
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#'
#' # Generates 1 row dataframe
#' example_df <- define_water(ph = 7, temp = 20, alk = 100) %>%
#'   convert_water()
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(to_dataframe = map(defined_water, convert_water)) %>%
#'   unnest(to_dataframe) %>%
#'   select(-defined_water)
#'
#' @export

convert_water <- function(water) {
  nms <- methods::slotNames(water)
  lst <- lapply(nms, function(nm) methods::slot(water, nm))
  as.data.frame(stats::setNames(lst, nms)) %>%
    select(where(~ any(!is.na(.))))
}

#' @title Convert a `water` class object to a dataframe with ions in mg/L or ug/L
#'
#' @description This function is the same as \code{\link{convert_water}} except it converts the units of following slots from
#' M to mg/L: na, ca, mg, k, cl, so4, hco3, co3, h2po4, hpo4, po4, ocl, bro3, f, fe, al.  These slots are converted to
#' ug/L: br, mn.  All other values remain unchanged.
#'
#' @param water A water class object
#'
#' @examples
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1) %>%
#'   convert_watermg()
#'
#' @export
#'
convert_watermg <- function(water) {
  if (missing(water)) {
    stop("No source water defined. Create a water using the 'define_water' function.")
  }
  if (!methods::is(water, "water")) {
    stop("Input water must be of class 'water'. Create a water using 'define_water'.")
  }

  water@na <- convert_units(water@na, "na", "M", "mg/L")
  water@ca <- convert_units(water@ca, "ca", "M", "mg/L")
  water@mg <- convert_units(water@mg, "mg", "M", "mg/L")
  water@k <- convert_units(water@k, "k", "M", "mg/L")
  water@cl <- convert_units(water@cl, "cl", "M", "mg/L")
  water@so4 <- convert_units(water@so4, "so4", "M", "mg/L")
  water@hco3 <- convert_units(water@hco3, "hco3", "M", "mg/L")
  water@co3 <- convert_units(water@co3, "co3", "M", "mg/L")
  water@h2po4 <- convert_units(water@h2po4, "h2po4", "M", "mg/L")
  water@hpo4 <- convert_units(water@hpo4, "hpo4", "M", "mg/L")
  water@po4 <- convert_units(water@po4, "po4", "M", "mg/L")
  water@ocl <- convert_units(water@ocl, "ocl", "M", "mg/L")

  water@bro3 <- convert_units(water@bro3, "bro3", "M", "mg/L")
  water@f <- convert_units(water@f, "f", "M", "mg/L")
  water@fe <- convert_units(water@fe, "fe", "M", "mg/L")
  water@al <- convert_units(water@al, "al", "M", "mg/L")

  # These get converted to ug/L instead.
  water@br <- convert_units(water@br, "br", "M", "ug/L")
  water@mn <- convert_units(water@mn, "mn", "M", "ug/L")

  convert_water(water)
}

#' Apply `define_water` and output a dataframe
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame.
#' It outputs all carbonate calculations and other parameters in a data frame.
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>% define_water_once()
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>% define_water_once()
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export

define_water_once <- function(df) {
  defined_df <- defined_water <- NULL # Quiet RCMD check global variable note
  df %>%
    define_water_chain() %>%
    mutate(defined_df = furrr::future_map(defined_water, convert_water)) %>%
    unnest_wider(defined_df) %>%
    select(-defined_water) %>%
    as.data.frame()
}

#' Apply `define_water` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be chained with "downstream" tidywater functions.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "defined_water".
#'
#' @seealso \code{\link{define_water}}
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
#'   balance_ions_once()
#'
#' example_df <- water_df %>%
#'   define_water_chain(output_water = "This is a column of water") %>%
#'   balance_ions_once(input_water = "This is a column of water")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_once()
#'
#' #' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

define_water_chain <- function(df, output_water = "defined_water") {
  define_water_args <- c(
    "ph", "temp", "alk", "tot_hard", "ca", "mg", "na", "k", "cl", "so4", "tot_ocl", "tot_po4", "tot_nh4",
    "tds", "cond",
    "toc", "doc", "uv254", "br", "f", "fe", "al", "mn"
  )

  extras <- df %>%
    select(!any_of(define_water_args))

  output <- df %>%
    select(any_of(define_water_args)) %>%
    mutate(!!output_water := furrr::future_pmap(., define_water)) %>%
    select(!any_of(define_water_args)) %>%
    cbind(extras)
}

#' Apply `balance_ions` function and output a dataframe
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame.
#' Its output is a dataframe with updated ions depending on starting concentrations
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using \code{\link{define_water_chain}}
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#'
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_once()
#'
#' example_df <- water_df %>%
#'   define_water_chain(output_water = "Different_defined_water_column") %>%
#'   balance_ions_once(input_water = "Different_defined_water_column")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_once()
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export

balance_ions_once <- function(df, input_water = "defined_water") {
  balance_df <- balanced_water <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    mutate(balanced_water = furrr::future_pmap(list(water = !!as.name(input_water)), balance_ions)) %>%
    mutate(balance_df = furrr::future_map(.data$balanced_water, convert_water)) %>%
    unnest_wider(balance_df) %>%
    select(-balanced_water)
}

#' Apply `balance_ions` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using \code{\link{define_water_chain}}
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "balanced_water".
#'
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' library(purrr)
#' library(furrr)
#' library(tidyr)
#' library(dplyr)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 5)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain(output_water = "balanced ions, balanced life") %>%
#'   chemdose_ph_chain(input_water = "balanced ions, balanced life", naoh = 5)
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 5)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

balance_ions_chain <- function(df, input_water = "defined_water", output_water = "balanced_water") {
  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(list(water = !!as.name(input_water)), balance_ions))
}

#' Apply `chemdose_ph` function and output a dataframe
#'
#' This function allows \code{\link{chemdose_ph}} to be added to a piped data frame.
#' Its output is a data frame with updated ions and pH.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a column(s) name matching a valid chemical(s), the function will dose that chemical(s) in addition to the
#' ones specified in the function's arguments.
#' The column names must match the chemical names as displayed in \code{\link{chemdose_ph}}.
#' To see which chemicals can be passed into the function, see \code{\link{chemdose_ph}}.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param hcl Hydrochloric acid: HCl -> H + Cl
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3po4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param co2 Carbon Dioxide CO2 (gas) + H2O -> H2CO3*
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param caocl2 Calcium hypochlorite: Ca(OCl)2 -> Ca + 2OCl
#' @param nh4oh Amount of ammonium hydroxide added in mg/L as N: NH4OH -> NH4 + OH
#' @param nh42so4 Amount of ammonium sulfate added in mg/L as N: (NH4)2SO4 -> 2NH4 + SO4
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param caco3 Amount of calcium carbonate added (or removed) in mg/L: CaCO3 -> Ca + CO3
#'
#' @seealso \code{\link{chemdose_ph}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_once(input_water = "balanced_water", naoh = 5)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     hcl = seq(1, 12, 1),
#'     naoh = 20
#'   ) %>%
#'   chemdose_ph_once(input_water = "balanced_water", mgoh2 = 55, co2 = 4)
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_once(input_water = "balanced_water", naoh = 5)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export

chemdose_ph_once <- function(df, input_water = "defined_water",
                             hcl = 0, h2so4 = 0, h3po4 = 0, co2 = 0, naoh = 0,
                             na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                             cl2 = 0, naocl = 0, caocl2 = 0, nh4oh = 0, nh42so4 = 0,
                             alum = 0, ferricchloride = 0, ferricsulfate = 0, caco3 = 0) {
  dose_chem <- dosed_chem_water <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    chemdose_ph_chain(
      input_water = input_water, output_water = "dosed_chem_water",
      hcl, h2so4, h3po4, co2, naoh,
      na2co3, nahco3, caoh2, mgoh2,
      cl2, naocl, caocl2, nh4oh, nh42so4,
      alum, ferricchloride, ferricsulfate, caco3
    ) %>%
    mutate(dose_chem = furrr::future_map(dosed_chem_water, convert_water)) %>%
    unnest(dose_chem) %>%
    select(-dosed_chem_water)
}

#' Apply `chemdose_ph` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{chemdose_ph}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' Ions and pH will be updated based on input chemical doses.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a column(s) name matching a valid chemical(s), the function will dose that chemical(s) in addition to the
#' ones specified in the function's arguments.
#' The column names must match the chemical names as displayed in \code{\link{chemdose_ph}}.
#' To see which chemicals can be passed into the function, see \code{\link{chemdose_ph}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "dosed_chem_water".
#' @param hcl Hydrochloric acid: HCl -> H + Cl
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3po4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param co2 Carbon Dioxide CO2 (gas) + H2O -> H2CO3*
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param caocl2 Calcium hypochlorite: Ca(OCl)2 -> Ca + 2OCl
#' @param nh4oh Amount of ammonium hydroxide added in mg/L as N: NH4OH -> NH4 + OH
#' @param nh42so4 Amount of ammonium sulfate added in mg/L as N: (NH4)2SO4 -> 2NH4 + SO4
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param caco3 Amount of calcium carbonate added (or removed) in mg/L: CaCO3 -> Ca + CO3
#'
#' @seealso \code{\link{chemdose_ph}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(input_water = "balanced_water", naoh = 5)
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     hcl = seq(1, 12, 1),
#'     naoh = 20
#'   ) %>%
#'   chemdose_ph_chain(input_water = "balanced_water", mgoh2 = 55, co2 = 4)
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(input_water = "balanced_water", naoh = 5)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

chemdose_ph_chain <- function(df, input_water = "defined_water", output_water = "dosed_chem_water",
                              hcl = 0, h2so4 = 0, h3po4 = 0, co2 = 0, naoh = 0,
                              na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                              cl2 = 0, naocl = 0, caocl2 = 0, nh4oh = 0, nh42so4 = 0,
                              alum = 0, ferricchloride = 0, ferricsulfate = 0, caco3 = 0) {
  ID <- NULL # Quiet RCMD check global variable note
  dosable_chems <- tibble(
    hcl, h2so4, h3po4, co2, naoh,
    na2co3, nahco3, caoh2, mgoh2,
    cl2, naocl, caocl2, nh4oh, nh42so4,
    alum, ferricchloride, ferricsulfate, caco3
  )

  chem_inputs_arg <- dosable_chems %>%
    select_if(~ any(. > 0))

  chem_inputs_col <- df %>%
    subset(select = names(df) %in% names(dosable_chems)) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(chem_inputs_col) - 1 == 0 & length(chem_inputs_arg) == 0) {
    warning("No chemical dose found. Create dose column, enter a dose argument, or check availbility of chemical in the chemdose_ph function.")
  }

  if (length(chem_inputs_col) > 0 & length(chem_inputs_arg) > 0) {
    if (any(names(chem_inputs_arg) %in% names(chem_inputs_col))) {
      stop("At least one chemical was dosed as both a function argument and a data frame column. Remove your chemical(s) from one of these inputs.")
    }
  }

  if (nrow(chem_inputs_arg) == 1) {
    chem_doses <- chem_inputs_col %>%
      cross_join(chem_inputs_arg)
    # Add missing chemical columns
    chem2 <- dosable_chems %>%
      subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
      cross_join(chem_doses) %>%
      mutate(ID = row_number())
  } else if (nrow(chem_inputs_arg) > 1) {
    chem_doses <- chem_inputs_col %>%
      cross_join(chem_inputs_arg)
    chem2 <- dosable_chems %>%
      subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
      unique() %>%
      cross_join(chem_doses)
  }

  output <- df %>%
    subset(select = !names(df) %in% names(chem_inputs_col)) %>%
    mutate(ID = row_number()) %>%
    left_join(chem2, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        hcl = hcl,
        h2so4 = h2so4,
        h3po4 = h3po4,
        co2 = co2,
        naoh = naoh,
        na2co3 = na2co3,
        nahco3 = nahco3,
        caoh2 = caoh2,
        mgoh2 = mgoh2,
        cl2 = cl2,
        naocl = naocl,
        caocl2 = caocl2,
        nh4oh = nh4oh,
        nh42so4 = nh4oh,
        alum = alum,
        ferricchloride = ferricchloride,
        ferricsulfate = ferricsulfate,
        caco3 = caco3
      ),
      chemdose_ph
    )) %>%
    select(!any_of(names(dosable_chems)), any_of(names(chem_doses)))
}

#' Apply `solvedose_ph` to a dataframe and create a new column with numeric dose
#'
#' This function allows \code{\link{solvedose_ph}} to be added to a piped data frame.
#' Its output is a chemical dose in mg/L.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has column(s) named "target_ph" or "chemical", the function will use the column(s)
#' as function argument(s). If these columns aren't present, specify "target_ph" or "chemical" as function arguments.
#' The chemical names must match the chemical names as displayed in \code{\link{solvedose_ph}}.
#' To see which chemicals can be dosed, see \code{\link{solvedose_ph}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_column name of the output column storing doses in mg/L. Default is "dose_required".
#' @param target_ph set a goal for pH using the function argument or a data frame column
#' @param chemical select the chemical to be used to reach the desired pH using function argument or data frame column
#' @seealso \code{\link{solvedose_ph}}
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
#'   balance_ions_chain() %>%
#'   mutate(
#'     target_ph = 10,
#'     chemical = rep(c("naoh", "mgoh2"), 6)
#'   ) %>%
#'   solvedose_ph_once()
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   solvedose_ph_once(input_water = "balanced_water", target_ph = 8.8, chemical = "naoh")
#'
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_ph = seq(9, 10.1, .1)) %>%
#'   solvedose_ph_once(chemical = "naoh")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_ph = seq(9, 10.1, .1)) %>%
#'   solvedose_ph_once(chemical = "naoh")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

solvedose_ph_once <- function(df, input_water = "defined_water", output_column = "dose_required", target_ph = NULL, chemical = NULL) {
  dose <- NULL # Quiet RCMD check global variable note
  dosable_chems <- tibble(
    hcl = 0, h2so4 = 0, h3po4 = 0,
    co2 = 0,
    naoh = 0, caoh2 = 0, mgoh2 = 0,
    na2co3 = 0, nahco3 = 0,
    cl2 = 0, naocl = 0, caocl2 = 0,
    alum = 0, ferricchloride = 0, ferricsulfate = 0
  )

  chem <- df %>%
    filter(chemical %in% names(dosable_chems))

  if (length(chemical) > 0) {
    if (!chemical %in% names(dosable_chems)) {
      stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_ph.")
    }
  }

  if (length(chem$chemical) > 0 & !all(unique(df$chemical) %in% names(dosable_chems))) {
    stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_ph.")
  }

  if ("target_ph" %in% names(df) & length(target_ph) > 0) {
    stop("Target pH was set as both a function argument and a data frame column. Remove your target pH from one of these inputs.")
  }

  if ("chemical" %in% names(df) & length(chemical) > 0) {
    stop("Chemical was set as both a function argument and a data frame column. Remove your chemical from one of these inputs.")
  }

  output <- chem %>%
    mutate(
      target_ph = target_ph,
      chemical = chemical
    ) %>%
    mutate(dose = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        chemical = chemical,
        target_ph = target_ph
      ),
      solvedose_ph
    )) %>%
    mutate(!!output_column := as.numeric(dose)) %>%
    select(-dose)
}

#' Apply `solvedose_alk` to a dataframe and create a new column with numeric dose
#'
#' This function allows \code{\link{solvedose_alk}} to be added to a piped data frame.
#' Its output is a chemical dose in mg/L.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has column(s) named "target_alk" or "chemical", the function will use the column(s)
#' as function argument(s). If these columns aren't present, specify "target_alk" or "chemical" as function arguments.
#' The chemical names must match the chemical names as displayed in \code{\link{solvedose_alk}}.
#' To see which chemicals can be dosed, see \code{\link{solvedose_alk}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column with names for each of the chemicals being dosed.
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_column name of the output column storing doses in mg/L. Default is "dose_required".
#' @param target_alk set a goal for alkalinity using the function argument or a data frame column
#' @param chemical select the chemical to be used to reach the desired alkalinity using function argument or data frame column
#' @seealso \code{\link{solvedose_alk}}
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
#'   balance_ions_chain() %>%
#'   mutate(
#'     target_alk = 300,
#'     chemical = rep(c("naoh", "na2co3"), 6)
#'   ) %>%
#'   solvedose_alk_once()
#'
#' # When the selected chemical can't raise the alkalinity, the dose_required will be NA
#' # Eg,soda ash can't bring the alkalinity to 100 when the water's alkalinity is already at 200.
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   solvedose_alk_once(input_water = "balanced_water", target_alk = 100, chemical = "na2co3")
#'
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_alk = seq(100, 210, 10)) %>%
#'   solvedose_alk_once(chemical = "na2co3")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   mutate(target_alk = seq(100, 210, 10)) %>%
#'   solvedose_alk_once(chemical = "na2co3")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

solvedose_alk_once <- function(df, input_water = "defined_water", output_column = "dose_required", target_alk = NULL, chemical = NULL) {
  dose <- NULL # Quiet RCMD check global variable note
  dosable_chems <- tibble(
    hcl = 0, h2so4 = 0, h3po4 = 0,
    co2 = 0,
    naoh = 0, caoh2 = 0, mgoh2 = 0,
    na2co3 = 0, nahco3 = 0
  )

  chem <- df %>%
    filter(chemical %in% names(dosable_chems))

  if (length(chemical) > 0) {
    if (!chemical %in% names(dosable_chems)) {
      stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_alk")
    }
  }

  if (length(chem$chemical) > 0 & !all(unique(df$chemical) %in% names(dosable_chems))) {
    stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_alk")
  }

  if ("target_alk" %in% names(df) & length(target_alk) > 0) {
    stop("Target alkalinity was set as both a function argument and a data frame column. Remove your target alkalinity from one of these inputs.")
  }

  if ("chemical" %in% names(df) & length(chemical) > 0) {
    stop("Chemical was set as both a function argument and a data frame column. Remove your chemical from one of these inputs.")
  }

  output <- chem %>%
    mutate(
      target_alk = target_alk,
      chemical = chemical
    ) %>%
    mutate(dose = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        chemical = chemical,
        target_alk = target_alk
      ),
      solvedose_alk
    )) %>%
    mutate(!!output_column := as.numeric(dose)) %>%
    select(-dose)
}

#' Apply `blend_waters` to a dataframe and output `water` slots as a dataframe
#'
#' This function allows \code{\link{blend_waters}} to be added to a piped data frame.
#' Its output is a data frame with updated ions and pH.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' The `water` class columns to use in the function are specified as function arguments. Ratios may be input
#' as columns with varied ratios (in this case, input column names in the function arguments), OR input as numbers directly.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}
#' @param waters List of column names containing a water class to be blended
#' @param ratios List of column names or vector of blend ratios in the same order as waters. (Blend ratios must sum to 1)
#'
#' @seealso \code{\link{blend_waters}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   mutate(
#'     ratios1 = .4,
#'     ratios2 = .6
#'   ) %>%
#'   blend_waters_once(waters = c("defined_water", "dosed"), ratios = c("ratios1", "ratios2"))
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_once(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_once(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export


blend_waters_once <- function(df, waters, ratios) {
  blend_df <- blended <- NULL # Quiet RCMD check global variable note
  df_subset <- df %>% select(all_of(waters))

  for (row in 1:length(df_subset[[1]])) {
    water_vectors <- c()
    blend_ratios <- c()

    for (cols in 1:length(df_subset)) {
      water_save <- df_subset[[cols]][row]
      water_vectors <- c(water_vectors, water_save)

      if (is.character(ratios)) {
        df_ratio <- df %>% select(all_of(ratios))
        ratio_save <- df_ratio[[cols]][row]
        blend_ratios <- c(blend_ratios, ratio_save)
      } else {
        ratio_save <- ratios[[cols]]
        blend_ratios <- c(blend_ratios, ratio_save)
      }
    }

    suppressWarnings(df$blended[row] <- list(blend_waters(water_vectors, blend_ratios)))
  }

  output <- df %>%
    mutate(blend_df = furrr::future_map(.data$blended, convert_water)) %>%
    unnest_wider(blend_df) %>%
    select(-blended)
}

#' Apply `blend_waters` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{blend_waters}} to be added to a piped data frame.
#' Its output is a `water` class with updated ions and pH.
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' The `water` class columns to use in the function are specified as function arguments. Ratios may be input
#' as columns with varied ratios (in this case, input column names in the function arguments), OR input as numbers directly.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already
#' been computed using \code{\link{define_water_chain}},
#' @param waters List of column names containing a water class to be blended
#' @param ratios List of column names or vector of blend ratios in the same order as waters. (Blend ratios must sum to 1)
#' @param output_water name of output column storing updated parameters with the class, water. Default is "blended_water".
#'
#' @seealso \code{\link{blend_waters}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22) %>%
#'   mutate(
#'     ratios1 = .4,
#'     ratios2 = .6
#'   ) %>%
#'   blend_waters_chain(
#'     waters = c("defined_water", "dosed_chem_water"),
#'     ratios = c("ratios1", "ratios2"), output_water = "Blending_after_chemicals"
#'   )
#'
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_chain(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(naoh = 22, output_water = "dosed") %>%
#'   blend_waters_chain(waters = c("defined_water", "dosed", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export


blend_waters_chain <- function(df, waters, ratios, output_water = "blended_water") {
  output <- df %>%
    rowwise() %>%
    mutate(
      waters = furrr::future_pmap(across(all_of(waters)), list),
      ratios = ifelse(
        is.numeric(ratios),
        list(ratios),
        (list(c_across(all_of(ratios))))
      )
    ) %>%
    ungroup() %>%
    mutate(!!output_water := furrr::future_pmap(list(waters = waters, ratios = ratios), blend_waters)) %>%
    select(-c(waters, ratios))
}


#' Pluck out a single parameter from a `water` class object
#'
#' This function plucks one or more selected parameters from selected columns of `water` class objects.
#' The names of the output columns will follow the form `water_parameter`
#' To view all slots as columns, please use one of the `fn_once` functions or \code{\link{convert_water}}.
#'
#' @param df a data frame containing a water class column, which has already
#' been computed using \code{\link{define_water}}
#' @param input_waters vector of names of the columns of water class data to be used as the input for this function.
#' @param parameter vector of water class parameters to view outside the water column
#'
#' @seealso \code{\link{convert_water}}
#'
#' @examples
#'
#' library(dplyr)
#' library(furrr)
#' library(purrr)
#' library(tidyr)
#'
#' pluck_example <- water_df %>%
#'   define_water_chain() %>%
#'   pluck_water(parameter = "tot_co3")
#'
#' pluck_example <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   pluck_water(input_waters = c("defined_water", "balanced_water"), parameter = c("na", "cl"))
#'
#' plan(multisession)
#' pluck_example <- water_df %>%
#'   define_water_chain() %>%
#'   pluck_water(parameter = c("ph", "alk"))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#' @import dplyr
#' @export

pluck_water <- function(df, input_waters = c("defined_water"), parameter) {
  if (missing(parameter)) {
    stop("Parameter not specified to pluck.")
  }
  if (!any(parameter %in% methods::slotNames("water"))) {
    stop("One or more parameters doesn't exist in water class.")
  }
  if (!any(input_waters %in% colnames(df))) {
    stop("One or more specified waters doesn't exist in dataframe. Check column names.")
  }


  plucked <- data.frame(row.names = seq(1, nrow(df)))
  for (water in input_waters) {
    if (!methods::is(df[[water]][[1]], "water")) {
      stop("All waters must be of class 'water'.")
    }
    output_column <- paste0(water, "_", parameter)
    temp <- furrr::future_map2(parameter, output_column, ~ {
      df %>%
        mutate(!!as.name(.y) := furrr::future_map_dbl(!!as.name(water), pluck, .x)) %>%
        select(!!as.name(.y))
    }) %>%
      purrr::list_cbind()

    plucked <- bind_cols(plucked, temp)
  }

  bind_cols(df, plucked)
}

#' Apply `dissolve_pb` to a dataframe and create a new column with numeric dose
#'
#' This function allows \code{\link{dissolve_pb}} to be added to a piped data frame.
#' Two additional columns will be added to the dataframe; the name of the controlling lead solid, and total dissolved lead (M).
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' Use the `output_col_solid` and `output_col_result` arguments to name the ouput columns for the controlling lead solid
#' and total dissolved lead, respectively. The input `water` used for the calculation will be appended to the
#' start of these output columns. Omit the input `water` in the output columns, set `water_prefix` to FALSE (default is TRUE).
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_col_solid name of the output column storing the controlling lead solid. Default is "controlling_solid".
#' @param output_col_result name of the output column storing dissolved lead in M. Default is "pb".
#' @param water_prefix name of the input water used for the calculation, appended to the start of output columns. Default is TRUE.
#' Chenge to FALSE to remove the water prefix from output column names.
#' @param hydroxypyromorphite defaults to "Schock", the constant, K, developed by Schock et al (1996). Can also use "Zhu".
#' @param pyromorphite defaults to "Topolska", the constant, K, developed by Topolska et al (2016). Can also use "Xie".
#' @param laurionite defaults to "Nasanen", the constant, K, developed by Nasanen & Lindell (1976). Can also use "Lothenbach".
#' @seealso \code{\link{dissolve_pb}}
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
#'   balance_ions_chain() %>%
#'   dissolve_pb_once(input_water = "balanced_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   dissolve_pb_once(output_col_result = "dissolved_lead", pyromorphite = "Xie")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   dissolve_pb_once(output_col_result = "dissolved_lead", laurionite = "Lothenbach")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest_wider
#' @export

dissolve_pb_once <- function(df, input_water = "defined_water", output_col_solid = "controlling_solid",
                             output_col_result = "pb", hydroxypyromorphite = "Schock",
                             pyromorphite = "Topolska", laurionite = "Nasanen", water_prefix = TRUE) {
  calc <- tot_dissolved_pb <- controlling_solid <- NULL # Quiet RCMD check global variable note
  if (!(hydroxypyromorphite == "Schock" | hydroxypyromorphite == "Zhu")) {
    stop("Hydroxypyromorphite equilibrium constant must be 'Schock' or 'Zhu'.")
  }

  if (!(pyromorphite == "Topolska" | pyromorphite == "Xie")) {
    stop("Pyromorphite equilibrium constant must be 'Topolska' or 'Xie'.")
  }

  if (!(laurionite == "Nasanen" | laurionite == "Lothenbach")) {
    stop("Laurionite equilibrium constant must be 'Nasanen' or 'Lothenbach'.")
  }

  output <- df %>%
    mutate(calc = furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        hydroxypyromorphite = hydroxypyromorphite,
        pyromorphite = pyromorphite,
        laurionite = laurionite
      ),
      dissolve_pb
    )) %>%
    unnest_wider(calc)

  if (water_prefix) {
    output <- output %>%
      rename(
        !!paste(input_water, output_col_result, sep = "_") := tot_dissolved_pb,
        !!paste(input_water, output_col_solid, sep = "_") := controlling_solid
      )
  } else {
    output <- output %>%
      rename(
        !!output_col_result := tot_dissolved_pb,
        !!output_col_solid := controlling_solid
      )
  }
}

#' Apply `chemdose_toc` function and output a data frame
#'
#' This function allows \code{\link{chemdose_toc}} to be added to a piped data frame.
#' Its output is a data frame with updated TOC, DOC, and UV254.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a column(s) name matching a valid coagulant(s), the function will dose that coagulant(s). Note:
#' The function can only dose a coagulant as either a column or from the function arguments, not both.
#'
#' The column names must match the coagulant names as displayed in \code{\link{chemdose_toc}}.
#' To see which coagulants can be passed into the function, see \code{\link{chemdose_toc}}.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column named for the coagulant being dosed,
#' and a column named for the set of coefficients to use.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param coeff String specifying the Edwards coefficients to be used from "Alum", "Ferric", "General Alum", "General Ferric", or "Low DOC" or
#' named vector of coefficients, which must include: k1, k2, x1, x2, x3, b
#'
#' @seealso \code{\link{chemdose_toc}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(alum = 30) %>%
#'   chemdose_toc_once(input_water = "dosed_chem_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     ferricchloride = seq(1, 12, 1),
#'     coeff = "Ferric"
#'   ) %>%
#'   chemdose_toc_once(input_water = "balanced_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_toc_once(input_water = "balanced_water", alum = 40, coeff = "General Alum")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(ferricchloride = seq(1, 12, 1)) %>%
#'   chemdose_toc_once(input_water = "balanced_water", coeff = "Ferric")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export

chemdose_toc_once <- function(df, input_water = "defined_water",
                              alum = 0, ferricchloride = 0, ferricsulfate = 0, coeff = "Alum") {
  dosed_chem_water <- dose_chem <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    chemdose_toc_chain(
      input_water = input_water, output_water = "dosed_chem_water",
      alum, ferricchloride, ferricsulfate, coeff
    ) %>%
    mutate(dose_chem = furrr::future_map(dosed_chem_water, convert_water)) %>%
    unnest(dose_chem) %>%
    select(-dosed_chem_water)
}

#' Apply `chemdose_toc` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows \code{\link{chemdose_toc}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' TOC, DOC, and UV254 will be updated based on input chemical doses.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a coagulant(s) name matching a valid coagulant(s), the function will dose that coagulant(s). Note:
#' The function can only dose a coagulant either a column or from the function arguments, not both.
#'
#' The column names must match the chemical names as displayed in \code{\link{chemdose_toc}}.
#' To see which chemicals can be passed into the function, see \code{\link{chemdose_toc}}.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column named for the coagulant being dosed,
#' and a column named for the set of coefficients to use.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, Water. Default is "coagulated_water".
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param ferricchloride Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param ferricsulfate Amount of ferric sulfate added in mg/L: Fe2(SO4)3*8.8H2O + 6HCO3 -> 2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2
#' @param coeff String specifying the Edwards coefficients to be used from "Alum", "Ferric", "General Alum", "General Ferric", or "Low DOC" or
#' named vector of coefficients, which must include: k1, k2, x1, x2, x3, b
#'
#' @seealso \code{\link{chemdose_toc}}
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
#'   balance_ions_chain() %>%
#'   chemdose_ph_chain(alum = 30) %>%
#'   chemdose_toc_chain(input_water = "dosed_chem_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(
#'     ferricchloride = seq(1, 12, 1),
#'     coeff = "Ferric"
#'   ) %>%
#'   chemdose_toc_chain(input_water = "balanced_water")
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_toc_chain(input_water = "balanced_water", alum = 40, coeff = "General Alum")
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   mutate(ferricchloride = seq(1, 12, 1)) %>%
#'   chemdose_toc_chain(input_water = "balanced_water", coeff = "Ferric")
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

chemdose_toc_chain <- function(df, input_water = "defined_water", output_water = "coagulated_water",
                               alum = 0, ferricchloride = 0, ferricsulfate = 0, coeff = "Alum") {
  ID <- NULL # Quiet RCMD check global variable note
  dosable_chems <- tibble(alum, ferricchloride, ferricsulfate)

  chem_inputs_arg <- dosable_chems %>%
    select_if(~ any(. > 0))

  chem_inputs_col <- df %>%
    subset(select = names(df) %in% names(dosable_chems)) %>%
    # add row number for joining
    mutate(ID = row_number())


  if (length(chem_inputs_col) - 1 == 0 & length(chem_inputs_arg) == 0) {
    warning("No chemical dose found. Create dose column, enter a dose argument, or check availbility of chemical in the chemdose_ph function.")
  }

  if (length(chem_inputs_col) > 1 & length(chem_inputs_arg) > 0) {
    stop("Coagulants were dosed as both a function argument and a data frame column. Choose one input method.")
  }
  if (length(chem_inputs_col) > 2 | length(chem_inputs_arg) > 1) {
    stop("Multiple coagulants dosed. Choose one coagulant.")
  }

  chem_doses <- chem_inputs_col %>%
    cross_join(chem_inputs_arg)
  chem2 <- dosable_chems %>%
    subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
    cross_join(chem_doses)

  if (length(df$coeff) > 0) {
    coeff <- tibble(coeff = df$coeff) %>%
      mutate(ID = row_number())
    chem3 <- chem2 %>%
      left_join(coeff, by = "ID")
  } else if (length(coeff) == 1) {
    chem3 <- chem2 %>%
      mutate(coeff = list(coeff))
  } else if (is.numeric(coeff) & length(coeff) == 6) {
    chem3 <- chem2 %>%
      mutate(coeff = list(coeff))
  } else {
    stop("coeffs must be specified with a string or named vector. See documentation for acceptable formats.")
  }

  output <- df %>%
    subset(select = !names(df) %in% c("alum", "ferricchloride", "ferricsulfate", "coeff")) %>%
    mutate(ID = row_number()) %>%
    left_join(chem3, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        alum = alum,
        ferricchloride = ferricchloride,
        ferricsulfate = ferricsulfate,
        coeff = coeff
      ),
      chemdose_toc
    )) %>%
    select(!any_of(names(dosable_chems)), any_of(names(chem_doses)))
}

#' Apply `calculate_corrosion` to a dataframe and create new columns with up to 6 corrosion indices
#'
#' This function allows \code{\link{calculate_corrosion}} to be added to a piped data frame.
#' Up to six additional columns will be added to the dataframe depending on what corrosion/scaling
#' indices are selected: Aggressive index (AI), Ryznar index (RI), Langelier saturation index (LSI),
#' Larson-Skold index (LI), chloride-to-sulfate mass ratio (CSMR) & calcium carbonate precipitation potential (CCPP).
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#' for the option to use parallel processing and speed things up. To initialize parallel processing, use
#' `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#' `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#' shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, created using \code{\link{define_water}}
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param index The indices to be calculated.
#'  Default calculates all six indices: "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr".
#'  CCPP may not be able to be calculated sometimes, so it may be advantageous to leave this out of the function to avoid errors
#' @param form Form of calcium carbonate mineral to use for modelling solubility: "calcite" (default), "aragonite", or "vaterite"
#' @seealso \code{\link{calculate_corrosion}}
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
#'   calculate_corrosion_once()
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   calculate_corrosion_once(index = c("aggressive", "ccpp"))
#'
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   calculate_corrosion_once(index = c("aggressive", "ccpp"))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export

calculate_corrosion_once <- function(df, input_water = "defined_water", index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
                                     form = "calcite") {
  corrosion_indices <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    calculate_corrosion_chain(input_water = input_water, index = index, form = form) %>%
    mutate(index = furrr::future_map(corrosion_indices, convert_water)) %>%
    unnest(index) %>%
    select(-corrosion_indices) %>%
    select_if(~ any(!is.na(.)))
}


#' Apply `calculate_corrosion` to a dataframe and output a column of `water` class to be chained to other tidywater functions.
#'
#' This function allows \code{\link{calculate_corrosion}} to be added to a piped data frame.
#' Up to six additional columns will be added to the output `water` class column depending on what corrosion/scaling
#' indices are selected: Aggressive index (AI), Ryznar index (RI), Langelier saturation index (LSI),
#' Larson-Skold index (LI), chloride-to-sulfate mass ratio (CSMR) & calcium carbonate precipitation potential (CCPP).
#'
#'
#' The data input comes from a `water` class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' The `water` class column to use in the function is specified in the `input_water` argument (default input `water` is "defined_water".
#' The name of the output `water` class column defaults to  "corrosion_indices", but may be altered using the `output_water` argument.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#' for the option to use parallel processing and speed things up. To initialize parallel processing, use
#' `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#' `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#' shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}}, and a column named for each of the chemicals being dosed
#' @param input_water name of the column of water class data to be used as the input. Default is "defined_water".
#' @param output_water name of output column storing updated indices with the class, water. Default is "corrosion_indices".
#' @param index The indices to be calculated.
#'  Default calculates all six indices: "aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"
#'  CCPP may not be able to be calculated sometimes, so it may be advantageous to leave this out of the function to avoid errors
#' @param form Form of calcium carbonate mineral to use for modelling solubility: "calcite" (default), "aragonite", or "vaterite"
#' @seealso \code{\link{calculate_corrosion}}
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
#'   calculate_corrosion_chain()
#'
#' example_df <- water_df %>%
#'   define_water_chain() %>%
#'   calculate_corrosion_chain(index = c("aggressive", "ccpp"))

#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'  define_water_chain() %>%
#'  calculate_corrosion_chain(index = c("aggressive", "ccpp"))
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

calculate_corrosion_chain <- function(df, input_water = "defined_water", output_water = "corrosion_indices",
                                      index = c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"),
                                      form = "calcite") {
  if (any(!index %in% c("aggressive", "ryznar", "langelier", "ccpp", "larsonskold", "csmr"))) {
    stop("Index must be one or more of c('aggressive', 'ryznar', 'langelier', 'ccpp', 'larsonskold', 'csmr')")
  }

  index <- list(index)

  output <- df %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        index = index,
        form = form
      ),
      calculate_corrosion
    ))
}

#' Apply `chemdose_dbp`function within a data frame and output a data frame
#'
#' DBP = disinfection byproduct
#'
#' This function allows \code{\link{chemdose_dbp}} to be added to a piped data frame.
#' Its output is a data frame containing columns for TTHM, HAA5, and individual DBP species.
#' DBPs are estimated based on the applied chlorine dose, the reaction time, treatment type, chlorine type, and DBP formation location.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a chlorine dose column (cl2) or time column (time), the function will use those columns. Note:
#' The function can only take cl2 and time inputs as EITHER a column or from the function arguments, not both.
#'
#' tidywater functions cannot be added after this function because they require a `water` class input.
#'
#' For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#' for the option to use parallel processing and speed things up. To initialize parallel processing, use
#' `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#' `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#' shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_once}}. The df may include a column named for the applied chlorine dose (cl2),
#' and a column for time.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param cl2 Applied chlorine dose (mg/L as Cl2). Model results are valid for doses between 1.51 and 33.55 mg/L.
#' @param time Reaction time (hours). Model results are valid for reaction times between 2 and 168 hours.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default),
#' "coag" for water that has been coagulated or softened, and "gac" for water that has been treated by granular activated carbon (GAC).
#' GAC treatment has also been used for estimating formation after membrane treatment with good results.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @param location Location for DBP formation, either in the "plant" (default), or in the distribution system, "ds".
#'
#' @seealso \code{\link{chemdose_dbp}}
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
#'     time = 30
#'   ) %>%
#'   chemdose_dbp_once(input_water = "balanced_water")
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
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_dbp_once(input_water = "balanced_water", cl2 = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @export

chemdose_dbp_once <- function(df, input_water = "defined_water", cl2 = 0, time = 0,
                              treatment = "raw", cl_type = "chlorine", location = "plant") {
  temp_dbp <- dbps <- NULL # Quiet RCMD check global variable note
  output <- df %>%
    chemdose_dbp_chain(
      input_water = input_water, output_water = "temp_dbp",
      cl2, time, treatment, cl_type, location
    ) %>%
    mutate(dbps = furrr::future_map(temp_dbp, convert_water)) %>%
    unnest(dbps) %>%
    select(-temp_dbp)
}

#' Apply `chemdose_dbp` within a data frame and output a column of `water` class to be chained to other tidywater functions
#'
#' DBP = disinfection byproduct
#'
#' This function allows \code{\link{chemdose_dbp}} to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be used with "downstream" tidywater functions.
#' TTHM, HAA5, and individual DBP species will be updated based on the applied chlorine dose,
#' the reaction time, treatment type, chlorine type, and DBP formation location.
#'
#' The data input comes from a `water` class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a chlorine dose column (cl2) or time column (time), the function will use those columns. Note:
#' The function can only take cl2 and time inputs as EITHER a column or from the function arguments, not both.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a water class column, which has already been computed using
#' \code{\link{define_water_chain}}. The df may include a column named for the applied chlorine dose (cl2),
#' and a column for time.
#' @param input_water name of the column of water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "disinfected_water".
#' @param cl2 Applied chlorine dose (mg/L as Cl2). Model results are valid for doses between 1.51 and 33.55 mg/L.
#' @param time Reaction time (hours). Model results are valid for reaction times between 2 and 168 hours.
#' @param treatment Type of treatment applied to the water. Options include "raw" for no treatment (default),
#' "coag" for water that has been coagulated or softened, and "gac" for water that has been treated by granular activated carbon (GAC).
#' GAC treatment has also been used for estimating formation after membrane treatment with good results.
#' @param cl_type Type of chlorination applied, either "chlorine" (default) or "chloramine".
#' @param location Location for DBP formation, either in the "plant" (default), or in the distribution system, "ds".
#'
#' @seealso \code{\link{chemdose_dbp}}
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
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#'   mutate(br = 50) %>%
#'   define_water_chain() %>%
#'   balance_ions_chain() %>%
#'   chemdose_dbp_chain(input_water = "balanced_water", cl2 = 4, time = 8)
#'
#' # Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @import dplyr
#' @export

chemdose_dbp_chain <- function(df, input_water = "defined_water", output_water = "disinfected_water",
                               cl2 = 0, time = 0, treatment = "raw", cl_type = "chlorine", location = "plant") {
  ID <- NULL # Quiet RCMD check global variable note
  inputs_arg <- tibble(cl2, time) %>%
    select_if(~ any(. > 0))

  inputs_col <- df %>%
    subset(select = names(df) %in% c("cl2", "time")) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(inputs_col) < 2 & length(inputs_arg) == 0) {
    warning("Cl2 and time arguments missing. Add them as a column or function argument.")
  }

  if (("cl2" %in% colnames(inputs_arg) & "cl2" %in% colnames(inputs_col)) | ("time" %in% colnames(inputs_arg) & "time" %in% colnames(inputs_col))) {
    stop("Chlorine and/or time were dosed as both a function argument and a data frame column. Choose one input method.")
  }

  cl_time <- inputs_col %>%
    cross_join(inputs_arg)

  output <- df %>%
    subset(select = !names(df) %in% c("cl2", "time")) %>%
    mutate(
      ID = row_number(),
      treatment = treatment,
      cl_type = cl_type,
      location = location
    ) %>%
    left_join(cl_time, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(
      list(
        water = !!as.name(input_water),
        cl2 = cl2,
        time = time,
        treatment = treatment,
        cl_type = cl_type,
        location = location
      ),
      chemdose_dbp
    ))
}
