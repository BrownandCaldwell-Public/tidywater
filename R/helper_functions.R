# Helper Functions
# These functions format the base tidywater functions to make more user friendly

# Author: Libby McKenna
# Reviewers: Sierra Johnson 2/27/24


#' Convert water class object to a dataframe
#'
#' This converts a water class to a dataframe with individual columns for each slot (water quality parameter) in the water.
#' This is useful for one-off checks and is applied in all `fn_once` tidywater functions. For typical applications,
#' there may be a `fn_once` tidywater function that provides a more efficient solution.
#' 
#'
#' @param water A "water" class object
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
#' convert_water()
#'
#' example_df <- water_df %>%
#' define_water_chain() %>%
#' mutate(to_dataframe  = map(defined_water, convert_water)) %>%
#' unnest(to_dataframe) %>%
#' select(-defined_water)
#'
#' @export

convert_water <- function(water) {
  nms <- slotNames(water)
  lst <- lapply(nms, function(nm) slot(water, nm))
  as.data.frame(setNames(lst, nms))
}

#' Apply define_water and output a dataframe
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame.
#' It outputs all carbonate calculations and other parameters in a data frame.
#' tidywater functions cannot be added after this function because they require a water class input.
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
#'# Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>% define_water_once()
#' 
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @export

define_water_once <- function(df) {

  df %>%
    define_water_chain() %>%
    mutate(defined_df = furrr::future_map(defined_water, convert_water)) %>%
    unnest_wider(defined_df) %>%
    select(-defined_water) %>%
    as.data.frame()

}

#' Apply define_water within a dataframe and output a column of water class to be chained to other tidywater functions
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame.
#' Its output is a "water" class, and can therefore be chained with "downstream" tidywater functions.
#' 
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use 
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the 
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#' @param output_water name of the output column storing updated parameters with the class, Water. Default is "defined_water".
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
#' define_water_chain() %>%
#' balance_ions_once()
#' 
#' example_df <- water_df %>%
#' define_water_chain(output_water = "This is a column of water") %>%
#' balance_ions_once(input_water ="This is a column of water")
#'
#'# Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#' define_water_chain() %>%
#' balance_ions_once()
#' 
#' #' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @export

define_water_chain <- function(df, output_water = "defined_water") {

  define_water_args <- c("ph","temp","alk","tot_hard","ca_hard","na","k","cl","so4", "tot_ocl", "tot_po4", "tds", "cond",
                         "toc", "doc", "uv254")

  extras <- df %>%
    select(!any_of(define_water_args))

  output <- df %>%
    select(any_of(define_water_args)) %>%
    mutate(!!output_water := furrr::future_pmap(., define_water)) %>%
    select(!any_of(define_water_args)) %>%
    cbind(extras)
}

#' Apply balance_ions function and output a dataframe
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame.
#' Its output is a dataframe with updated ions depending on starting concentrations
#' tidywater functions cannot be added after this function because they require a water class input.
#' 
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use 
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the 
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a column, defined_water, which has already been computed using \code{\link{define_water}}
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
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
#' define_water_chain() %>%
#' balance_ions_once()
#'
#' example_df <- water_df %>%
#' define_water_chain(output_water = "Different_defined_water_column") %>%
#' balance_ions_once(input_water = "Different_defined_water_column")
#'
#'# Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#' define_water_chain() %>%
#' balance_ions_once()
#'
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#' 
#' @export

balance_ions_once <- function(df, input_water = "defined_water") {

  output<- df %>%
    mutate(balanced_water = furrr::future_pmap(list(water = !!as.name(input_water)), balance_ions)) %>%
    mutate(balance_df = furrr:: future_map(balanced_water, convert_water)) %>%
    unnest_wider(balance_df) %>%
    select(-balanced_water)

}

#' Apply balance_ions within a dataframe and output a column of water class to be chained to other tidywater functions
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame.
#' Its output is a "water" class, and can therefore be used with "downstream" tidywater functions.
#' 
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use 
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the 
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a column, defined_water, which has already been computed using \code{\link{define_water}}
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, Water. Default is "balanced_water".
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
#' define_water_chain() %>%
#' balance_ions_chain() %>%
#' chemdose_ph_chain(naoh = 5)
#'
#' example_df <- water_df %>%
#' define_water_chain() %>%
#' balance_ions_chain(output_water = "balanced ions, balanced life") %>%
#' chemdose_ph_chain(input_water = "balanced ions, balanced life", naoh = 5)
#' 
#' # Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#' define_water_chain() %>%
#' balance_ions_chain() %>%
#' chemdose_ph_chain(naoh = 5)
#'
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @export

balance_ions_chain <- function(df, input_water = "defined_water", output_water = "balanced_water") {

  output<- df %>%
    mutate(!!output_water := furrr::future_pmap(list(water = !!as.name(input_water)), balance_ions))

}

#' Apply chemdose_ph function and output a dataframe
#'
#' This function allows \code{\link{chemdose_ph}} to be added to a piped data frame.
#' Its output is a data frame with updated ions and pH.
#'
#' The data input comes from a Water class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#'
#' If the input data frame has a column(s) name matching a valid chemical(s), the function will dose that chemical(s) in addition to the
#' ones specified in the function's arguments.
#' The column names must match the chemical names as displayed in \code{\link{chemdose_ph}}.
#' To see which chemicals can be passed into the function, see \code{\link{chemdose_ph}}.
#'
#' tidywater functions cannot be added after this function because they require a water class input.
#' 
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use 
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the 
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}} or \code{\link{balance_ions}}. The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param hcl Hydrochloric acid: HCl -> H + Cl
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3po4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param caocl2 Calcium hypochlorite: Ca(OCl)2 -> Ca + 2OCl
#' @param co2 Carbon Dioxide CO2 (gas) + H2O -> H2CO3*
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param fecl3 Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param fe2so43 Ferric sulfate Fe2(SO4)3 + 6HCO3 -> 2Fe(OH)3(am) +3SO4 + 6CO2
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
#' define_water_chain() %>%
#' balance_ions_chain() %>%
#' chemdose_ph_once(input_water = "balanced_water", naoh = 5)
#'
#' example_df <- water_df %>%
#'  define_water_chain() %>%
#'   balance_ions_chain()%>%
#'   mutate(hcl = seq(1,12, 1),
#'          naoh = 20) %>%
#'   chemdose_ph_once(input_water = "balanced_water", mgoh2 = 55, co2 = 4)
#'  
#'# Initialize parallel processing   
#' plan(multisession)   
#' example_df <- water_df %>%
#' define_water_chain() %>%
#' balance_ions_chain() %>%
#' chemdose_ph_once(input_water = "balanced_water", naoh = 5)
#'
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @export

chemdose_ph_once <- function(df, input_water = "defined_water", hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0,
                               na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                               cl2 = 0, naocl = 0, caocl2 = 0, co2 = 0,
                               alum = 0, fecl3 = 0, fe2so43 = 0, caco3 = 0) {

  dosable_chems <- tibble(hcl, h2so4, h3po4, naoh,
                          na2co3, nahco3, caoh2, mgoh2,
                          cl2, naocl, caocl2, co2,
                          alum, fecl3, fe2so43, caco3)

  output <- df %>%
    chemdose_ph_chain(input_water = input_water, output_water = "dosed_chem_water",
                      hcl, h2so4, h3po4, naoh,
                      na2co3, nahco3, caoh2, mgoh2,
                      cl2, naocl, caocl2, co2,
                      alum, fecl3, fe2so43, caco3) %>%
    mutate(dose_chem = furrr::future_map(dosed_chem_water, convert_water)) %>%
    unnest(dose_chem) %>%
    select(-dosed_chem_water)
}

#' Apply chemdose_ph within a dataframe and output a column of water class to be chained to other tidywater functions
#'
#' This function allows \code{\link{chemdose_ph}} to be added to a piped data frame.
#' Its output is a "water" class, and can therefore be used with "downstream" tidywater functions.
#' Ions and pH will be updated based on input chemical doses.
#'
#' The data input comes from a Water class column, as initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
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
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}} or \code{\link{balance_ions}}. The df may include columns named for the chemical(s) being dosed.
#' @param input_water name of the column of Water class data to be used as the input for this function. Default is "defined_water".
#' @param output_water name of the output column storing updated parameters with the class, Water. Default is "dosed_chem_water".
#' @param hcl Hydrochloric acid: HCl -> H + Cl
#' @param h2so4 Sulfuric acid: H2SO4 -> 2H + SO4
#' @param h3po4 Phosphoric acid: H3PO4 -> 3H + PO4
#' @param naoh Caustic: NaOH -> Na + OH
#' @param na2co3 Soda ash: Na2CO3 -> 2Na + CO3
#' @param nahco3 Sodium bicarbonate: NaHCO3 -> Na + H + CO3
#' @param caoh2 Lime: Ca(OH)2 -> Ca + 2OH
#' @param mgoh2  Magneisum hydroxide: Mg(OH)2 -> Mg + 2OH
#' @param cl2 Chlorine gas: Cl2(g) + H2O -> HOCl + H + Cl
#' @param naocl Sodium hypochlorite: NaOCl -> Na + OCl
#' @param caocl2 Calcium hypochlorite: Ca(OCl)2 -> Ca + 2OCl
#' @param co2 Carbon Dioxide CO2 (gas) + H2O -> H2CO3*
#' @param alum Hydrated aluminum sulfate Al2(SO4)3*14H2O + 6HCO3 -> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2
#' @param fecl3 Ferric Chloride FeCl3 + 3HCO3 -> Fe(OH)3(am) + 3Cl + 3CO2
#' @param fe2so43 Ferric sulfate Fe2(SO4)3 + 6HCO3 -> 2Fe(OH)3(am) +3SO4 + 6CO2
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
#' define_water_chain() %>%
#' balance_ions_chain() %>%
#' chemdose_ph_chain(input_water = "balanced_water", naoh = 5)
#'
#' example_df <- water_df %>%
#'  define_water_chain() %>%
#'   balance_ions_chain()%>%
#'   mutate(hcl = seq(1,12, 1),
#'          naoh = 20) %>%
#'   chemdose_ph_chain(input_water = "balanced_water", mgoh2 = 55, co2 = 4)
#'
#'# Initialize parallel processing
#' plan(multisession)
#' example_df <- water_df %>%
#' define_water_chain() %>%
#' balance_ions_chain() %>%
#' chemdose_ph_chain(input_water = "balanced_water", naoh = 5)
#' 
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#' 
#' @export

chemdose_ph_chain <- function(df, input_water = "defined_water", output_water = "dosed_chem_water",
                               hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0,
                               na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                               cl2 = 0, naocl = 0, caocl2 = 0, co2 = 0,
                               alum = 0, fecl3 = 0, fe2so43 = 0, caco3 =0) {
 
  dosable_chems <- tibble(hcl, h2so4, h3po4, naoh,
                            na2co3, nahco3, caoh2, mgoh2,
                            cl2, naocl, caocl2, co2,
                            alum, fecl3, fe2so43, caco3)

  chem_inputs_arg <- dosable_chems %>%
    select_if(~any(. > 0))

  chem_inputs_col <- df %>%
    subset(select = names(df) %in% names(dosable_chems)) %>%
    # add row number for joining
    mutate(ID = row_number())

  if (length(chem_inputs_col) - 1 == 0 & length(chem_inputs_arg) == 0) {
    warning("No chemical dose found. Create dose column, enter a dose argument, or check availbility of chemical in the chemdose_ph function.")}

  if (length(chem_inputs_col) > 0 & length(chem_inputs_arg) >0){
    if (any(names(chem_inputs_arg) %in% names(chem_inputs_col))) {
      stop("At least one chemical was dosed as both a function argument and a data frame column. Remove your chemical(s) from one of these inputs.")}}

if(nrow(chem_inputs_arg) == 1) {
    chem_doses <- chem_inputs_col %>%
      cross_join(chem_inputs_arg)
    # Add missing chemical columns
    chem2 <- dosable_chems %>%
      subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
      cross_join(chem_doses) %>%
      mutate(ID = row_number())

  } else if(nrow(chem_inputs_arg) > 1) {

    chem_inputs_arg <- chem_inputs_arg %>%
      mutate(ID = row_number())
    chem_doses <- chem_inputs_col %>%
      left_join(chem_inputs_arg, by = "ID")
    chem2 <- dosable_chems %>%
      subset(select = !names(dosable_chems) %in% names(chem_doses)) %>%
      mutate(ID = row_number()) %>%
      left_join(chem_doses, by = "ID")
  }

  output<- df %>%
    subset(select = !names(df) %in% names(chem_inputs_col)) %>%
    mutate(ID = row_number()) %>%
    left_join(chem2, by = "ID") %>%
    select(-ID) %>%
    mutate(!!output_water := furrr::future_pmap(list(water= !!as.name(input_water),
                                        hcl = hcl,
                                        h2so4 = h2so4,
                                        h3po4 = h3po4,
                                        naoh= naoh,
                                        na2co3 = na2co3,
                                        nahco3 = nahco3,
                                        caoh2 = caoh2,
                                        mgoh2 = mgoh2,
                                        cl2 = cl2,
                                        naocl = naocl,
                                        caocl2=caocl2,
                                        co2=co2,
                                        alum= alum,
                                        fecl3= fecl3,
                                        fe2so43= fe2so43,
                                        caco3 =caco3),
                                   chemdose_ph)) %>%
    select(!any_of(names(dosable_chems)), any_of(names(chem_doses)))

}


#' Apply solvedose_ph to a dataframe and create a new column with numeric dose
#'
#' This function allows \code{\link{solvedose_ph}} to be added to a piped data frame.
#' Its output is a chemical dose in mg/L.
#'
#' The data input comes from a Water class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
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
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}}, and a column named for each of the chemicals being dosed
#' @param input_water name of the column of Water class data to be used as the input. Default is "defined_water".
#' @param output_water name of the output column storing doses in mg/L. Default is "dose_required".
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
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'mutate(target_ph = 10,
#'       chemical = rep(c("naoh", "mgoh2"), 6)) %>%
#'solvedose_ph_once()
#'
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'solvedose_ph_once(input_water = "balanced_water", target_ph = 8.8, chemical = "naoh")
#'
#'
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'mutate(target_ph = seq(9, 10.1, .1)) %>%
#'solvedose_ph_once(chemical = "naoh")
#'
#'# Initialize parallel processing
#' plan (multisession)
#' example_df <- water_df %>%
#'define_water_chain() %>%
#'mutate(target_ph = seq(9, 10.1, .1)) %>%
#'solvedose_ph_once(chemical = "naoh")
#'
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @export

solvedose_ph_once <- function(df, input_water = "defined_water", output_water = "dose_required", target_ph = NULL, chemical = NULL) {
 
  dosable_chems <-  tibble(
    # hcl = 0, h2so4 = 0, h3po4 = 0,
                           co2 = 0,
                           naoh = 0, caoh2 = 0, mgoh2 = 0,
                           # na2co3 = 0, nahco3 = 0,
                           # cl2 = 0, naocl = 0, caocl2 = 0,
                           # alum = 0, fecl3 = 0, fe2so43 = 0
    )

  chem <- df %>%
    filter(chemical %in% names(dosable_chems))

  if (length(chemical) > 0) {
     if (!chemical %in% names(dosable_chems)) {
    stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_ph.")}}

  if (length(chem$chemical) > 0 & !all(unique(df$chemical) %in% names(dosable_chems))) {
    stop("Can't find chemical. Check spelling or list of valid chemicals in solvedose_ph.")}

  if ("target_ph" %in% names(df) & length(target_ph) >0) {
    stop("Target pH was set as both a function argument and a data frame column. Remove your target pH from one of these inputs.")}

  if ("chemical" %in% names(df) & length(chemical) > 0) {
    stop("Chemcial was set as both a function argument and a data frame column. Remove your chemical from one of these inputs.")}

  output<- chem %>%
    mutate(target_ph = target_ph,
           chemical = chemical) %>%
    mutate(dose = furrr::future_pmap(list(water= !!as.name(input_water),
                                     chemical = chemical,
                                     target_ph = target_ph),
                                solvedose_ph)) %>%
    mutate(!!output_water := as.numeric(dose)) %>%
    select(-dose)
}


#' Apply blend_waters to a dataframe and output water as a dataframe
#'
#' This function allows \code{\link{blend_waters}} to be added to a piped data frame.
#' Its output is a data frame with updated ions and pH.
#'
#' The data input comes from a Water class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' The Water class columns to use in the function are specified as function arguments. Ratios may be input
#' as columns with varied ratios (in this case, input column names in the function arguments), OR input as numbers directly.
#'
#' tidywater functions cannot be added after this function because they require a water class input.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use 
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the 
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}}, and a column named for each of the chemicals being dosed
#' @param waters List of column names containing a Water class to be blended
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
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'chemdose_ph_chain(naoh = 22) %>%
#'mutate(ratios1 = .4,
#'       ratios2 = .6) %>%
#'blend_waters_once(waters = c("defined_water", "dosed_chem_water"), ratios = c("ratios1", "ratios2"))
#'
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'chemdose_ph_chain(naoh = 22) %>%
#'blend_waters_once(waters = c("defined_water", "dosed_chem_water", "balanced_water"), ratios = c(.2, .3, .5))
#'
#'# Initialize parallel processing
#' plan(multisession)
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'chemdose_ph_chain(naoh = 22) %>%
#'blend_waters_once(waters = c("defined_water", "dosed_chem_water", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @export


blend_waters_once <- function(df, waters, ratios) {

 df_subset <- df %>% select(all_of(waters))

for(row in 1:length(df_subset[[1]])) {

  water_vectors <- c()
  blend_ratios <- c()

  for(cols in 1:length(df_subset)) {

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
    mutate(blend_df = furrr::future_map(blended, convert_water)) %>%
    unnest_wider(blend_df) %>%
    select(-blended)

}

#' Apply blend_waters within a dataframe and output a column of water class to be chained to other tidywater functions
#'
#' This function allows \code{\link{blend_waters}} to be added to a piped data frame.
#' Its output is a "water" class with updated ions and pH.
#'
#' The data input comes from a Water class column, initialized in \code{\link{define_water}} or \code{\link{balance_ions}}.
#' The Water class columns to use in the function are specified as function arguments. Ratios may be input
#' as columns with varied ratios (in this case, input column names in the function arguments), OR input as numbers directly.
#'
#'  For large datasets, using `fn_once` or `fn_chain` may take many minutes to run. These types of functions use the furrr package
#'  for the option to use parallel processing and speed things up. To initialize parallel processing, use 
#'  `plan(multisession)` or `plan(multicore)` (depending on your operating system) prior to your piped code with the 
#'  `fn_once` or `fn_chain` functions. Note, parallel processing is best used when your code block takes more than a minute to run,
#'  shorter run times will not benefit from parallel processing.
#'
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}}, and a column named for each of the chemicals being dosed
#' @param waters List of column names containing a Water class to be blended
#' @param ratios List of column names or vector of blend ratios in the same order as waters. (Blend ratios must sum to 1)
#' @param output_water name of output column storing updated parameters with the class, Water. Default is "blended_water".
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
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'chemdose_ph_chain(naoh = 22) %>%
#'mutate(ratios1 = .4,
#'       ratios2 = .6) %>%
#'blend_waters_chain(waters = c("defined_water", "dosed_chem_water"), 
#'ratios = c("ratios1", "ratios2"), output_water = "Blending_after_chemicals")
#'
#'
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'chemdose_ph_chain(naoh = 22) %>%
#'blend_waters_chain(waters = c("defined_water", "dosed_chem_water", "balanced_water"), ratios = c(.2, .3, .5))
#'
#'# Initialize parallel processing
#'plan(multisession)
#'example_df <- water_df %>%
#'define_water_chain() %>%
#'balance_ions_chain() %>%
#'chemdose_ph_chain(naoh = 22) %>%
#'blend_waters_chain(waters = c("defined_water", "dosed_chem_water", "balanced_water"), ratios = c(.2, .3, .5))
#'
#' #Optional: explicitly close multisession processing
#' plan(sequential)
#'
#' @export


blend_waters_chain <- function(df, waters, ratios, output_water = "blended_water") {

  output <- df %>%
    rowwise() %>%
    mutate(waters = furrr::future_pmap(across(all_of(waters)), list),
           ratios = ifelse(
             is.numeric(ratios),
             list(ratios),
             (list(c_across(all_of(ratios))))
           )) %>%
    ungroup() %>%
    mutate(!!output_water := furrr::future_pmap(list(waters = waters, ratios = ratios), blend_waters)) %>%
    select(-c(waters, ratios))
}


#' Pluck out a single parameter from a water class object
#'
#' This function plucks a selected parameter from a column of water class objects.
#' To view multiple parameters, please use one of the "fn_once" functions. 
#'
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}}
#' @param input_water name of the column of Water class data to be used as the input for this function.
#' @param parameter water class attribute to view outside the water column
#' @param output_column name of output column storing the plucked variable's values
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
#'   pluck_water(parameter = "na", output_column = "defined_na") %>%
#'  pluck_water(input_water = "balanced_water", parameter = "na", output_column = "balanced_na")
#'
#' @export

pluck_water <- function(df, input_water = "defined_water", parameter, output_column = NULL){
  if (missing(parameter)) {
    stop("Parameter not specified to pluck.")
  }

  if(is.null(output_column)) {
    output_column = parameter
  } 
  
  df %>% 
    mutate(!!output_column := furrr::future_map_dbl(!!as.name(input_water), pluck, parameter))
  
}