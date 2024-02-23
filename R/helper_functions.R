# Helper Functions
# These functions format the base tidywater functions to make more user friendly

# Author: Libby McKenna
# Reviewers: 


#### Function to convert from Water class to a dataframe. Not exported in namespace.

convert_Water <- function(water) {
  nms <- slotNames(water)
  lst <- lapply(nms, function(nm) slot(water, nm))
  as.data.frame(setNames(lst, nms))
}

#' Define Water Once
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame. 
#' It outputs all carbonate calculations and other parameters in a data frame. 
#' tidywater functions cannot be added after this function because they require a water class input. 
#'
#' @param df a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' define_water_once(water_df)
#' 
#' example_df <- water_df %>% define_water_once()
#'
#' @export

define_water_once <- function(df) {
  ph = df$ph
  temp =df$temp
  alk = df$alk
  tot_hard = df$tot_hard
  ca_hard = df$ca_hard
  na = df$na
  k = df$k
  cl = df$cl
  so4 = df$so4
  tot_ocl = 0 + df$tot_ocl
  po4 = 0+ df$po4
  
  df2 <- df %>%
    select(-c(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4))
  
  water_to_df <- define_water(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4) %>%
    convert_Water()
}


#' Define Water pipe
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame. 
#' Its output is a water class, and can therefore be used with "downstream" tidywater functions.
#' 
#' @param df a data frame containing columns with all the parameters listed in \code{\link{define_water}}
#'
#' @seealso \code{\link{define_water}}
#'
#' @examples
#' define_water_pipe(water_df)
#' 
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' balance_ions_once()
#'
#' @export

define_water_pipe <- function(df) {

  extras <- df %>%
    select(-c(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4))
  
  output<- df %>%
    select(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4) %>%
    mutate(defined_water = pmap(., define_water)) %>%
    select(-c(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4)) %>%
    cbind(extras)
}

#' Balance Ions Once
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame. 
#' Its output is a dataframe with updated ions depending on starting concentrations
#' tidywater functions cannot be added after this function because they require a water class input.
#' 
#' @param df a data frame containing a column, defined_water, which has already been computed using \code{\link{define_water}}
#'
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' 
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' balance_ions_once() 
#'
#' @export

balance_ions_once <- function(df) {
  
  output<- df %>%
    mutate(balanced_water = map(defined_water, balance_ions)) %>%
    mutate(balance_df = map(balanced_water, convert_Water)) %>%
    unnest_wider(balance_df) %>%
    select(-balanced_water)
    
}


#' Balance Ions pipe
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame. 
#' Its output is a water class, and can therefore be used with "downstream" tidywater functions.
#' 
#' @param df a data frame containing a column, defined_water, which has already been computed using \code{\link{define_water}}
#'
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' balance_ions_pipe() %>%
#' dose_chemical_pipe()
#'
#' @export

balance_ions_pipe <- function(df) {
  
  output<- df %>%
    mutate(balanced_water = map(defined_water, balance_ions))
  
}

#' Dose Chemical Once
#'
#' This function allows \code{\link{dose_chemical}} to be added to a piped data frame. 
#' Its output is a data frame with updated ions and pH.
#' 
#' The data input comes from the column, defined_water, as intialized in \code{\link{define_water}}.
#' If the water output from \code{\link{balance_ions}} is desired as the input, rename the
#' column, 'balanced_water", to "defined_water".
#' 
#' The input data frame requires a column for each chemical being dosed, with data indicating the dose for that column's chemical.
#' The column names must match the chemical names as displayed in \code{\link{dose_chemical}}. 
#' To see which chemicals can be passed into the function, see \code{\link{dose_chemical}}. 
#' 
#' tidywater functions cannot be added after this function because they require a water class input.
#' 
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}}, and a column named for the chemical being dosed
#'
#' @seealso \code{\link{dose_chemical}}
#'
#' @examples
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' balance_ions_pipe() %>%
#' mutate(define_water = balanced_water) %>%
#' dose_chemical_once()
#' 
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' mutate(hcl = seq(1,12, 1),
#' naoh = 20) %>%
#' dose_chemical_once()
#'
#' @export

dose_chemical_once <- function(df) {
  
  dosable_chems <-  tibble(hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0,
                           na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                           cl2 = 0, naocl = 0, caocl2 = 0, co2 = 0,
                           alum = 0, fecl3 = 0, fe2so43 = 0)
   
  
  chem <- df %>%
    subset(select = names(df) %in% names(dosable_chems))
  
  if (length(chem) == 0) {
    stop("No chemical dose column found. Check column name or availbility of chemical in the dose_chemical function.")}

chem2 <- dosable_chems %>%
  subset(select = !names(dosable_chems) %in% names(df)) %>%
  cross_join(chem)
  
  output<- df %>%
    mutate(dosed_chem_water = pmap(list(water= defined_water,
                                        hcl = chem2$hcl,
                                        h2so4 = chem2$h2so4,
                                        h3po4 = chem2$h3po4,
                                        naoh= chem2$naoh,
                                        na2co3 = chem2$na2co3,
                                        nahco3 = chem2$nahco3,
                                        caoh2 = chem2$caoh2,
                                        mgoh2 = chem2$mgoh2,
                                        cl2 = chem2$cl2,
                                        naocl = chem2$naocl,
                                        caocl2=chem2$caocl2,
                                        co2=chem2$co2,
                                        alum= chem2$alum, 
                                        fecl3= chem2$fecl3,
                                        fe2so43= chem2$fe2so43),
                                   dose_chemical)) %>%
    mutate(dose_chem = map(dosed_chem_water, convert_Water)) %>%
    unnest(dose_chem) %>%
    select(-dosed_chem_water) 
}

#' Dose Chemical Pipe
#'
#' This function allows \code{\link{dose_chemical}} to be added to a piped data frame. 
#' Its output is a water class, and can therefore be used with "downstream" tidywater functions.
#' Ions and pH will be updated based on input chemical doses.
#' 
#' The data input comes from the column, defined_water, as initialized in \code{\link{define_water}}.
#' If the water output from \code{\link{balance_ions}} is desired as the input, rename the
#' column, 'balanced_water", to "defined_water".
#' 
#' The input data frame requires a column for each chemical being dosed, with data indicating the dose for that column's chemical. 
#' The column names must match the chemical names as displayed in \code{\link{dose_chemical}}. 
#' To see which chemicals can be passed into the function, see \code{\link{dose_chemical}}. 
#' 
#' @param df a data frame containing a column, define_water, which has already
#' been computed using \code{\link{define_water}}, and a column named for each of the chemicals being dosed
#'
#' @seealso \code{\link{dose_chemical}}
#'
#' @examples
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' balance_ions_pipe() %>%
#' mutate(define_water = balanced_water) %>%
#' dose_chemical_pipe()
#' 
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' mutate(hcl = seq(1,12, 1),
#' naoh = 20) %>%
#' dose_chemical_pipe() %>%
#' balance_ions_once()
#'
#' @export

dose_chemical_pipe <- function(df) {
  
  dosable_chems <-  tibble(hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0,
                           na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                           cl2 = 0, naocl = 0, caocl2 = 0, co2 = 0,
                           alum = 0, fecl3 = 0, fe2so43 = 0)
  
  
  chem <- df %>%
    subset(select = names(df) %in% names(dosable_chems))
  
  if (length(chem) == 0) {
    stop("No chemical dose column found. Check column name or availbility of chemical in the dose_chemical function.")}
  
  chem2 <- dosable_chems %>%
    subset(select = !names(dosable_chems) %in% names(df)) %>%
    cross_join(chem)
  
  output<- df %>%
    mutate(dosed_chem_water = pmap(list(water= defined_water,
                                        hcl = chem2$hcl,
                                        h2so4 = chem2$h2so4,
                                        h3po4 = chem2$h3po4,
                                        naoh= chem2$naoh,
                                        na2co3 = chem2$na2co3,
                                        nahco3 = chem2$nahco3,
                                        caoh2 = chem2$caoh2,
                                        mgoh2 = chem2$mgoh2,
                                        cl2 = chem2$cl2,
                                        naocl = chem2$naocl,
                                        caocl2=chem2$caocl2,
                                        co2=chem2$co2,
                                        alum= chem2$alum, 
                                        fecl3= chem2$fecl3,
                                        fe2so43= chem2$fe2so43),
                                   dose_chemical))
}

#' Dose Target Pipe
#'
#' This function allows \code{\link{dose_target}} to be added to a piped data frame. 
#' Its output is a chemical dose in mg/L.
#' 
#' The data input comes from the column, defined_water, as intialized in \code{\link{define_water}}.
#' If the water output from \code{\link{balance_ions}} is desired as the input, rename the
#' column, 'balanced_water", to "defined_water".
#' 
#' The input data frame requires a column, "chemical", indicating which chemical should be used to reach the target pH
#' The input data frame also requires a column, "target_ph", indicating the final pH after the selected chemical is added
#' The column names must match the chemical names as displayed in \code{\link{dose_target}}. 
#' To see which chemicals can be passed into the function, see \code{\link{dose_target}}. 
#' 
#' 
#' @param df a data frame containing a column, defined_water, which has already
#' been computed using \code{\link{define_water}}, and a column named for each of the chemicals being dosed
#'
#' @seealso \code{\link{dose_target}}
#'
#' @examples
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' dose_target_once()
#' 
#' example_df <- water_df %>% 
#' define_water_pipe() %>% 
#' mutate(hcl = seq(1,12, 1),
#' naoh = 20) %>%
#' dose_target_once()
#'
#' @export

dose_target_once <- function(df) {
  
  dosable_chems <-  tibble(hcl = 0, h2so4 = 0, h3po4 = 0, naoh = 0,
                           na2co3 = 0, nahco3 = 0, caoh2 = 0, mgoh2 = 0,
                           cl2 = 0, naocl = 0, caocl2 = 0, co2 = 0,
                           alum = 0, fecl3 = 0, fe2so43 = 0)
  
  
  chem <- df %>%
    filter(chemical %in% names(dosable_chems))
  
  bad_chem <- df %>%
    filter(!chemical %in% names(dosable_chems)) #%>%
    # mutate(Dose_Required = NA)
  
  if (length(chem$chemical) == 0) {
    stop("No chemical column found. Check 'chemical' column name or create column 'chemical'.")}
  
  if (length(bad_chem$chemical) > 0 & length(chem$chemical) == 0) {
    stop("Chemical(s) not recognized. Check availbility or spelling of chemical(s) in the dose_target function")}
  
  if (length(bad_chem$chemical) > 0 & length(chem$chemical) > 0) {
    warning("Some chemicals not recognized. Only computing a dose for recognized chemicals.")}
 
  if ("target_ph" %in% names(df) == FALSE) {
    stop("No target pH indicated. Check 'target_ph' column name or create column 'target_ph'.")}
  
  
  # chem2 <- dosable_chems %>%
  #   subset(select = !names(dosable_chems) %in% names(df)) %>%
  #   cross_join(chem)
  
  output<- chem %>%
    mutate(Dose_Required = pmap(list(water= defined_water, 
                                     chemical = chemical,
                                     target_ph = target_ph),
                                dose_target)) %>%
    bind_rows(bad_chem)# %>%
    # mutate(Dose_Required = case_when(Dose_Required == "NULL" ~ NA, TRUE ~Dose_Required))
    # select(-dosed_chem_water) 
}
# df <- example_df

example_df <- water_df %>% 
define_water_pipe() %>% 
  mutate(dsfsaf = 23432) %>%
 balance_ions_pipe() %>%
  mutate(target_ph = 10,
         chemical = rep(c("naoh", "mgoh2"), 6)
         ) %>%
  dose_target_once()# %>%
  # balance_ions_once()
  
  mutate(dose_targ = pmap(list(water= balanced_water, target_ph = 9, chemical = "naoh"), dose_target)) #%>%
  mutate(dose_chem = map(dosed_chem_water, convert_Water)) %>%
  unnest(dose_chem)

def <- define_water(
  ph = 8,
  temp =  23,
  alk = 100,
  tot_hard = 100,
  ca_hard = 50,
  na= 20,
  k= 20,
  cl = 20,
  so4 =20,
  tot_ocl = 1,
  po4 = 0)

dose_chemical(def, naoh = 20)
  

# water_df <- data.frame(
#   ph = rep(c(7.9, 8.5, 8.1, 7.8), 3),
#   temp =  rep(c(20, 25, 19), 4),
#   alk = rep(c(50, 80, 100, 200), 3),
#   tot_hard = rep(c(50, 75, 100, 30, 400, 110), 2),
#   ca_hard = rep(c(50, 70, 65, 20, 350, 100), 2),
#   na= rep(c(20, 90), 6),
#   k= rep(c(20, 90), 6),
#   cl = rep(c(30, 92), 6),
#   so4 = rep(c(20, 40, 60, 80), 3),
#   tot_ocl = rep(c(0, 1), 6),
#   po4 = rep(c(0, 0, 1), 4))


# def_pipe <- water_df %>%
#   mutate(libby = 234879) %>%
#   define_water_pipe() %>%
#   balance_ions_once()
#   mutate(bal_ion  = map(define_water, balance_ions)) %>%
#   mutate(balance_df = map(bal_ion, convert_Water)) %>%
#   unnest_wider(balance_df)
# def_pipe$define_water[[1]]

# balance_pipe <- water_df %>% 
#   define_water_pipe() %>%
#   balance_ions_pipe()
