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
#' # Put example code here
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
# # 
# library(tidyverse)
# test <- water_df %>%
#   define_water_once()


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
#' # Put example code here
#'
#' @export

define_water_pipe <- function(df) {

  extras <- df %>%
    select(-c(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4))
  
  output<- df %>%
    select(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4) %>%
    mutate(define_water = pmap(., define_water)) %>%
    select(-c(ph,temp,alk,tot_hard,ca_hard,na,k,cl,so4, tot_ocl, po4)) %>%
    cbind(extras)
}

# def_pipe <- water_df %>%
#   mutate(libby = 234879) %>%
#   define_water_pipe()



#' Balance Ions Once
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame. 
#' Its output is a dataframe with updated ions depending on starting concentrations
#' tidywater functions cannot be added after this function because they require a water class input.
#' 
#' @param df a data frame containing a column, define water, which has already been computed using \code{\link{define_water}}
#'
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' # Put example code here
#'
#' @export

balance_ions_once <- function(df) {
  
  output<- df %>%
    mutate(balanced_water = map(define_water, balance_ions)) %>%
    mutate(balance_df = map(balanced_water, convert_Water)) %>%
    unnest_wider(balance_df) %>%
    select(-balanced_water)
    
}


# def_pipe <- water_df %>%
#   mutate(libby = 234879) %>%
#   define_water_pipe() %>%
#   balance_ions_once()
#   mutate(bal_ion  = map(define_water, balance_ions)) %>%
#   mutate(balance_df = map(bal_ion, convert_Water)) %>%
#   unnest_wider(balance_df)
# def_pipe$define_water[[1]]


#' Balance Ions pipe
#'
#' This function allows \code{\link{balance_ions}} to be added to a piped data frame. 
#' Its output is a water class, and can therefore be used with "downstream" tidywater functions.
#' 
#' @param df a data frame containing a column, define water, which has already been computed using \code{\link{define_water}}
#'
#' @seealso \code{\link{balance_ions}}
#'
#' @examples
#' # Put example code here
#'
#' @export

balance_ions_pipe <- function(df) {
  
  output<- df %>%
    mutate(balanced_water = map(define_water, balance_ions))
  
}

# balance_pipe <- water_df %>% 
#   define_water_pipe() %>%
#   balance_ions_pipe()
