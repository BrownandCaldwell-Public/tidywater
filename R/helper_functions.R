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

#' Define water in a pipe without tidywater functions following
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame. 
#' It outputs all carbonate calculations and other parameters in a data frame. 
#' Units of all chemical additions in mg/L as chemical (not as product).
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
# 
# 
# test <- water_df %>%
#   define_water_once()

#' Define water pipeable function
#'
#' This function allows \code{\link{define_water}} to be added to a piped data frame. 
#' It outputs all carbonate calculations and other parameters in a data frame. 
#' Units of all chemical additions in mg/L as chemical (not as product).
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

FUA_balance <- FUA1 %>%
  rbind(FLA1) %>%
  mutate(tot_ocl = 0, po4 = 0) %>%
  define_water_once()

test_water <- define_water(7, 22, 133, 133, 122, 23, 23, 23, 24, 1, 0)

df <- data.frame(ph =7, temp=22, alk=133, tot_hard=133, ca_hard =122,
                 na =23, k=  23, cl=23, so4=24, tot_ocl=1, po4=0)
test_water_extras <- data.frame(ph =7, temp=22, alk=133, tot_hard=133, ca_hard =122,
                                na =23, k=  23, cl=23, so4=24, tot_ocl=1, po4=0, Fluoride = 1)

# pipe <- test_water %>%
end <- define_water(test_water)
