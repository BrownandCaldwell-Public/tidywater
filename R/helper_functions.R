# Helper Functions
# These functions format the base tidywater functions to make more user friendly

# Author: Libby McKenna
# Reviewers: 


#### Function to convert from Water class to a dataframe. Not exported in namespace.

test_water <- define_water(7, 22, 133, 133, 122, 23, 23, 23, 24, 1, 0)

df <- data.frame(ph =7, temp=22, alk=133, tot_hard=133, ca_hard =122,
                         na =23, k=  23, cl=23, so4=24, tot_ocl=1, po4=0)
test_water_extras <- data.frame(ph =7, temp=22, alk=133, tot_hard=133, ca_hard =122,
                                na =23, k=  23, cl=23, so4=24, tot_ocl=1, po4=0, Fluoride = 1)

# pipe <- test_water %>%
  end <- define_water(test_water)

convert_Water <- function(water) {
  nms <- slotNames(water)
  lst <- lapply(nms, function(nm) slot(water, nm))
  as.data.frame(setNames(lst, nms))
}

define_water_help <- function(df) {
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
    
  ouput <- cbind(df2, water_to_df)
}

# FUA_balance <- FUA1 %>%
#   mutate(FUA_water = future_pmap(., define_water_map)) %>%
#   select(-c(alk:so4)) %>%
#   unnest_wider(FUA_water) #%>%
#   balance_ions()
  
FUA_balance2 <- FUA1 %>%
  mutate(tot_ocl = 0, po4 = 0) %>%
  define_water_help()
