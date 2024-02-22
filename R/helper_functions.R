# Helper Functions
# These functions format the base tidywater functions to make more user friendly

# Author: Libby McKenna
# Reviewers: 


#### Function to convert from Water class to a dataframe. Not exported in namespace.

# test_water <- define_water(7, 22, 133, 133, 122, 23, 23, 23, 24, 1, 0)


convert_Water <- function(water) {
  nms <- slotNames(water)
  
  lst <- lapply(nms, function(nm) slot(water, nm))
  as.data.frame(setNames(lst, nms))
}
