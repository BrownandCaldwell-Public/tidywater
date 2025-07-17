# Open system option

opensys_ph <- function(input_water, partialpressure = 10^-3.5) {
  validate_water(input_water, slots = c("tot_co3"))
  
  co2_M <- 10^-1.5 * partialpressure # KH for CO2 is 10^-1.5
  # co2_mg <- convert_units(co2_M, "co2", "M", "mg/L")
  # 
  # output_water <- input_water %>%
  #   chemdose_ph(co2 = co2_mg)
  # 
  # output_water@tot_co3 <- co2_M + output_water@hco3 + output_water@co3
  # 
  discons <- tidywater::discons
  k1co3 <- K_temp_adjust(discons["k1co3", ]$deltah, discons["k1co3", ]$k, input_water@temp)
  k2co3 <- K_temp_adjust(discons["k2co3", ]$deltah, discons["k2co3", ]$k, input_water@temp)

  # alpha1 <- calculate_alpha1_carbonate(output_water@h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as HCO3-
  # alpha2 <- calculate_alpha2_carbonate(output_water@h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as CO32-
  #
  # output_water@h2co3 <- co2_M
  # output_water@hco3 <- output_water@tot_co3 * alpha1
  # output_water@co3 <- output_water@tot_co3 * alpha2
  
  return(output_water)
}
  