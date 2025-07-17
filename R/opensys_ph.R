# Open system option

opensys_ph <- function(input_water, partialpressure = 10^-3.5) {
  validate_water(input_water, slots = c("tot_co3"))
  
  co2 <- 10^-1.5 * partialpressure # KH for CO2 is 10^-1.5
  
  output_water <- input_water %>%
    chemdose_ph(co2 = co2)
}
  