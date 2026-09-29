# Profile chemdose_ph_df() across alum-dosing scenarios.

library(tidywater)
library(dplyr)
library(tidyr)

alum_doses <- tibble(alum = seq(10, 100, 10))

raw_water <- water_df |>
  define_water_df() |>
  balance_ions_df() |>
  # Create several dosing scenarios.
  cross_join(alum_doses)

raise_ph <- tibble(
  chemical = c("naoh", "mgoh2"),
  target_ph = c(8, 8)
)

profvis::profvis(
  {
    solve_column <- raw_water |>
      chemdose_ph_df(input_water = "balanced") |>
      cross_join(raise_ph) |>
      solvedose_ph_df(input_water = "dosed_chem")
  },
  interval = 0.005
)
