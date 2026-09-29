# Profile the warning-heavy path: a water with unknown background ions, so every
# chemdose_ph() call warns about the slots it cannot update. solvedose_ph() calls
# chemdose_ph() once per uniroot iteration inside suppressWarnings(), so these
# warnings are built and thrown away thousands of times.

library(tidywater)
library(dplyr)
library(tidyr)

sparse_water <- water_df[, c("ph", "temp", "alk", "toc", "doc", "uv254")]

workload <- function() {
  sparse_water |>
    define_water_df() |>
    cross_join(tibble(alum = seq(10, 100, 10))) |>
    chemdose_ph_df(input_water = "defined", output_water = "alum_dosed") |>
    solvedose_ph_df(input_water = "alum_dosed", target_ph = 8, chemical = "naoh") |>
    chemdose_ph_df(input_water = "alum_dosed", output_water = "caustic_dosed", naoh = dose)
}

# Warm up so package loading and first-call overhead stay out of the timings.
invisible(suppressWarnings(workload()))
print(system.time(suppressWarnings(workload())))

profvis::profvis(suppressWarnings(workload()), interval = 0.005)
