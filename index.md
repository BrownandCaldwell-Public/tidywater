# tidywater

## Overview

Tidywater incorporates published water chemistry and empirical models in
a standard format. The modular functions allow for building custom,
comprehensive drinking water treatment processes. Functions are designed
to work in a [tidyverse](https://www.tidyverse.org/) workflow.

## Installation

``` r
# Install tidywater from CRAN:
install.packages("tidywater")

# Alternatively, install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("BrownandCaldwell-Public/tidywater")
```

## Examples

In this first example, acid-base chemistry and TOC removal models are
demonstrated. This example uses tidywater base functions to model a
single water quality scenario.

``` r
library(tidywater)
library(tidyverse)
## Use base tidywater functions to model water quality for a single scenario.
base_coagulation <- define_water(ph = 8, alk = 90, tds = 50, toc = 3, doc = 2.8, uv254 = 0.08) %>%
  # note that we get a warning about sulfate from this code because we didn't specify sulfate in the define_water
  chemdose_ph(alum = 30) %>%
  chemdose_toc(alum = 30)
#> Warning in chemdose_ph(., alum = 30): Sulfate-containing chemical dosed, but
#> so4 water slot is NA. Slot not updated because background so4 unknown.
```

To model multiple water quality scenarios, use tidywater’s helper
functions (x_df) to apply the models to a dataframe. The `pluck_cols`
argument can be added to return the parameters impacted by the model as
separate columns. The `pluck_water` function can pull any parameter from
a water into a separate column.

``` r

coagulation <- water_df %>%
  define_water_df(output_water = "raw") %>%
  mutate(alum = 30) %>%
  chemdose_ph_df(input_water = "raw", output_water = "phchange") %>% # return "phchange" water
  chemdose_toc_df(input_water = "phchange", output_water = "coag", pluck_cols = TRUE) # return "coag" water and coag_doc, coag_toc, coag_uv254 as columns

## To get individual parameters, use `pluck_water`
coagulation <- coagulation %>%
  pluck_water(input_waters = c("raw", "coag"), parameter = c("ph", "doc"))
```

Note that these functions use a “water” class. The “water” class is the
foundation of the package; it provides a mechanism for linking models in
any order while maintaining water quality information. The
`define_water` function takes water quality inputs, but
`define_water_df` may be used to convert a dataframe to a list of
“waters”.

For more detailed examples on tidywater functions and how to use “water”
class data, please see the tidywater vignettes:
`browseVignettes("tidywater")`

## Limitations

This project is maintained by volunteers and is provided without
warranties or guarantees of any kind.

Use at your own risk. For official support, please contact Brown and
Caldwell.

Please read our CONTRIBUTING.md and SECURITY.md before submitting issues
or pull requests.
