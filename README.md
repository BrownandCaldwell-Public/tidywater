
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidywater <img src="man/figures/tidywaterlogo.png" align="right" height = "138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/BrownandCaldwell-Public/tidywater/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BrownandCaldwell-Public/tidywater/actions/workflows/R-CMD-check.yaml)
[![License:MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/BrownandCaldwell-Public/tidywater/tree/main?tab=License-2-ov-file#mit-license)
[![License:Apache
2.0](https://img.shields.io/badge/license-Apache%20License%202.0-blue)](https://github.com/BrownandCaldwell-Public/tidywater/tree/main?tab=License-2-ov-file#mit-license)
<!-- badges: end -->

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
  chemdose_ph(alum = 30) %>%
  chemdose_toc(alum = 30)
#> Warning in chemdose_ph(., alum = 30): Sulfate-containing chemical dosed, but
#> so4 water slot is NA. Slot not updated because background so4 unknown.
```

To model multiple water quality scenarios, use tidywater’s helper
functions (x_chain or x_once) to apply the models to a dataframe.

``` r
## x_chain functions apply models to a list of "waters", and output a list of "waters" so that
## the data can be piped into the next tidywater model.
coagulation <- water_df %>%
  define_water_chain(output_water = "raw") %>%
  mutate(alum = 30) %>%
  chemdose_ph_chain(input_water = "raw", output_water = "phchange") %>%
  chemdose_toc_chain(input_water = "phchange", output_water = "coag")
#> Warning: `chemdose_toc_chain()` was deprecated in tidywater 0.10.0.
#> ℹ Please use `chemdose_toc_df()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: `chemdose_ph_chain()` was deprecated in tidywater 0.10.0.
#> ℹ Please use `chemdose_ph_df()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: `define_water_chain()` was deprecated in tidywater 0.10.0.
#> ℹ Please use `define_water_df()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.

## To get out individual parameters, use `pluck_water`
coagulation <- coagulation %>%
  pluck_water(input_waters = c("raw", "coag"), parameter = c("ph", "doc"))
```

Note that these functions use a “water” class. The “water” class is the
foundation of the package; it provides a mechanism for linking models in
any order while maintaining water quality information. The
`define_water` function takes water quality inputs, but
`define_water_chain` may be used to convert a dataframe to a list of
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
