
# tidywater

<!-- badges: start -->
[![R-CMD-check](https://github.com/BrownandCaldwell-Public/tidywater/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BrownandCaldwell-Public/tidywater/actions/workflows/R-CMD-check.yaml)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
<!-- badges: end -->

Tidywater was built to incorporate published water chemistry and empirical models in a standard format with one centralized tool. 
Functions are built to be chained together to model a complete treatment process. Functions are designed to work in a [tidyverse](https://www.tidyverse.org/) workflow. 

The "water" class is the foundation of the package; it provides a mechanism for linking models in any order while maintaining water quality information.
To create a water, start with the `define_water` function. After the water has been defined, you can apply other models linked together with
the pipe operator "%>%" or in separate lines. To access a water quality parameter in a water, use the @ operator. For example,
water@ph returns the pH value. All water slots are defined in lowercase. To view slots in the water class, use `define_water` with water quality inputs (i.e. `define_water(8, 25, 100, 150, 100)`)
and print it in the console. 

The package also includes several functions that do not use waters as the inputs and outputs.
The `solve`... functions include a water in the input and a output numeric result. Several convenience functions, such as `convert_units`
are also implemented without the water class.

## Installation

You can install the development version of tidywater from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BrownandCaldwell-Public/tidywater")
```

## Example

There are two primary ways to apply tidywater functions: to a single water or to a dataframe of water quality

### Single water

Functions can be applied to a water one at a time in their base form. Note that all function argument names are lowercase.

``` r
library(tidywater)

# Create water
raw_water <- define_water(ph = 7, temp = 25, alk = 100, tot_hard = 100, ca_hard = 80, na = 100, k = 10, cl = 50, so4 = 50)

# Balance ions
raw_water_balanced <- balance_ions(raw_water)

# View water
raw_water_balanced
summarize_wq(raw_water_balanced)
plot_ions(raw_water_balanced)

# Chemical dosing - acid/base equilibrium based
# Use documentation to view list of available chemicals.
coag_water <- chemdose_ph(raw_water_balanced, alum = 30, hcl = 10)

# Check individual slots in water
coag_water@ph
coag_water@alk

# Solve for target chemical dose based on pH or alkalinity setpoint

caustic1 <- solvedose_ph(coag_water, 8.8, "naoh")
caustic2 <- solvedose_alk(coag_water, 80, "naoh")

finished_water <- chemdose_ph(coag_water, naoh = caustic1)

# Blend multiple waters together

blended_water <- blend_waters(waters = c(raw_water, finished_water), ratios = c(.2, .8))

```

Functions can also be piped together as long as each function used has a water input and output. 

``` r
library(tidywater)

# Create water
raw_water <- define_water(ph = 7, temp = 25, alk = 100, tot_hard = 100, ca_hard = 80, na = 100, k = 10, cl = 50, so4 = 50) %>%
balance_ions() %>%
chemdose_ph(alum = 30, hcl = 10)

```

### Dataframe

When applying these functions, it is often valuable to generate a dataframe of modeling scenarios to apply a treatment train to.
Multiple wrapper functions have been included to make it easier to pipe functions together in a dataframe without requiring
purrr.

``` r
library(tidywater)
library(dplyr)

# Create water
# Begin with a dataframe with column names matching define_water inputs. See water_df for an example.
raw_water <- water_df %>%
cross_join(tibble(alum = seq(0,50,2))) %>% # this creates a model scenario for each alum dose and each water
define_water_chain(output_water = "raw_water") %>%
balance_ions_chain(input_water = "raw_water", output_water = "raw_water_balanced") %>%
# The function automatically finds the alum column, but you can specify additional chemicals as well.
chemdose_ph_chain(input_water = "raw_water_balanced", output_water = "coag_water", hcl = 10) %>% 
solvedose_ph_once(input_water = "coag_water", output = "naoh", 8.8, "naoh") %>%
# Remove or rename previous chemical doses to avoid applying them twice
rename(alum_coag = alum, hcl_coag = hcl) %>%
chemdose_ph_chain(input_water = "coag_water", output_water = "finished_water") %>%
# Create larger dataframe with multiple blend scenarios
cross_join(tibble(ratio_raw = c(0, .2, .5), ratio_fin = c(1, .8, .5))) %>%
blend_waters_chain(c("raw_water_balanced", "finished_water"), ratios = c("ratio_raw", "ratio_fin"))

# At any point in the process, use _once wrapper to extract final water as individual dataframe columns instead of water class.
raw_water <- water_df %>%
define_water_chain("raw_water") %>%
balance_ions_chain(input_water = "raw_water", output_water = "raw_water_balanced") %>%
chemdose_ph_once(input_water = "raw_water_balanced", alum = 30, hcl = 10)

```

## Vision

Most of the water quality models we use regularly for drinking water treatment are spread among disparate tools that require
great expertise and manual effort to piece together into a treatment process. That effort is repeated every time we want to
modify the process for a different treatment train. The manual effort also constrains the number of model scenarios that can be run
on an individual treatment train. 

Due to these barriers to access, the industry as a whole is under utilizing that vast amounts of data that are collected and
the wealth of published knowledge already available to us. We believe there is a lot of potential value in the data we collect
and the models we know can be applied to understand treatment.

Tidywater was designed to centralize all models in one accessible tool, with the objective of making modeling easier and
building on the value we can gain from in-depth data analysis and modeling. The underlying functions provide building blocks
that can be applied in a modular fashion to customize treatment trains and model scenarios. It is also intended to be a testing
ground for new modeling approaches that can eventually be incorporated into the package to contribute to our industry as a whole.

The water class and current functions were designed to be able to add other water quality parameters to the class without
breaking existing code. With this design, other models can be added to the package that utilize the water class. These functions will
automatically fit in with the existing functions.

## Roadmap

The package currently includes basic water quality and acid/based equilibrium functions.

Published models that we intend to incorporate into tidywater include:
- Edwards coagulation model
- EPA TELSS models
- Other DOC and DBP models from the EPA WTP model

If you are interested in contributing additional models, please see our [contribution guide](https://github.com/BrownandCaldwell/tidywater/tree/main/.github/CONTRIBUTING.md) to make sure your efforts can
be incorporated into the package correctly and efficiently.


## Code of Conduct

Please note that the tidywater project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## References

The following sources were used for package development. Any functions not listed below are derived from other functions.

- Acid/base equilibrium chemistry fundamentals. Used in `define_water`, `chemdose_ph`, `balance_ions`, `plot_ions`, `convert_units`,
`discons`, `blend_waters`, `calculate_hardness`.
  - Benjamin, M. M. (2015). Water chemistry. Waveland Press, Inc. 
  - Stumm, W., & Morgan, J. J. (1996). Aquatic Chemistry: Chemical equilibria and rates in natural waters. Wiley. 

- Chemical dose pH calculations adapted from the WTP model manual. Used in `chemdose_ph`.
  - Water treatment plant model version 2.0 User’s Manual. (2001). University of Colorado at Boulder. Available at: https://www.epa.gov/sites/default/files/2017-03/documents/wtp_model_v._2.0_manual_508.pdf (Accessed: 29 February 2024). 

- Molar weights from Google - `mweights`.

