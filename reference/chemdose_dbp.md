# Calculate DBP formation

Calculates disinfection byproduct (DBP) formation based on the U.S.
EPA's Water Treatment Plant Model (U.S. EPA, 2001). Required arguments
include an object of class "water" created by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
chlorine dose, type, reaction time, and treatment applied (if any). The
function also requires additional water quality parameters defined in
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
including bromide, TOC, UV254, temperature, and pH.

For a single water use `chemdose_dbp`; for a dataframe use
`chemdose_dbp_df`. Use `pluck_cols = TRUE` to get values from the output
water as new dataframe columns. For most arguments in the `_df` helper
"use_col" default looks for a column of the same name in the dataframe.
The argument can be specified directly in the function instead or an
unquoted column name can be provided.

## Usage

``` r
chemdose_dbp(
  water,
  cl2,
  time,
  treatment = "raw",
  cl_type = "chorine",
  location = "plant",
  correction = TRUE,
  coeff = NULL
)

chemdose_dbp_df(
  df,
  input_water = "defined",
  output_water = "disinfected",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  cl2 = "use_col",
  time = "use_col",
  treatment = "use_col",
  cl_type = "use_col",
  location = "use_col",
  correction = TRUE,
  coeff = NULL
)
```

## Source

TTHMs, raw: U.S. EPA (2001) equation 5-131

HAAs, raw: U.S. EPA (2001) equation 5-134

TTHMs, treated: U.S. EPA (2001) equation 5-139

HAAs, treated: U.S. EPA (2001) equation 5-142

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- cl2:

  Applied chlorine dose (mg/L as Cl2). Model results are valid for doses
  between 1.51 and 33.55 mg/L.

- time:

  Reaction time (hours). Model results are valid for reaction times
  between 2 and 168 hours.

- treatment:

  Type of treatment applied to the water. Options include "raw" for no
  treatment (default), "coag" for water that has been coagulated or
  softened, and "gac" for water that has been treated by granular
  activated carbon (GAC). GAC treatment has also been used for
  estimating formation after membrane treatment with good results.

- cl_type:

  Type of chlorination applied, either "chlorine" (default) or
  "chloramine".

- location:

  Location for DBP formation, either in the "plant" (default), or in the
  distributions system, "ds".

- correction:

  Model calculations are adjusted based on location and cl_type. Default
  value is TRUE.

- coeff:

  Optional input to specify custom coefficients to the dbp model. Must
  be a data frame with the following columns: ID, and the corresponding
  coefficients A, a, b, c, d, e, f, and ph_const for each dbp of
  interest. Default value is NULL.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).
  The df may include columns for the other function arguments.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "disinfected".

- pluck_cols:

  Extract primary water slots modified by the function (tthm, haa5) into
  new numeric columns for easy access with TRUE. Alternatively, specify
  "all" to get all DBP species in addition: (tthm, chcl3, chcl2br,
  chbr2cl, chbr3, haa5, mcaa, dcaa, tcaa, mbaa, dbaa) Defaults to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`chemdose_dbp` returns a single water class object with predicted DBP
concentrations.

`chemdose_dbp_df` returns a data frame containing a water class column
with updated tthm, chcl3, chcl2br, chbr2cl, chbr3, haa5, mcaa, dcaa,
tcaa, mbaa, dbaa concentrations. Optionally, it also adds columns for
those slots individually.

## Details

The function will calculate haloacetic acids (HAA) as HAA5, and total
trihalomethanes (TTHM). Use `summarize_wq(water, params = c("dbps"))` to
quickly tabulate the results.

## Examples

``` r
example_dbp <- define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
  chemdose_dbp(cl2 = 2, time = 8)
#> Warning: Missing value for DOC. Default value of 95% of TOC will be used.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
example_dbp <- define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
  chemdose_dbp(cl2 = 3, time = 168, treatment = "coag", location = "ds")
#> Warning: Missing value for DOC. Default value of 95% of TOC will be used.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

# \donttest{
example_df <- water_df %>%
  dplyr::mutate(br = 50) %>%
  define_water_df() %>%
  chemdose_dbp_df(input_water = "defined", cl2 = 4, time = 8)

example_df <- water_df %>%
  dplyr::mutate(br = 50) %>%
  dplyr::slice_sample(n = 3) %>%
  define_water_df() %>%
  dplyr::mutate(
    cl2_dose = c(2, 3, 4),
    time = 30
  ) %>%
  chemdose_dbp_df(
    cl2 = cl2_dose, treatment = "coag", location = "ds",
    cl_type = "chloramine", pluck_cols = TRUE
  )
#> Warning: Temperature is outside the model bounds of temp=20 Celsius for coagulated water.
#> Warning: pH is outside the model bounds of pH = 7.5 for coagulated water
#> Warning: pH is outside the model bounds of pH = 7.5 for coagulated water
#> Warning: Temperature is outside the model bounds of temp=20 Celsius for coagulated water.
#> Warning: pH is outside the model bounds of pH = 7.5 for coagulated water
# }
```
