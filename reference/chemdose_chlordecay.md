# Calculate chlorine decay

calculates the decay of chlorine or chloramine based on the U.S. EPA's
Water Treatment Plant Model (U.S. EPA, 2001). For a single water use
`chemdose_chlordecay`; for a dataframe use `chemdose_chlordecay_df`. Use
`pluck_cols = TRUE` to get values from the output water as new dataframe
columns. For most arguments in the `_df` helper "use_col" default looks
for a column of the same name in the dataframe. The argument can be
specified directly in the function instead or an unquoted column name
can be provided.

## Usage

``` r
chemdose_chlordecay(
  water,
  cl2_dose,
  time,
  treatment = "raw",
  cl_type = "chlorine",
  use_chlorine_slot = FALSE
)

chemdose_chlordecay_df(
  df,
  input_water = "defined",
  output_water = "disinfected",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  cl2_dose = "use_col",
  time = "use_col",
  treatment = "use_col",
  cl_type = "use_col",
  use_chlorine_slot = "use_col"
)
```

## Source

U.S. EPA (2001)

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- cl2_dose:

  Applied chlorine or chloramine dose (mg/L as cl2). Model results are
  valid for doses between 0.995 and 41.7 mg/L for raw water, and for
  doses between 1.11 and 24.7 mg/L for coagulated water.

- time:

  Reaction time (hours). Chlorine decay model results are valid for
  reaction times between 0.25 and 120 hours.Chloramine decay model does
  not have specified boundary conditions.

- treatment:

  Type of treatment applied to the water. Options include "raw" for no
  treatment (default), "coag" for water that has been coagulated or
  softened.

- cl_type:

  Type of chlorination applied, either "chlorine" (default) or
  "chloramine".

- use_chlorine_slot:

  Defaults to FALSE. When TRUE, uses either free_chlorine or
  combined_chlorine slot in water (depending on cl_type). If 'cl2_dose'
  argument, not specified, chlorine slot will be used. If 'cl2_dose'
  specified and use_chlorine_slot is TRUE, all chlorine will be summed.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include a column named for the applied chlorine dose (cl2),
  and a column for time in hours.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "disinfected".

- pluck_cols:

  Extract water slots modified by the function (free_chlorine,
  combined_chlorine) into new numeric columns for easy access. Default
  to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`chemdose_chlordecay` returns an updated disinfectant residual in the
free_chlorine or combined_chlorine water slot in units of M. Use
[convert_units](https://BrownandCaldwell-Public.github.io/tidywater/reference/convert_units.md)
to convert to mg/L.

`chemdose_chlordecay_df` returns a data frame containing a water class
column with updated free_chlorine or combined_chlorine residuals.
Optionally, it also adds columns for each of those slots individually.

## Details

Required arguments include an object of class "water" created by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md),
applied chlorine/chloramine dose, type, reaction time, and treatment
applied (options include "raw" for no treatment, or "coag" for
coagulated water). The function also requires additional water quality
parameters defined in
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
including TOC and UV254. The output is a new "water" class with the
calculated total chlorine value stored in the 'free_chlorine' or
'combined_chlorine' slot, depending on what type of chlorine is dosed.
When modeling residual concentrations through a unit process, the U.S.
EPA Water Treatment Plant Model applies a correction factor based on the
influent and effluent residual concentrations (see U.S. EPA (2001)
equation 5-118) that may need to be applied manually by the user based
on the output.

## Examples

``` r
example_cl2 <- define_water(8, 20, 66, toc = 4, uv254 = 0.2) %>%
  chemdose_chlordecay(cl2_dose = 2, time = 8)
#> Warning: Missing value for DOC. Default value of 95% of TOC will be used.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

example_cl2 <- define_water(8, 20, 66, toc = 4, uv254 = 0.2, free_chlorine = 3) %>%
  chemdose_chlordecay(cl2_dose = 2, time = 8, use_chlorine_slot = TRUE)
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Missing value for DOC. Default value of 95% of TOC will be used.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

# \donttest{

example_df <- water_df %>%
  dplyr::mutate(br = 50) %>%
  define_water_df() %>%
  chemdose_chlordecay_df(input_water = "defined", cl2_dose = 4, time = 8)
#> Warning: Existing 'free_chlorine' slot will be overridden based on recent dose. To sum results instead, set 'use_chlorine_slot = TRUE'.
#> Warning: Existing 'free_chlorine' slot will be overridden based on recent dose. To sum results instead, set 'use_chlorine_slot = TRUE'.
#> Warning: Existing 'free_chlorine' slot will be overridden based on recent dose. To sum results instead, set 'use_chlorine_slot = TRUE'.
#> Warning: Existing 'free_chlorine' slot will be overridden based on recent dose. To sum results instead, set 'use_chlorine_slot = TRUE'.
#> Warning: Existing 'free_chlorine' slot will be overridden based on recent dose. To sum results instead, set 'use_chlorine_slot = TRUE'.
#> Warning: Existing 'free_chlorine' slot will be overridden based on recent dose. To sum results instead, set 'use_chlorine_slot = TRUE'.

example_df <- water_df %>%
  dplyr::mutate(
    br = 50,
    free_chlorine = 2
  ) %>%
  define_water_df() %>%
  dplyr::mutate(
    cl2_dose = seq(2, 24, 2),
    ClTime = 30
  ) %>%
  chemdose_chlordecay_df(
    time = ClTime,
    use_chlorine_slot = TRUE,
    treatment = "coag",
    cl_type = "chloramine",
    pluck_cols = TRUE
  )
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
#> Warning: Chlorine dose was summed with residual chlorine in the water object. If this is not intended, either do not specify 'cl_dose' or use 'use_chlorine_slot = FALSE'.
# }
```
