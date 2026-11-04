# Calculate chlorine and chloramine Concentrations with the breakpoint cblorination approach

chemdose_chloramine, adopted from the U.S. EPA's Chlorine Breakpoint
Curve Simulator, calculates chlorine and chloramine concentrations based
on the two papers Jafvert & Valentine (Environ. Sci. Technol., 1992, 26
(3), pp 577-586) and Vikesland et al. (Water Res., 2001, 35 (7), pp
1766-1776). Required arguments include an object of class "water"
created by
[define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md),
chlorine dose, and reaction time. The function also requires additional
water quality parameters defined in
[define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)
including temperature, pH, and alkalinity. For a single water use
`chemdose_chloramine`; for a dataframe use `chemdose_chloramine_df`. Use
`pluck_cols = TRUE` to get values from the output water as new dataframe
columns. For most arguments in the `_df` helper "use_col" default looks
for a column of the same name in the dataframe. The argument can be
specified directly in the function instead or an unquoted column name
can be provided.

## Usage

``` r
chemdose_chloramine(
  water,
  time,
  cl2 = 0,
  nh3 = 0,
  use_free_cl_slot = FALSE,
  use_tot_nh3_slot = FALSE
)

chemdose_chloramine_df(
  df,
  input_water = "defined",
  output_water = "chloraminated",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  keep_dose_cols = FALSE,
  time = "use_col",
  cl2 = "use_col",
  nh3 = "use_col",
  use_free_cl_slot = "use_col",
  use_tot_nh3_slot = "use_col"
)
```

## Source

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)

- time:

  Reaction time (minutes). Time defined needs to be greater or equal to
  1 minute.

- cl2:

  Applied chlorine dose (mg/L as Cl2), defaults to 0.If not specified,
  use free_chlorine slot in water.

- nh3:

  Applied ammonia dose (mg/L as N), defaults to 0. If not specified, use
  tot_nh3 slot in water.

- use_free_cl_slot:

  Defaults to FALSE. If TRUE, uses free_chlorine slot in water. If TRUE
  AND there is a cl2 input, both the free_chlorine water slot and
  chlorine dose will be used.

- use_tot_nh3_slot:

  Defaults to FALSE. If TRUE, uses tot_nh3 slot in water. If TRUE AND
  there is a nh3 input, both the tot_nh3 water slot and ammonia dose
  will be used.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).
  The df may include a column named for the applied chlorine dose
  (cl2_dose), and a column for time in hours.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "chloraminated".

- pluck_cols:

  Extract water slots modified by the function ("free_chlorine",
  "nh2cl", "nhcl2", "ncl3", "combined_chlorine", "tot_nh3") into new
  numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

- keep_dose_cols:

  Option to keep the original chemical dose columns in the output data
  frame. Default is FALSE.

## Value

`chemdose_chloramine` returns a water class object with predicted
chlorine and chloramine concentrations.

`chemdose_chloramine_df` returns a data frame containing water class
column with updated chlorine/chloramine slots: free_chlorine, nh2cl,
nhcl2, ncl3, combined_chlorine, tot_nh3. Optionally, it also adds
columns for each of those slots individually.

## Examples

``` r
breakpoint <- define_water(7.5, 20, 65, free_chlorine = 5, tot_nh3 = 1) %>%
  chemdose_chloramine(time = 40, cl2 = 2, nh3 = 1, use_free_cl_slot = TRUE)
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
#> Warning: Both `cl2` dose and free_chlorine slot (7.05158942825713e-05 mol/L) were used.
#> ℹ To use ONLY the dose, set `use_free_cl_slot = FALSE`.
#> ℹ To use ONLY the water slot, remove `cl2`.
#> Warning: Ammonia dose was used; tot_nh3 slot (7.13944041066061e-05 mol/L) was ignored.
#> ℹ To use ONLY the water slot, set `use_tot_nh3_slot = TRUE` and remove `nh3`.
#> ℹ To use BOTH, set `use_tot_nh3_slot = TRUE`.

# \donttest{
breakpoint <- water_df %>%
  dplyr::mutate(free_chlorine = 5, tot_nh3 = 1) %>%
  define_water_df() %>%
  dplyr::mutate(
    time = 8,
    cl2dose = rep(c(2, 3, 4), 4)
  ) %>%
  chemdose_chloramine_df(
    output_water = "final",
    cl2 = cl2dose,
    use_free_cl_slot = TRUE,
    use_tot_nh3_slot = TRUE,
    pluck_cols = TRUE
  )
#> Warning: Both `cl2` dose and free_chlorine slot (7.05158942825713e-05 mol/L) were used.
#> ℹ To use ONLY the dose, set `use_free_cl_slot = FALSE`. ℹ To use ONLY the water
#> slot, remove `cl2`. (12 rows affected.)
# }
```
