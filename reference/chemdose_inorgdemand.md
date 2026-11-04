# Inorganic Oxidant Demand

Calculates Cl residual from inorganic scavengers present in the input
water object: Mg, Fe, and Ammonia. The output water will have updated
`free_chlorine` slot to reflect the chlorine residual after accounting
for the inorganic demand. The `mn`, `fe`, and `tot_nh3` slots will be
set to 0, as they are assumed to be fully oxidized by the chlorine dose.
Other products of the oxidation reactions will not be tracked or updated
in the output water object. For a single water use
`chemdose_inorgdemand`; to apply the model to a dataframe use
`chemdose_inorgdemand_df`.

By default (`use_chlorine_slot = FALSE`), the output `free_chlorine` is
`cl2_dose` minus the inorganic demand, and any free chlorine already in
the water is overwritten (with a warning). Set
`use_chlorine_slot = TRUE` to add the existing `free_chlorine` slot to
`cl2_dose` before the demand is subtracted. If `cl2_dose` is not
specified, the existing `free_chlorine` slot is used (with a warning
when `use_chlorine_slot = FALSE`). The function errors if the inorganic
demand is greater than the available chlorine.

The inorganic demand of ammonia is calculated assuming all ammonia is
converted to nitrogen gas, not considering the conversion of ammonia to
nitrate. For chloramine chemistry, use `chemdose_chloramine` if
interested in more precise breakpoint chlorination calculations.

## Usage

``` r
chemdose_inorgdemand(water, cl2_dose, use_chlorine_slot = FALSE)

chemdose_inorgdemand_df(
  df,
  input_water = "defined",
  output_water = "oxidized",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  keep_dose_cols = FALSE,
  cl2_dose = "use_col",
  use_chlorine_slot = "use_col"
)
```

## Arguments

- water:

  Input water object with mn, fe, and tot_nh3 slots defined. The
  free_chlorine slot must also be defined when the slot is used
  (`use_chlorine_slot = TRUE` or `cl2_dose` not specified).

- cl2_dose:

  Chlorine dose in mg/L Cl2.

- use_chlorine_slot:

  Defaults to FALSE. When TRUE, the existing free_chlorine slot in the
  water is added to `cl2_dose`. If `cl2_dose` is not specified, only the
  chlorine slot will be used, regardless of use_chlorine_slot.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).
  The df may include columns for the chlorine dose (cl2_dose) and
  use_chlorine_slot.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "oxidized".

- pluck_cols:

  Extract the free_chlorine slot into a new numeric column for easy
  access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

- keep_dose_cols:

  Option to keep the original chemical dose columns in the output data
  frame. Default is FALSE.

## Value

`chemdose_inorgdemand` returns a water class object with updated
free_chlorine slot that reflects the chlorine residual.

`chemdose_inorgdemand_df` returns a data frame containing a water class
column with updated free_chlorine, mn, fe, and tot_nh3 slots.
Optionally, it also adds a column for free_chlorine.

## Examples

``` r

water1<-define_water(ph = 7, alk = 70, mn = 1, fe = 2, tot_nh3 = 1)
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
cl_residual <- chemdose_inorgdemand(water1, 20)

# Use the existing free chlorine in the water
water2 <- define_water(ph = 7, alk = 70, mn = 1, fe = 2, tot_nh3 = 1, free_chlorine = 12)
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
cl_residual <- chemdose_inorgdemand(water2, use_chlorine_slot = TRUE)

# \donttest{
example_df <- water_df %>%
  dplyr::mutate(mn = 20, fe = 0.5, tot_nh3 = 0.2) %>%
  define_water_df() %>%
  chemdose_inorgdemand_df(cl2_dose = 5, pluck_cols = TRUE)
#> Warning: Existing free_chlorine slot will be overridden based on recent dose. To sum
#> results instead, set `use_chlorine_slot = TRUE`. (6 rows affected.)

example_df <- water_df %>%
  dplyr::mutate(mn = 20, fe = 0.5, tot_nh3 = 0.2) %>%
  define_water_df() %>%
  dplyr::mutate(ClDose = seq(3, 14, 1)) %>%
  chemdose_inorgdemand_df(cl2_dose = ClDose, output_water = "chlorinated")
#> Warning: Existing free_chlorine slot will be overridden based on recent dose. To sum
#> results instead, set `use_chlorine_slot = TRUE`. (6 rows affected.)

# Use the free_chlorine already in each water instead of a dose
example_df <- water_df %>%
  dplyr::mutate(mn = 20, fe = 0.5, tot_nh3 = 0.2, free_chlorine = 4) %>%
  define_water_df() %>%
  chemdose_inorgdemand_df(use_chlorine_slot = TRUE, pluck_cols = TRUE)
# }
```
