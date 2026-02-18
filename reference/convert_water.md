# Convert `water` class object to a dataframe

This converts a `water` class to a dataframe with individual columns for
each slot (water quality parameter) in the `water`. This is useful for
one-off checks. For typical applications, use `pluck_cols = TRUE` in any
`_df` function or `pluck_water` to choose relevant slots.

Use convert_water to keep all slots in the same units as the water.

Use convert_watermg to convert to more typical units. Converts the
following slots from M to mg/L: na, ca, mg, k, cl, so4, hco3, co3,
h2po4, hpo4, po4, ocl, bro3, f, fe, al. Converts these slots to ug/L:
br, mn. All other values remain unchanged.

## Usage

``` r
convert_water(water)

convert_watermg(water)
```

## Arguments

- water:

  A water class object

## Value

A data frame containing columns for all non-NA water slots.

A data frame containing columns for all non-NA water slots with ions in
mg/L.

## Examples

``` r
# Generates 1 row dataframe
example_df <- define_water(ph = 7, temp = 20, alk = 100) %>%
  convert_water()
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(to_dataframe = purrr::map(defined, convert_water)) %>%
  tidyr::unnest(to_dataframe) %>%
  dplyr::select(-defined)

water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1) %>%
  convert_watermg()
#> Warning: User entered total hardness is >10% different than calculated hardness.
```
