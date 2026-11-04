# Convert `water` class object to a dataframe

This converts a `water` class to a dataframe with individual columns for
each slot (water quality parameter) in the `water`. This is useful for
one-off checks. For typical applications, use `pluck_cols = TRUE` in any
`_df` function or `pluck_water` to choose relevant slots.

Use convert_water to keep all slots in the same units as the water.

Use convert_watermg to convert to more typical units. Converts all
non-NA water slots with ions to mg/L, with the exception of br and mn,
which are converted to ug/L. Total silicate is converted to mg/L as
SiO2, and citrate species are converted to mg/L as citric acid, matching
the
[define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)
inputs. All other values remain unchanged.

The `ks` slot (a data frame of corrected equilibrium constants) is not
included in the output. Access it directly with `water@ks`.

## Usage

``` r
convert_water(water)

convert_watermg(water)
```

## Arguments

- water:

  A water class object

## Value

A data frame containing columns for all non-NA water slots, except `ks`.

A data frame containing columns for all non-NA water slots with ions in
mg/L.

## Examples

``` r

# Generates 1 row dataframe
example_df <- define_water(ph = 7, temp = 20, alk = 100) %>%
  convert_water()
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(to_dataframe = purrr::map(defined, convert_water)) %>%
  tidyr::unnest(to_dataframe) %>%
  dplyr::select(-defined)

water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1) %>%
  convert_watermg()
```
