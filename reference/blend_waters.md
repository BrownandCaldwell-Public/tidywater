# Determine blended water quality from multiple waters based on mass balance and acid/base equilibrium

This function takes a vector of waters defined by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and a vector of ratios and outputs a new water object with updated ions
and pH. For a single blend use `blend_waters`; for a dataframe use
`blend_waters_df`. Use
[pluck_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/pluck_water.md)
to get values from the output water as new dataframe columns.

## Usage

``` r
blend_waters(waters, ratios)

blend_waters_df(df, waters, ratios, output_water = "blended")
```

## Arguments

- waters:

  Vector of source waters created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).
  For `df` function, this can include quoted column names and/or
  existing single water objects unquoted.

- ratios:

  Vector of ratios in the same order as waters. (Blend ratios must sum
  to 1). For `df` function, this can also be a list of quoted column
  names.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md)

- output_water:

  name of output column storing updated parameters with the class,
  water. Default is "blended_water".

## Value

`blend_waters` returns a water class object with blended water quality
parameters.

`blend_waters_df` returns a data frame with a water class column
containing blended water quality

## See also

[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

## Examples

``` r
water1 <- define_water(7, 20, 50)
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
water2 <- define_water(7.5, 20, 100, tot_nh3 = 2)
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
blend_waters(c(water1, water2), c(.4, .6))
#> pH (unitless):  7.31 
#> Temperature (deg C):  20 
#> Alkalinity (mg/L CaCO3):  80 
#> Use summary functions or slot names to view other parameters.


example_df <- water_df %>%
  dplyr::slice_head(n = 3) %>%
  define_water_df() %>%
  chemdose_ph_df(naoh = 22) %>%
  dplyr::mutate(
    ratios1 = .4,
    ratios2 = .6
  ) %>%
  blend_waters_df(
    waters = c("defined", "dosed_chem"),
    ratios = c("ratios1", "ratios2"), output_water = "Blending_after_chemicals"
  )

# \donttest{
waterA <- define_water(7, 20, 100, tds = 100)
example_df <- water_df %>%
  dplyr::slice_head(n = 3) %>%
  define_water_df() %>%
  blend_waters_df(waters = c("defined", waterA), ratios = c(.8, .2))
#> Warning: The following parameters are missing in some of the waters and will be set to NA in the blend:
#>    tot_hard, toc, doc, uv254, na, ca, mg, k, cl, so4
#> To fix this, make sure all waters provided have the same parameters specified.
#> Warning: The following parameters are missing in some of the waters and will be set to NA in the blend:
#>    tot_hard, toc, doc, uv254, na, ca, mg, k, cl, so4
#> To fix this, make sure all waters provided have the same parameters specified.
#> Warning: The following parameters are missing in some of the waters and will be set to NA in the blend:
#>    tot_hard, toc, doc, uv254, na, ca, mg, k, cl, so4
#> To fix this, make sure all waters provided have the same parameters specified.
# }
```
