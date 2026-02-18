# Calculate DOC Concentration in PAC system

Calculates DOC concentration multiple linear regression model found in
2-METHYLISOBORNEOL AND NATURAL ORGANIC MATTER ADSORPTION BY POWDERED
ACTIVATED CARBON by HYUKJIN CHO (2007). Assumes all particulate TOC is
removed when PAC is removed; therefore TOC = DOC in output. For a single
water use `pac_toc`; for a dataframe use `pac_toc_df`. Use
`pluck_cols = TRUE` to get values from the output water as new dataframe
columns. For most arguments in the `_df` helper "use_col" default looks
for a column of the same name in the dataframe. The argument can be
specified directly in the function instead or an unquoted column name
can be provided.

water must contain DOC or TOC value.

## Usage

``` r
pac_toc(water, dose, time, type = "bituminous")

pac_toc_df(
  df,
  input_water = "defined",
  output_water = "paced",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  dose = "use_col",
  time = "use_col",
  type = "use_col"
)
```

## Source

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

CHO(2007)

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- dose:

  Applied PAC dose (mg/L). Model results are valid for doses
  concentrations between 5 and 30 mg/L.

- time:

  Contact time (minutes). Model results are valid for reaction times
  between 10 and 1440 minutes

- type:

  Type of PAC applied, either "bituminous", "lignite", "wood".

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include columns named for the dose, time, and type

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "paced". Pronouced P.A.ceed (not ideal we know).

- pluck_cols:

  Extract water slots modified by the function (doc, toc, uv254) into
  new numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`pac_toc` returns a water class object with updated DOC, TOC, and UV254
slots.

`pac_toc_df` returns a data frame containing a water class column with
updated DOC, TOC, and UV254 concentrations. Optionally, it also adds
columns for each of those slots individually.

## Details

The function will calculate DOC concentration by PAC adsorption in
drinking water treatment. UV254 concentrations are predicted based on a
linear relationship with DOC.

## Examples

``` r
water <- define_water(toc = 2.5, uv254 = .05, doc = 1.5) %>%
  pac_toc(dose = 15, time = 50, type = "wood")
#> Warning: Missing value for pH. Carbonate balance will not be calculated.
#> Warning: Missing value for alkalinity. Carbonate balance will not be calculated.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.


example_df <- water_df %>%
  define_water_df("raw") %>%
  dplyr::mutate(dose = seq(11, 22, 1), PACTime = 30) %>%
  pac_toc_df(input_water = "raw", time = PACTime, type = "wood", pluck_cols = TRUE)
```
