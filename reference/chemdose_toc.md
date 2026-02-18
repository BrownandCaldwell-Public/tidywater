# Determine TOC removal from coagulation

This function applies the Edwards (1997) model to a water created by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
to determine coagulated DOC. Model assumes all particulate TOC is
removed; therefore TOC = DOC in output. Coagulated UVA is from U.S. EPA
(2001) equation 5-80. Note that the models rely on pH of coagulation. If
only raw water pH is known, utilize
[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md)
first. For a single water use `chemdose_toc`; for a dataframe use
`chemdose_toc_df`. Use `pluck_cols = TRUE` to get values from the output
water as new dataframe columns. For most arguments in the `_df` helper
"use_col" default looks for a column of the same name in the dataframe.
The argument can be specified directly in the function instead or an
unquoted column name can be provided.

## Usage

``` r
chemdose_toc(
  water,
  alum = 0,
  ferricchloride = 0,
  ferricsulfate = 0,
  coeff = "Alum",
  caoh2 = 0
)

chemdose_toc_df(
  df,
  input_water = "defined",
  output_water = "coagulated",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  alum = "use_col",
  ferricchloride = "use_col",
  ferricsulfate = "use_col",
  caoh2 = "use_col",
  coeff = "use_col"
)
```

## Source

Edwards (1997)

U.S. EPA (2001)

See reference list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).
  Water must include ph, doc, and uv254

- alum:

  Amount of hydrated aluminum sulfate added in mg/L: Al2(SO4)3\*14H2O +
  6HCO3 -\> 2Al(OH)3(am) +3SO4 + 14H2O + 6CO2

- ferricchloride:

  Amount of ferric chloride added in mg/L: FeCl3 + 3HCO3 -\>
  Fe(OH)3(am) + 3Cl + 3CO2

- ferricsulfate:

  Amount of ferric sulfate added in mg/L: Fe2(SO4)3\*8.8H2O + 6HCO3 -\>
  2Fe(OH)3(am) + 3SO4 + 8.8H2O + 6CO2

- coeff:

  String specifying the Edwards coefficients to be used from "Alum",
  "Ferric", "General Alum", "General Ferric", or "Low DOC" or data frame
  of coefficients, which must include: k1, k2, x1, x2, x3, b

- caoh2:

  Option to add caoh2 in mg/L to soften the water. Will predict DOC,
  TOC, UV254 using a modified equation (see reference list). Defaults to
  zero.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include a column named for the coagulant being dosed, and a
  column named for the set of coefficients to use.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "coagulated".

- pluck_cols:

  Extract water slots modified by the function (doc, toc, uv254) into
  new numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`chemdose_toc` returns a single water class object with an updated DOC,
TOC, and UV254 concentration.

`chemdose_toc_df` returns a data frame containing a water class column
with updated DOC, TOC, and UV254 concentrations. Optionally, it also
adds columns for each of those slots individually.

## See also

[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md)

## Examples

``` r
water <- define_water(ph = 7, temp = 25, alk = 100, toc = 3.7, doc = 3.5, uv254 = .1)
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
dosed_water <- chemdose_ph(water, alum = 30) %>%
  chemdose_toc(alum = 30, coeff = "Alum")
#> Warning: Sulfate-containing chemical dosed, but so4 water slot is NA. Slot not updated because background so4 unknown.

dosed_water <- chemdose_ph(water, alum = 10, h2so4 = 10) %>%
  chemdose_toc(alum = 10, coeff = data.frame(
    x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068
  ))
#> Warning: Sulfate-containing chemical dosed, but so4 water slot is NA. Slot not updated because background so4 unknown.

# \donttest{
example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(FerricDose = seq(1, 12, 1)) %>%
  chemdose_toc_df(ferricchloride = FerricDose, coeff = "Ferric")

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(ferricchloride = seq(1, 12, 1)) %>%
  chemdose_toc_df(coeff = "Ferric", pluck_cols = TRUE)
# }
```
