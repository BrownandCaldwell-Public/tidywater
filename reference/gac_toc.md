# Calculate TOC Concentration in GAC system

Calculates TOC concentration after passing through GAC treatment
according to the model developed in "Modeling TOC Breakthrough in
Granular Activated Carbon Adsorbers" by Zachman and Summers (2010), or
the logistics curve approach in EPA WTP Model v. 2.0 Manual (2001). For
a single water use `gac_toc`; for a dataframe use `gac_toc_df`. Use
[pluck_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/pluck_water.md)
to get values from the output water as new dataframe columns. For most
arguments in the `_df` helper "use_col" default looks for a column of
the same name in the dataframe. The argument can be specified directly
in the function instead or an unquoted column name can be provided.

Water must contain DOC or TOC value.

## Usage

``` r
gac_toc(
  water,
  ebct = 10,
  model = "Zachman",
  media_size = "12x40",
  bed_vol,
  pretreat = "coag"
)

gac_toc_df(
  df,
  input_water = "defined",
  output_water = "gaced",
  model = "use_col",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  media_size = "use_col",
  ebct = "use_col",
  bed_vol = "use_col",
  pretreat = "use_col"
)
```

## Source

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

Zachman and Summers (2010)

U.S. EPA (2001)

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- ebct:

  Empty bed contact time (minutes). Model results are valid for 10 or 20
  minutes. Defaults to 10 minutes.

- model:

  Specifies which GAC TOC removal model to apply. Options are Zachman
  and WTP. Defaults to Zachman.

- media_size:

  Size of GAC filter mesh. Model includes 12x40 and 8x30 mesh sizes.
  Defaults to 12x40.

- bed_vol:

  Bed volume of GAC filter to predict effluent TOC for.

- pretreat:

  Specifies the level of pretreatment prior to GAC treatment. Defaults
  to "coag". Other option is coagulant, ozonation, and biotreatment,
  called "o3biof".

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include columns named for the media_size, ebct, and bed
  volume.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated parameters with the class,
  water. Default is "gaced".

- pluck_cols:

  Extract water slots modified by the function (doc, toc, uv254) into
  new numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`gac_toc` returns a water class object with updated DOC, TOC, and UV254
slots.

`gac_toc_df` returns a data frame containing a water class column with
updated DOC, TOC, and UV254 slots

## Details

GAC model for TOC removal

The function will calculate TOC concentration by GAC adsorption in
drinking water treatment. UV254 concentrations are predicted based on a
linear relationship with DOC from WTP Model Equation 5-93 and 5-94.

## Examples

``` r
water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5) %>%
  gac_toc(media_size = "8x30", ebct = 20, model = "Zachman", bed_vol = 15000)
#> Warning: Missing value for alkalinity. Carbonate balance will not be calculated.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

# \donttest{
example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(
    model = "WTP",
    media_size = "8x30",
    ebct = 10,
    bed_vol = rep(c(12000, 15000, 18000), 4)
  ) %>%
  gac_toc_df()

example_df <- water_df %>%
  define_water_df("raw") %>%
  dplyr::mutate(
    model = "WTP",
    bed_vol = 15000
  ) %>%
  gac_toc_df(input_water = "raw")
# }
```
