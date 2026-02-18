# Calculate maximum bed volumes to stay below target DOC

Calculates GAC filter bed volumes to achieve target effluent DOC
according to the model developed in "Modeling TOC Breakthrough in
Granular Activated Carbon Adsorbers" by Zachman and Summers (2010), or
the USEPA WTP Model v. 2.0 Manual (2001). For a single water use
`gacbv_toc`; for a dataframe use `gacbv_toc_df`. For most arguments in
the `_df` helper "use_col" default looks for a column of the same name
in the dataframe. The argument can be specified directly in the function
instead or an unquoted column name can be provided.

Water must contain DOC or TOC value.

## Usage

``` r
gacbv_toc(
  water,
  ebct = 10,
  model = "Zachman",
  media_size = "12x40",
  target_doc
)

gacbv_toc_df(
  df,
  input_water = "defined",
  model = "use_col",
  media_size = "use_col",
  ebct = "use_col",
  target_doc = "use_col",
  water_prefix = TRUE
)
```

## Source

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

Zachman and Summers (2010)

USEPA (2001)

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- ebct:

  Empty bed contact time (minutes). Model results are valid for 10 or 20
  minutes. Default is 10 minutes.

- model:

  Specifies which GAC TOC removal model to apply. Options are Zachman
  and WTP.

- media_size:

  Size of GAC filter mesh. Model includes 12x40 and 8x30 mesh sizes.
  Default is 12x40.

- target_doc:

  Optional input to set a target DOC concentration and calculate
  necessary bed volume

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md)
  The df may include columns named for the chemical(s) being dosed.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`gacbv_toc` returns a data frame of bed volumes that achieve the target
DOC.

`gacbv_toc_df` returns a data frame with columns for bed volumes.

## Details

GAC model for TOC removal

The function will calculate bed volume required to achieve given target
DOC values.

## Examples

``` r
water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5)
#> Warning: Missing value for alkalinity. Carbonate balance will not be calculated.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
bed_volume <- gacbv_toc(water, media_size = "8x30", ebct = 20, model = "Zachman", target_doc = 0.8)

# \donttest{
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(
    model = "WTP",
    media_size = "8x30",
    ebct = 10,
    target_doc = rep(c(0.5, 0.8, 1), 4)
  ) %>%
  gacbv_toc_df()
# }
```
