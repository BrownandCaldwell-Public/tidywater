# Calculate TOC Concentration in GAC system

Returns a data frame with a breakthrough curve based on the TOC
concentration after passing through GAC treatment, according to the
model developed in "Modeling TOC Breakthrough in Granular Activated
Carbon Adsorbers" by Zachman and Summers (2010), or the USEPA WTP Model
v. 2.0 Manual (2001).

Water must contain DOC or TOC value.

## Usage

``` r
gacrun_toc(
  water,
  ebct = 10,
  model = "Zachman",
  media_size = "12x40",
  bvs = c(2000, 20000, 100)
)

gacrun_toc_df(
  df,
  input_water = "defined",
  water_prefix = TRUE,
  ebct = "use_col",
  model = "use_col",
  media_size = "use_col",
  bvs = "use_col"
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

  Size of GAC filter mesh. If model is Zachman, can choose between 12x40
  and 8x30 mesh sizes, otherwise leave as default. Defaults to 12x40.

- bvs:

  If using WTP model, option to run the WTP model for a specific
  sequence of bed volumes, otherwise leave as default. Defaults c(2000,
  20000, 100).

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include a column named for the coagulant being dosed, and a
  column named for the set of coefficients to use.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- water_prefix:

  Append the input_water name to the start of the output columns.
  Default is TRUE.

## Value

`gacrun_toc` returns a data frame with bed volumes and breakthrough TOC
values.

`gacrun_toc_df` returns a data frame containing columns of the
breakthrough curve (breakthrough and bed volume).

## Details

GAC model for TOC removal

The function will calculate bed volumes and normalized TOC breakthrough
(TOCeff/TOCinf) given model type. Both models were developed using data
sets from bench-scale GAC treatment studies using bituminous GAC and
EBCTs of either 10 or 20 minutes. The specific mesh sizes used to
develop the Zachman and Summers model were 12x40 or 8x30. The models
were also developed using influent pH and TOC between specific ranges.
Refer to the papers included in the references for more details.

## Examples

``` r
water <- define_water(ph = 8, toc = 2.5, uv254 = .05, doc = 1.5) %>%
  gacrun_toc(media_size = "8x30", ebct = 20, model = "Zachman")
#> Warning: Missing value for alkalinity. Carbonate balance will not be calculated.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

# \donttest{
example_df <- water_df %>%
  define_water_df() %>%
  gacrun_toc_df()
# }
```
