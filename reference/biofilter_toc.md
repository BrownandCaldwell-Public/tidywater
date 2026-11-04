# Determine TOC removal from biofiltration using Terry & Summers, Peterson & Summers, or Terry et al models

This function applies the Terry model to a water created by
[define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)
to determine biofiltered DOC (mg/L). The Terry & Summers (2018) model
does not adjust for ozone dose but assumes a constant biodegradable
fraction based on whether or not the water was ozonated. When the reuse
argument is TRUE, the Peterson and Summers (2021) model is used to
determine biofiltered TOC (mg/L). The Peterson & Summers (2021) model
does not adjust for temperature impacts due to a lack of collected
temperature data in the metanalysis literature. Peterson & Summers
(2021) is also a TOC-basis model, so the output water object's BDOC slot
will not be updated. If a lab-measured ATP concentration is known, the
Terry et al. (2024) model can be used to determine biofiltered DOC
(mg/L) for both drinking water and reuse. The Terry et al. (2024) model
has similar BDOC assumptions as Terry & Summers (2018), but the model
ATP value can be adjusted for biofilter water temperature. For all three
models, all particulate TOC is assumed to be removed so TOC = DOC. For a
single water use `biofilter_toc`; for a dataframe use
`biofilter_toc_df`. Use `pluck_cols = TRUE` to get values from the
output water as new dataframe columns. For most arguments in the `_df`
helper "use_col" default looks for a column of the same name in the
dataframe. The argument can be specified directly in the function
instead or an unquoted column name can be provided.

## Usage

``` r
biofilter_toc(
  water,
  ebct,
  ozonated = TRUE,
  ozone_dose = 0,
  reuse = FALSE,
  media_type = "anthracite",
  atp = 0
)

biofilter_toc_df(
  df,
  input_water = "defined",
  output_water = "biofiltered",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  ebct = "use_col",
  ozonated = "use_col",
  ozone_dose = "use_col",
  reuse = "use_col",
  media_type = "use_col",
  atp = "use_col"
)
```

## Source

Terry and Summers 2018

Peterson and Summers 2021

Terry et al. 2024

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).

- ebct:

  The empty bed contact time (min) used for the biofilter.

- ozonated:

  Logical; TRUE if the water is ozonated (default), FALSE otherwise.
  Used by the Terry & Summers (2018) model and to select the BDOC
  fraction for reuse waters in the Terry et al. (2024) ATP model.
  Ignored by the Peterson & Summers (2021) model, which uses
  `ozone_dose` instead.

- ozone_dose:

  The ozone dose (mg/L) applied to the water. Only used if
  `reuse = TRUE`.

- reuse:

  Logical; TRUE if the water is effluent from a reuse treatment process,
  FALSE otherwise. If TRUE, the Peterson and Summers (2021) model is
  used to determine TOC removal. If FALSE, the Terry and Summers (2018)
  model is used to determine BDOC removal.

- media_type:

  The type of media used in the biofilter. Default is "anthracite",
  other options are "gac" and "sand". Only used if `reuse = TRUE`.

- atp:

  The measured adenosine triphosphate (ATP) concentration at the top of
  the biofilter (ng/mL at 22 degrees Celcius). Optional argument that
  will use Terry et al. (2024) model for both drinking water and reuse
  if nonzero value is provided. (This will also ignore ozone dose and
  media type for reuse models.) Defaults to 0.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).
  The df may include a column indicating the EBCT or whether the water
  is ozonated.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "biofiltered".

- pluck_cols:

  Extract water slots modified by the function (doc, toc, bdoc) into new
  numeric columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`biofilter_toc` returns water class object with modeled DOC removal from
biofiltration.

`biofilter_toc_df` returns a data frame containing a water class column
with updated DOC, TOC, and BDOC concentrations. Optionally, it also adds
columns for each of those slots individually.

## Examples

``` r
water <- define_water(ph = 7, temp = 25, alk = 100, toc = 5.0, doc = 4.0, uv254 = .1) %>%
  biofilter_toc(ebct = 10, ozonated = FALSE)
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.


example_df <- water_df %>%
  define_water_df() %>%
  biofilter_toc_df(input_water = "defined", ebct = c(10, 15), ozonated = FALSE)

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(
    BiofEBCT = c(10, 10, 10, 15, 15, 15, 20, 20, 20, 25, 25, 25),
    ozonated = c(rep(TRUE, 6), rep(FALSE, 6))
  ) %>%
  biofilter_toc_df(input_water = "defined", ebct = BiofEBCT)
```
