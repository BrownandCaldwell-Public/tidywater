# Determine TOC removal from biofiltration using Terry & Summers BDOC model

This function applies the Terry model to a water created by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
to determine biofiltered DOC (mg/L). All particulate TOC is assumed to
be removed so TOC = DOC. For a single water use `biofilter_toc`; for a
dataframe use `biofilter_toc_df`. Use `pluck_cols = TRUE` to get values
from the output water as new dataframe columns. For most arguments in
the `_df` helper "use_col" default looks for a column of the same name
in the dataframe. The argument can be specified directly in the function
instead or an unquoted column name can be provided.

## Usage

``` r
biofilter_toc(water, ebct, ozonated = TRUE)

biofilter_toc_df(
  df,
  input_water = "defined",
  output_water = "biofiltered",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  ebct = "use_col",
  ozonated = "use_col"
)
```

## Source

Terry and Summers 2018

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).

- ebct:

  The empty bed contact time (min) used for the biofilter.

- ozonated:

  Logical; TRUE if the water is ozonated (default), FALSE otherwise.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
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
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.


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
