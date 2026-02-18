# Calculate bromate formation

Calculates bromate (BrO3-, ug/L) formation based on selected model.
Required arguments include an object of class "water" created by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
ozone dose, reaction time, and desired model. The function also requires
additional water quality parameters defined in
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
including bromide, DOC or UV254 (depending on the model), pH, alkalinity
(depending on the model), and optionally, ammonia (added when defining
water using the `tot_nh3` argument.) For a single water use
`ozonate_bromate`; for a dataframe use `ozonate_bromate_df`. Use
`pluck_cols = TRUE` to get values from the output water as new dataframe
columns. For most arguments in the `_df` helper "use_col" default looks
for a column of the same name in the dataframe. The argument can be
specified directly in the function instead or an unquoted column name
can be provided.

## Usage

``` r
ozonate_bromate(water, dose, time, model = "Ozekin")

ozonate_bromate_df(
  df,
  input_water = "defined",
  output_water = "ozonated",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  dose = "use_col",
  time = "use_col",
  model = "use_col"
)
```

## Source

Ozekin (1994), Sohn et al (2004), Song et al (1996), Galey et al (1997),
Siddiqui et al (1994)

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- dose:

  Applied ozone dose (mg/L as O3). Results typically valid for 1-10
  mg/L, but varies depending on model.

- time:

  Reaction time (minutes). Results typically valid for 1-120 minutes,
  but varies depending on model.

- model:

  Model to apply. One of c("Ozekin", "Sohn", "Song", "Galey",
  "Siddiqui")

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include a column named for the applied chlorine dose (cl2),
  and a column for time in minutes.

- input_water:

  name of the column of water class data to be used as the input for
  this function. Default is "defined".

- output_water:

  name of the output column storing updated water class object. Default
  is "ozonated".

- pluck_cols:

  Extract water slots modified by the function (bro3) into new numeric
  columns for easy access. Default to FALSE.

- water_prefix:

  Append the output_water name to the start of the plucked columns.
  Default is TRUE.

## Value

`ozonate_bromate` returns a single water class object with calculated
bromate (ug/L).

`ozonate_bromate_df` returns a data frame containing a water class
column with updated bro3 concentration. Optionally, it also adds columns
for each of those slots individually.

## Examples

``` r
example_dbp <- define_water(8, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
  ozonate_bromate(dose = 1.5, time = 5, model = "Ozekin")
#> Warning: Missing value for DOC. Default value of 95% of TOC will be used.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.
example_dbp <- define_water(7.5, 20, 66, toc = 4, uv254 = .2, br = 50) %>%
  ozonate_bromate(dose = 3, time = 15, model = "Sohn")
#> Warning: Missing value for DOC. Default value of 95% of TOC will be used.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.


example_df <- water_df %>%
  dplyr::slice_head(n = 6) %>%
  dplyr::mutate(br = 50) %>%
  define_water_df() %>%
  dplyr::mutate(
    dose = c(seq(.5, 3, .5)),
    OzoneTime = 30
  ) %>%
  ozonate_bromate_df(time = OzoneTime, model = "Sohn", pluck_cols = TRUE)
```
