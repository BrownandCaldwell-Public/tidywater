# Calculate a desired chemical dose for a target alkalinity

This function calculates the required amount of a chemical to dose based
on a target alkalinity and existing water quality. Returns numeric value
for dose in mg/L. Uses uniroot on the chemdose_ph function. For a single
water, use `solvedose_alk`; to apply the model to a dataframe, use
`solvedose_alk_df`. For most arguments, the `_df` helper "use_col"
default looks for a column of the same name in the dataframe. The
argument can be specified directly in the function instead or an
unquoted column name can be provided.

## Usage

``` r
solvedose_alk(water, target_alk, chemical)

solvedose_alk_df(
  df,
  input_water = "defined",
  output_column = "dose",
  target_alk = "use_col",
  chemical = "use_col"
)
```

## Arguments

- water:

  Source water of class "water" created by
  [`define_water`](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- target_alk:

  The final alkalinity in mg/L as CaCO3 to be achieved after the
  specified chemical is added.

- chemical:

  The chemical to be added. Current supported chemicals include: acids:
  "hcl", "h2so4", "h3po4", "co2", bases: "naoh", "na2co3", "nahco3",
  "caoh2", "mgoh2"

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include a column with names for each of the chemicals being
  dosed.

- input_water:

  name of the column of water class data to be used as the input.
  Default is "defined_water".

- output_column:

  name of the output column storing doses in mg/L. Default is
  "dose_required".

## Value

`solvedose_alk` returns a numeric value for the required chemical dose.

`solvedose_alk_df` returns a data frame containing the original data
frame and columns for target alkalinity, chemical dosed, and required
chemical dose.

## Details

`solvedose_alk` uses
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) on
[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md)
to match the required dose for the requested alkalinity target.

## See also

[solvedose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/solvedose_ph.md)

## Examples

``` r
dose_required <- define_water(ph = 7.9, temp = 22, alk = 100, 80, 50) %>%
  solvedose_alk(target_alk = 150, "naoh")
#> Warning: Missing value for magnesium. Value estimated from total hardness and calcium.
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(finAlk = seq(100, 210, 10)) %>%
  solvedose_alk_df(chemical = "na2co3", target_alk = finAlk)
#> Warning: Target alkalinity cannot be reached with selected chemical. NA returned.
#> Warning: Target alkalinity cannot be reached with selected chemical. NA returned.
```
