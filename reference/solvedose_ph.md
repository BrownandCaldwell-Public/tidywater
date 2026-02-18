# Calculate a desired chemical dose for a target pH

Calculates the required amount of a chemical to dose based on a target
pH and existing water quality. The function takes an object of class
"water", and user-specified chemical and target pH and returns a numeric
value for the required dose in mg/L. For a single water, use
`solvedose_ph`; to apply the model to a dataframe, use
`solvedose_ph_df`. For most arguments, the `_df` helper "use_col"
default looks for a column of the same name in the dataframe. The
argument can be specified directly in the function instead or an
unquoted column name can be provided.

## Usage

``` r
solvedose_ph(water, target_ph, chemical)

solvedose_ph_df(
  df,
  input_water = "defined",
  output_column = "dose",
  target_ph = "use_col",
  chemical = "use_col"
)
```

## Arguments

- water:

  Source water of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)

- target_ph:

  The final pH to be achieved after the specified chemical is added.

- chemical:

  The chemical to be added. Current supported chemicals include: acids:
  "hcl", "h2so4", "h3po4", "co2"; bases: "naoh", "na2co3", "nahco3",
  "caoh2", "mgoh2"

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md).
  The df may include a column with names for each of the chemicals being
  dosed.

- input_water:

  name of the column of water class data to be used as the input.
  Default is "defined".

- output_column:

  name of the output column storing doses in mg/L. Default is "dose".

## Value

`solvedose_ph` returns a numeric value for the required chemical dose.

`solvedose_ph_df` returns a data frame containing the original data
frame and columns for target pH, chemical dosed, and required chemical
dose.

## Details

`solvedose_ph` uses
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) on
[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md)
to match the required dose for the requested pH target.

## See also

[chemdose_ph](https://BrownandCaldwell-Public.github.io/tidywater/reference/chemdose_ph.md),
[solvedose_alk](https://BrownandCaldwell-Public.github.io/tidywater/reference/solvedose_alk.md)

## Examples

``` r
water <- define_water(ph = 7, temp = 25, alk = 10)
#> Warning: Major ions missing and neither TDS or conductivity entered. Ideal conditions will be assumed. Ionic strength will be set to NA and activity coefficients in future calculations will be set to 1.

# Calculate required dose of lime to reach pH 8
solvedose_ph(water, target_ph = 8, chemical = "caoh2")
#> [1] 1.5


example_df <- water_df %>%
  define_water_df() %>%
  solvedose_ph_df(input_water = "defined", target_ph = 8.8, chemical = "naoh")
```
