# Simulate contributions of various lead solids to total soluble lead

This function takes a water data frame defined by
[define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md)
and outputs a dataframe of the controlling lead solid and total lead
solubility. Lead solid solubility is calculated based on controlling
solid. Total dissolved lead species (tot_dissolved_pb, M) are calculated
based on lead complex calculations. For a single water, use
`dissolve_pb`; to apply the model to a dataframe, use `dissolve_pb_df`.
For most arguments, the `_df` "use_col" default looks for a column of
the same name in the dataframe. The argument can be specified directly
in the function instead or an unquoted column name can be provided.

## Usage

``` r
dissolve_pb(
  water,
  hydroxypyromorphite = "Schock",
  pyromorphite = "Topolska",
  laurionite = "Nasanen"
)

dissolve_pb_df(
  df,
  input_water = "defined",
  output_col_solid = "controlling_solid",
  output_col_result = "pb",
  hydroxypyromorphite = "Schock",
  pyromorphite = "Topolska",
  laurionite = "Nasanen",
  water_prefix = TRUE
)
```

## Source

Code is from EPA's TELSS lead solubility dashboard
<https://github.com/USEPA/TELSS> which is licensed under MIT License:
Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions: The above copyright notice and this permission
notice shall be included in all copies or substantial portions of the
Software.

Wahman et al. (2021)

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water.md).
  Water must include alk and is. If po4, cl, and so4 are known, those
  should also be included.

- hydroxypyromorphite:

  defaults to "Schock", the constant, K, developed by Schock et al
  (1996). Can also use "Zhu".

- pyromorphite:

  defaults to "Topolska", the constant, K, developed by Topolska et al
  (2016). Can also use "Xie".

- laurionite:

  defaults to "Nasanen", the constant, K, developed by Nasanen & Lindell
  (1976). Can also use "Lothenbach".

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://BrownandCaldwell-Public.github.io/tidywater/reference/define_water_df.md)

- input_water:

  name of the column of water class data to be used as the input.
  Default is "defined_water".

- output_col_solid:

  name of the output column storing the controlling lead solid. Default
  is "controlling_solid".

- output_col_result:

  name of the output column storing dissolved lead in M. Default is
  "pb".

- water_prefix:

  name of the input water used for the calculation, appended to the
  start of output columns. Default is TRUE. Change to FALSE to remove
  the water prefix from output column names.

## Value

`dissolve_pb` returns a one row data frame containing only the
controlling lead solid and modeled dissolved lead concentration.

`dissolve_pb_df` returns a data frame containing the controlling lead
solid and modeled dissolved lead concentration as new columns.

## Details

The solid with lowest solubility will form the lead scale (controlling
lead solid). Some lead solids have two k-constant options. The function
will default to the EPA's default constants. The user may change the
constants to hydroxypyromorphite = "Zhu" or pyromorphite = "Xie" or
laurionite = "Lothenbach"

Make sure that total dissolved solids, conductivity, or ca, na, cl, so4
are used in `define_water` so that an ionic strength is calculated.

## Examples

``` r
example_pb <- define_water(
  ph = 7.5, temp = 25, alk = 93, cl = 240,
  tot_po4 = 0, so4 = 150, tds = 200
) %>%
  dissolve_pb()
example_pb <- define_water(
  ph = 7.5, temp = 25, alk = 93, cl = 240,
  tot_po4 = 0, so4 = 150, tds = 200
) %>%
  dissolve_pb(pyromorphite = "Xie")


example_df <- water_df %>%
  define_water_df() %>%
  dissolve_pb_df(output_col_result = "dissolved_lead", pyromorphite = "Xie")
```
