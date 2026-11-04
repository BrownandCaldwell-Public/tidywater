# Determine TOC removal from coagulation

This function applies the Edwards (1997) model to a water created by
[define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)
to determine coagulated DOC. Reuse waters with effluent organic matter
(EfOM) can be modeled by selecting one of the Wilder & Korak (2025)
models with `coeff`. Reuse models apply only to aluminum coagulants
(alum, ACH, PACl). Model assumes all particulate TOC is removed;
therefore TOC = DOC in output. Coagulated UVA is from U.S. EPA (2001)
equation 5-80 for Edwards (1997) model and a regression for the Wilder &
Korak (2025) model. Note that the models rely on pH of coagulation. If
only raw water pH is known, utilize
[chemdose_ph](https://brownandcaldwell-public.github.io/tidywater/reference/chemdose_ph.md)
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
  ach = 0,
  pacl = 0,
  coeff = "Alum",
  caoh2 = 0,
  fns = NA_real_
)

chemdose_toc_df(
  df,
  input_water = "defined",
  output_water = "coagulated",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  keep_dose_cols = FALSE,
  alum = "use_col",
  ferricchloride = "use_col",
  ferricsulfate = "use_col",
  ach = "use_col",
  pacl = "use_col",
  caoh2 = "use_col",
  coeff = "use_col",
  fns = "use_col"
)
```

## Source

Edwards (1997)

Wilder & Korak (2025)

U.S. EPA (2001)

See reference list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>

## Arguments

- water:

  Source water object of class "water" created by
  [define_water](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).
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

- ach:

  Amount of aluminum chlorohydrate added in mg/L: Al2(OH)5Cl\*2H2O +
  HCO3 -\> 2Al(OH)3(am) + Cl + 2H2O + CO2

- pacl:

  Amount of polyaluminum chloride added in mg/L as Al2O3 (assumed Cl:Al
  ratio = 0.9): Al2(OH)4.2Cl(1.8) + \#HCO3 -\> 2Al(OH)3(am) + 1.8Cl +
  \#H2O + \#CO2....

- coeff:

  String specifying the Edwards coefficients to be used from "Alum",
  "Ferric", "General Alum", "General Ferric", or "Low DOC" or data frame
  of coefficients, which must include: k1, k2, x1, x2, x3, b. For reuse
  waters (aluminum coagulants only), Wilder & Korak string specifying
  coefficients can be used from "Reuse General Langmuir", "Reuse Site
  Langmuir", "Reuse General Linear", "Reuse Site Linear", or a dataframe
  of coefficients that must include: fns, x1, x2, x3, b for Langmuir or
  fns, z0, z1, z2 for Linear. Reuse models were validated for
  coagulation pH 5.5 - 8 and 20 - 200 mg/L alum. See reference list for
  more information.

- caoh2:

  Option to add caoh2 in mg/L to soften the water. Will predict DOC,
  TOC, UV254 using a modified equation (see reference list). Defaults to
  zero.

- fns:

  Fraction of non-sorbing DOC (0 - 1), required for the site-specific
  reuse models ("Reuse Site Langmuir" and "Reuse Site Linear"). Measured
  as the fraction of DOC remaining after coagulation with 200 mg/L alum
  at pH 6 (Wilder & Korak 2025). The general reuse models use fixed
  values of 0.649 (Langmuir) and 0.650 (Linear); a supplied `fns` is
  ignored with a warning. Not used by Edwards models.

- df:

  a data frame containing a water class column, which has already been
  computed using
  [define_water_df](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md).
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

- keep_dose_cols:

  Option to keep the original chemical dose columns in the output data
  frame. Default is FALSE.

## Value

`chemdose_toc` returns a single water class object with an updated DOC,
TOC, and UV254 concentration.

`chemdose_toc_df` returns a data frame containing a water class column
with updated DOC, TOC, and UV254 concentrations. Optionally, it also
adds columns for each of those slots individually.

## See also

[chemdose_ph](https://brownandcaldwell-public.github.io/tidywater/reference/chemdose_ph.md)

## Examples

``` r
water <- define_water(ph = 7, temp = 25, alk = 100, toc = 3.7, doc = 3.5, uv254 = .1)
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
dosed_water <- chemdose_ph(water, alum = 30) %>%
  chemdose_toc(alum = 30, coeff = "Alum")
#> Warning: Sulfate-containing chemical dosed, but so4 slot is "NA"; slot not updated
#> because background SO4 is unknown.

dosed_water <- chemdose_ph(water, alum = 10, h2so4 = 10) %>%
  chemdose_toc(alum = 10, coeff = data.frame(
    x1 = 280, x2 = -73.9, x3 = 4.96, k1 = -0.028, k2 = 0.23, b = 0.068
  ))
#> Warning: Sulfate-containing chemical dosed, but so4 slot is "NA"; slot not updated
#> because background SO4 is unknown.

dosed_reuse <- chemdose_ph(water, alum = 10, h2so4 = 10) %>%
  chemdose_toc(alum = 40, coeff = "Reuse Site Langmuir", fns = 0.6)
#> Warning: Sulfate-containing chemical dosed, but so4 slot is "NA"; slot not updated
#> because background SO4 is unknown.

# \donttest{
example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(FerricDose = seq(1, 12, 1)) %>%
  chemdose_toc_df(ferricchloride = FerricDose, coeff = "Ferric")
#> Warning: pH>8 is outside Edwards model range; assumed pH==8 for modeling. (6 rows
#> affected.)

example_df <- water_df %>%
  define_water_df() %>%
  dplyr::mutate(ferricchloride = seq(1, 12, 1)) %>%
  chemdose_toc_df(coeff = "Ferric", pluck_cols = TRUE)
#> Warning: pH>8 is outside Edwards model range; assumed pH==8 for modeling. (6 rows
#> affected.)
# }
```
