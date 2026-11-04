# Data frame of model coefficients for coagulation TOC removal

A dataset containing coefficients from the Edwards (1997) model for
coagulation TOC removal, or Wilder & Korak (2025) model for reuse
waters.

## Usage

``` r
chemdosetoccoeff
```

## Format

A dataframe with 9 rows and 11 columns. Columns not used by a model are
NA:

- ID:

  Coefficient type

- x3:

  x3 parameter

- x2:

  x2 parameter

- x1:

  x1 parameter

- k1:

  k1 parameter

- k2:

  k2 parameter

- b:

  b parameter

- fns:

  Fraction of non-sorbing DOC

- z0:

  z0 parameter

- z1:

  z1 parameter

- z2:

  z2 parameter

## Source

Edwards (1997) Table 2. Wilder & Korak (2025) Table 1.

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>
