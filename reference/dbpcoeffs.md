# Data frame of DBP coefficients for predicting DBP formation

A dataset containing coefficients for calculating DBP formation

## Usage

``` r
dbpcoeffs
```

## Format

A dataframe with 30 rows and 10 columns

- ID:

  abbreviation of dbp species

- alias:

  full name of dbp species

- water_type:

  specifies which model the constants apply to, either treated or
  untreated water

- A:

  First coefficient in DBP model

- a:

  Second coefficient in DBP model, associated with TOC or DOC

- b:

  Third coefficient in DBP model, associated with Cl2

- c:

  Fourth coefficient in DBP model, associated with Br-

- d:

  Fifth coefficient in DBP model, associated with temperature

- e:

  Sixth coefficient in DBP model, associated with pH

- f:

  Seventh coefficient in DBP model, associated with reaction time

## Source

U.S. EPA (2001)

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>
