# Data frame of bromate coefficients for predicting bromate formation during ozonation

A dataset containing coefficients for calculating ozone formation

## Usage

``` r
bromatecoeffs
```

## Format

A dataframe with 30 rows and 10 columns

- model:

  First author of source model

- ammonia:

  Either T or F, depending on whether the model applies to waters with
  ammonia present.

- A:

  First coefficient in bromate model

- a:

  Exponent in bromate model, associated with Br-

- b:

  Exponent in bromate model, associated with DOC

- c:

  Exponent in bromate model, associated with UVA

- d:

  Exponent in bromate model, associated with pH

- e:

  Exponent in bromate model, associated with Alkalinity

- f:

  Exponent in bromate model, associated with ozone dose

- g:

  Exponent in bromate model, associated with reaction time

- h:

  Exponent in bromate model, associated with ammonia (NH4+)

- i:

  Exponent in bromate model, associated with temperature

- I:

  Coefficient in bromate model, associated with temperature in the
  exponent. Either i or I are used, not both.

## Source

Ozekin (1994), Sohn et al (2004), Song et al (1996), Galey et al (1997),
Siddiqui et al (1994)

See references list at:
<https://github.com/BrownandCaldwell-Public/tidywater/wiki/References>
