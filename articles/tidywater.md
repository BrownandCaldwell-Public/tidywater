# Getting started with tidywater

``` r
library(tidywater)
```

## Defining an example water

Most tidywater functions required a `water` object. Start by creating a
water with `define_water`. There are a lot of optional arguments that
correspond to different water quality parameters. Start by specifying
everything you know, or at least all the parameters relevant to the
modeling you want to do. Parameters are all lowercase and use common
abbreviations or chemical formulas. If you aren’t sure what the correct
argument name is, check the `define_water` documentation. Concentrations
are specified in the most common units - usually mg/L or ug/L depending
on the parameter. Units are also in the documentation, so make sure to
check carefully until you are familiar with the system.

``` r
mywater <- define_water(
  ph = 7, temp = 15, alk = 100, tot_hard = 100, na = 100, cl = 80,
  cond = 100,
  toc = 3, uv254 = .02, br = 50
)
#> Warning in define_water(ph = 7, temp = 15, alk = 100, tot_hard = 100, na = 100,
#> : Missing values for calcium and magnesium but total hardness supplied. Default
#> ratio of 65% Ca2+ and 35% Mg2+ will be used.
#> Warning in define_water(ph = 7, temp = 15, alk = 100, tot_hard = 100, na = 100,
#> : Missing value for DOC. Default value of 95% of TOC will be used.
```

Now that we have a water, we can apply treatment models to it. The main
models require a `water` input and will usually output another `water`
or a number. Functions in tidywater follow the naming convention
`treatmentapplied_parametersmodeled`. For example, when we want to dose
chemical and see the impact on pH/alkalinity, we use `chemdose_ph`.
There are a lot of available chemicals, which you can view with the
documentation. Most chemicals are specified using the chemical formula
in all lowercase, except hydrated coagulants, which are named. Units for
the chemical are also specified, and are usually mg/L as chemical.

## Coagulation model

First, let’s apply a model that will predict the effect of coagulation
on the given `mywater` conditions we set up earlier. Start by
determining the impact of adding 5 mg/L HCl and 20 mg/L alum as the
coagulant.

``` r
dosed_water <- chemdose_ph(mywater, hcl = 5, alum = 20)
#> Warning in chemdose_ph(mywater, hcl = 5, alum = 20): Sulfate-containing
#> chemical dosed, but so4 water slot is NA. Slot not updated because background
#> so4 unknown.
mywater@ph
#> [1] 7
dosed_water@ph
#> [1] 6.68
```

Now `dosed_water` has updated pH chemistry based on the hydrochloric
acid and alum doses. However, other slots in the water, such as TOC,
have not been updated. If we also want to know how the coagulant impacts
TOC, we need to apply `chemdose_toc` as well. This function defaults to
published model coefficients, but because it’s an empirical model, you
could also select your own coefficients.

``` r
coag_water <- chemdose_toc(dosed_water, alum = 20)

dosed_water@doc
#> [1] 2.85
coag_water@doc
#> [1] 2.428063
```

The two functions `chemdose_ph` and `chemdose_toc` can also be chained
together using the pipe operator `%>%` to calculate the changes to pH
and TOC more compactly. Notice that the pH and DOC values returned from
`piped_coag_water` are the same as those calculated above in
`coag_water`.

``` r
piped_coag_water <- mywater %>%
  chemdose_ph(hcl = 5, alum = 20) %>%
  chemdose_toc(alum = 20)
#> Warning in chemdose_ph(., hcl = 5, alum = 20): Sulfate-containing chemical
#> dosed, but so4 water slot is NA. Slot not updated because background so4
#> unknown.

piped_coag_water@ph
#> [1] 6.68
piped_coag_water@doc
#> [1] 2.428063
```

We can also solve for chemical doses to achieve a target pH with
`solvedose_ph`. This function outputs a number instead of a water.

``` r
caustic_req <- solvedose_ph(coag_water, target_ph = 8.6, chemical = "naoh")

fin_water <- chemdose_ph(coag_water, naoh = caustic_req)
```

## Disinfection model

We can apply similar principals for disinfection. Note that we have to
specify the chlorine dose in mg/L Cl2 in both `chemdose_ph` and
`chemdose_dbp` because they are calculating two different things. In
this example, the DBP function displays some warnings because the water
we are modeling is outside the bounds of the original model fitting.
This is common, and something you should always be aware of (even if
tidywater doesn’t warn you). We can use `summarize_wq` to view different
groups of parameters in the water.

``` r
dist_water <- chemdose_ph(fin_water, naocl = 4) %>%
  chemdose_dbp(cl2 = 4, time = 24, treatment = "coag")
#> Warning in chemdose_dbp(., cl2 = 4, time = 24, treatment = "coag"): UV254 is
#> outside the model bounds of 0.016 <= UV254 <= 0.215 cm-1 for coagulated water.
#> Warning in chemdose_dbp(., cl2 = 4, time = 24, treatment = "coag"): Temperature
#> is outside the model bounds of temp=20 Celsius for coagulated water.
#> Warning in chemdose_dbp(., cl2 = 4, time = 24, treatment = "coag"): pH is
#> outside the model bounds of pH = 7.5 for coagulated water

summarize_wq(dist_water, "dbps")
```

[TABLE]

## Summary and Recommended Resources

In this tutorial, we walked through how to set up influent water
conditions by specifying water quality parameters using the
`define_water` function. Then, we applied two common treatment models,
coagulation and disinfection, and predicted the effect of the chemical
addition on pH and TOC and/or DBPs. These functions return a water
object with new water quality parameters, which represent effluent water
conditions. It is important to note that there are individual functions
that update each water quality parameter. Changes to pH and TOC, for
example, require separate functions to calculate, which can be chained
together using the pipe operator `%>%`. There are also functions that
can solve for the required chemical dose, such as `solvedose_ph` which
solves for the chemical dose needed to achieve a target pH value.

Tidywater functions can also be applied to data frames using the `_df`
suffix. To learn more about those functions, look at the documentation
or read the helper function vignette.

If you want a more detailed introduction to tidywater, check out the
intro vignette.
