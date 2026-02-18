# Determine if TOC removal meets Stage 1 DBP Rule requirements

This function takes raw water alkalinity, raw water TOC, and finished
water TOC. It then calculates the TOC removal percentage and checks
compliance with the Stage 1 DBP Rule.

## Usage

``` r
regulate_toc(alk_raw, toc_raw, toc_finished)

regulate_toc_df(
  df,
  alk_raw = "use_col",
  toc_raw = "use_col",
  toc_finished = "use_col"
)
```

## Arguments

- alk_raw:

  Raw water alkalinity (mg/L as calcium carbonate).

- toc_raw:

  Raw water total organic carbon (mg/L).

- toc_finished:

  Finished water total organic carbon (mg/L).

- df:

  a data frame optionally containing columns for raw water alkalinity,
  raw water TOC, and finished water TOC

## Value

A data frame containing the TOC removal compliance status.

A data frame with compliance status, removal percent, and optional note
columns.

## Details

The function prints the input parameters and the calculated removal
percentage for TOC. It checks compliance with regulations considering
the raw TOC, alkalinity, and removal percentage. If the conditions are
met, it prints "In compliance"; otherwise, it prints "Not in compliance"
and stops execution with an error message.

## Examples

``` r
regulate_toc(50, 5, 2)
#>   toc_compliance_status toc_removal_percent
#> 1         In Compliance                  60


regulated <- water_df %>%
  dplyr::select(toc_raw = toc, alk_raw = alk) %>%
  regulate_toc_df(toc_finished = seq(0, 1.2, 0.1))
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.

regulated <- water_df %>%
  define_water_df() %>%
  chemdose_ph_df(alum = 30, output_water = "dosed") %>%
  chemdose_toc_df("dosed") %>%
  pluck_water(c("coagulated", "defined"), c("toc", "alk")) %>%
  dplyr::select(toc_finished = coagulated_toc, toc_raw = defined_toc, alk_raw = defined_alk) %>%
  regulate_toc_df()
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
#> Warning: Raw water TOC < 2 mg/L. No regulation applies.
```
