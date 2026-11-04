# Determine if TOC removal meets Stage 1 DBP Rule requirements

This function takes raw water alkalinity, raw water TOC, and finished
water TOC. It then calculates the TOC removal percentage and checks
compliance with the Stage 1 DBP Rule.

## Usage

``` r
regulate_toc(
  alk_raw,
  toc_raw,
  toc_finished,
  raw_water,
  treated_water,
  skip_alternate = FALSE
)

regulate_toc_df(
  df,
  alk_raw = "use_col",
  toc_raw = "use_col",
  toc_finished = "use_col",
  input_raw_water = "defined",
  input_treated_water = "treated",
  skip_alternate = FALSE
)
```

## Arguments

- alk_raw:

  Raw water alkalinity (mg/L as calcium carbonate). Not needed if waters
  are provided.

- toc_raw:

  Raw water total organic carbon (mg/L). Not needed if waters are
  provided.

- toc_finished:

  Finished water total organic carbon (mg/L). Not needed if waters are
  provided.

- raw_water:

  A water object of class "water" created by
  [`define_water()`](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)
  containing raw water parameters. If provided, the function will use
  the `alk` and `toc` slots from this object as inputs. If not provided,
  `alk_raw` and `toc_raw` must be specified.

- treated_water:

  A water object of class "water" created by
  [`define_water()`](https://brownandcaldwell-public.github.io/tidywater/reference/define_water.md)
  containing treated water parameters. If provided, the function will
  use the `toc` slot from this object as input for finished water TOC.
  If not provided, `toc_finished` must be specified.

- skip_alternate:

  Logical value indicating whether to skip alternative compliance
  checks. By default, alternative compliance will be checked.

- df:

  a data frame optionally containing columns for raw water alkalinity,
  raw water TOC, and finished water TOC

- input_raw_water:

  a name of the column of water class data to be used as the input raw
  water for this function. Default is "defined".

- input_treated_water:

  a name of the column of water class data to be used as the input
  treated water for this function. Default is "treated".

## Value

A data frame containing the TOC removal compliance status, and a comment
with the reasoning for the compliance status.

A data frame with compliance status, removal percent, and optional note
columns.

## Details

The function prints the input parameters and the calculated removal
percentage for TOC. It checks compliance with regulations considering
the raw TOC, alkalinity, and removal percentage. It outputs a dataframe
that contains the compliance status as a boolean, calculated removal
precent as a numeric, and a comment that indicates internal reasoning
for compliance status.

## Examples

``` r
regulate_toc(alk_raw=50, toc_raw=5, toc_finished=2)
#>   toc_compliance_status toc_removal_percent required_removal
#> 1                  TRUE                  60                0
#>                                                                              comment
#> 1 Treated water TOC < 2 mg/L meets alternative compliance (sect 141.135 (a)(2)(ii)).

water1 <- define_water(alk = 70, toc = 4)
#> Warning: Missing value for `ph`; carbonate balance will not be calculated.
#> Warning: Missing value for `doc`; defaulting to 95% of TOC.
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
water2 <- define_water(toc = 2.5)
#> Warning: Missing value for `ph`; carbonate balance will not be calculated.
#> Warning: Missing value for `alk`; carbonate balance will not be calculated.
#> Warning: Missing value for `doc`; defaulting to 95% of TOC.
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
regulate_toc(raw_water = water1, treated_water = water2)
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Raw water SUVA is missing or cannot be calculated. Alternative compliance based on SUVA (sect 141.135 (a)(2)(v)) will not be applied.
#> Warning: Treated water SUVA is missing or cannot be calculated. Alternative compliance based on SUVA (sect 141.135 (a)(2)(vi)) will not be applied.
#>   toc_compliance_status toc_removal_percent required_removal
#> 1                  TRUE                37.5               25
#>                   comment
#> 1 Standard compliance met

regulate_toc(raw_water = define_water(alk = 50, toc = 5), 
treated_water = define_water(toc = 2), skip_alternate = TRUE)
#> Warning: Missing value for `ph`; carbonate balance will not be calculated.
#> Warning: Missing value for `doc`; defaulting to 95% of TOC.
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
#> Warning: Missing value for `ph`; carbonate balance will not be calculated.
#> Warning: Missing value for `alk`; carbonate balance will not be calculated.
#> Warning: Missing value for `doc`; defaulting to 95% of TOC.
#> Warning: Major ions, TDS, and conductivity are all missing; assuming ideal conditions.
#> Ionic strength will be "NA" and activity coefficients will be 1.
#>   toc_compliance_status toc_removal_percent required_removal
#> 1                  TRUE                  60               45
#>                   comment
#> 1 Standard compliance met


regulated <- water_df %>%
  dplyr::select(toc_raw = toc, alk_raw = alk) %>%
  regulate_toc_df(toc_finished = seq(2, 4, 0.1))

regulated <- water_df %>%
  define_water_df() %>%
  chemdose_ph_df(alum = 30, output_water = "dosed") %>%
  chemdose_toc_df("dosed") %>%
  pluck_water(c("coagulated", "defined"), c("toc", "alk")) %>%
  dplyr::select(toc_finished = coagulated_toc, toc_raw = defined_toc, alk_raw = defined_alk) %>%
  regulate_toc_df()
#> Warning: No coagulants dosed; final water will equal input water. (12 rows affected.)

water1<-water_df |> 
 define_water_df() |> 
 chemdose_toc_df(output_water= "dosed", alum=20) |> 
 regulate_toc_df(input_raw_water = "defined", input_treated_water= "dosed")
#> Warning: pH>8 is outside Edwards model range; assumed pH==8 for modeling. (6 rows
#> affected.)
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
#> Warning: Treated Water DBPs (TTHM or HAA5) are missing. Alternative compliance based on DBPs (sect 141.135 (a)(2)(iii and iv)) may not be applied.
 
```
