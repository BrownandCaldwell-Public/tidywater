# Create a water class object given water quality parameters

This function takes user-defined water quality parameters and creates an
S4 "water" class object that forms the input and output of all tidywater
models.

## Usage

``` r
define_water(
  ph,
  temp = 25,
  alk,
  tot_hard,
  ca,
  mg,
  na,
  k,
  cl,
  so4,
  mno4,
  free_chlorine = 0,
  combined_chlorine = 0,
  tot_po4 = 0,
  tot_nh3 = 0,
  tot_ch3coo = 0,
  tot_bo3 = 0,
  tot_sio4 = 0,
  tds,
  cond,
  toc,
  doc,
  uv254,
  br,
  f,
  fe,
  al,
  mn,
  no3
)
```

## Source

Crittenden et al. (2012) equation 5-38 - ionic strength from TDS

Snoeyink & Jenkins (1980) - ionic strength from conductivity

Lewis and Randall (1921), Crittenden et al. (2012) equation 5-37 - ionic
strength from ion concentrations

Harned and Owen (1958), Crittenden et al. (2012) equation 5-45 -
Temperature correction of dielectric constant (relative permittivity)

## Arguments

- ph:

  water pH

- temp:

  Temperature in degree C

- alk:

  Alkalinity in mg/L as CaCO3

- tot_hard:

  Total hardness in mg/L as CaCO3

- ca:

  Calcium in mg/L Ca2+

- mg:

  Magnesium in mg/L Mg2+

- na:

  Sodium in mg/L Na+

- k:

  Potassium in mg/L K+

- cl:

  Chloride in mg/L Cl-

- so4:

  Sulfate in mg/L SO42-

- mno4:

  Permanganate in mg/L MnO4-

- free_chlorine:

  Free chlorine in mg/L as Cl2. Used when a starting water has a free
  chlorine residual.

- combined_chlorine:

  Combined chlorine (chloramines) in mg/L as Cl2. Used when a starting
  water has a chloramine residual.

- tot_po4:

  Phosphate in mg/L as PO4 3-. Used when a starting water has a
  phosphate residual.

- tot_nh3:

  Total ammonia in mg/L as N

- tot_ch3coo:

  Total acetate in mg/L

- tot_bo3:

  Total borate (B(OH)4 -) in mg/L as B

- tot_sio4:

  Total silicate in mg/L as SiO2

- tds:

  Total Dissolved Solids in mg/L (optional if ions are known)

- cond:

  Electrical conductivity in uS/cm (optional if ions are known)

- toc:

  Total organic carbon (TOC) in mg/L

- doc:

  Dissolved organic carbon (DOC) in mg/L

- uv254:

  UV absorbance at 254 nm (cm-1)

- br:

  Bromide in ug/L Br-

- f:

  Fluoride in mg/L F-

- fe:

  Iron in mg/L Fe3+

- al:

  Aluminum in mg/L Al3+

- mn:

  Manganese in ug/L Mn2+

- no3:

  Nitrate in mg/L as N

## Value

define_water outputs a water class object where slots are filled or
calculated based on input parameters. Water slots have different units
than those input into the define_water function, as listed below.

- pH:

  pH, numeric, in standard units (SU).

- temp:

  temperature, numeric, in °C.

- alk:

  alkalinity, numeric, mg/L as CaCO3.

- tds:

  total dissolved solids, numeric, mg/L.

- cond:

  electrical conductivity, numeric, uS/cm.

- tot_hard:

  total hardness, numeric, mg/L as CaCO3.

- kw:

  dissociation constant for water, numeric, unitless.

- alk_eq:

  total alkalinity as equivalents, numeric, equivalent (eq).

- carbonate_alk_eq:

  carbonate alkalinity as equivalents, numeric, equivalent (eq).

- phosphate_alk_eq:

  phosphate alkalinity as equivalents, numeric, equivalent (eq).

- ammonium_alk_eq:

  ammonium alkalinity as equivalents, numeric, equivalent (eq).

- borate_alk_eq:

  borate alkalinity as equivalents, numeric, equivalent (eq).

- silicate_alk_eq:

  silicate alkalinity as equivalents, numeric, equivalent (eq).

- hypochlorite_alk_eq:

  hypochlorite alkalinity as equivalents, numeric, equivalent (eq).

- toc:

  total organic carbon, numeric, mg/L.

- doc:

  dissolved organic carbon, numeric, mg/L.

- bdoc:

  biodegradable organic carbon, numeric, mg/L.

- uv254:

  light absorption at 254 nm, numeric, cm-1.

- dic:

  dissolved inorganic carbon, numeric, mg/L as C.

- is:

  ionic strength, numeric, mol/L.

- na:

  sodium, numeric, mols/L.

- ca:

  calcium, numeric, mols/L.

- mg:

  magnesium, numeric, mols/L.

- k:

  potassium, numeric, mols/L.

- cl:

  chloride, numeric, mols/L.

- so4:

  sulfate, numeric, mols/L.

- mno4:

  permanganate, numeric, mols/L.

- no3:

  nitrate, numeric, mols/L.

- hco3:

  bicarbonate, numeric, mols/L.

- co3:

  carbonate, numeric, mols/L.

- h2po4:

  phosphoric acid, numeric, mols/L.

- hpo4:

  hydrogen phosphate, numeric, mols/L.

- po4:

  phosphate, numeric, mols/L.

- nh4:

  ammonium, numeric, mol/L as N.

- bo3:

  borate, numeric, mol/L.

- h3sio4:

  trihydrogen silicate, numeric, mol/L.

- h2sio4:

  dihydrogen silicate, numeric, mol/L.

- ch3coo:

  acetate, numeric, mol/L.

- h:

  hydrogen ion, numeric, mol/L.

- oh:

  hydroxide ion, numeric, mol/L.

- tot_po4:

  total phosphate, numeric, mol/L.

- tot_nh3:

  total ammonia, numeric, mol/L.

- tot_co3:

  total carbonate, numeric, mol/L.

- tot_bo3:

  total borate, numeric, mol/L.

- tot_sio4:

  total silicate, numeric, mol/L.

- tot_ch3coo:

  total acetate, numeric, mol/L.

- br:

  bromide, numeric, mol/L.

- bro3:

  bromate, numeric, mol/L.

- f:

  fluoride, numeric, mol/L.

- fe:

  iron, numeric, mol/L.

- al:

  aluminum, numeric, mol/L.

- mn:

  manganese, numeric, mol/L.

- free_chlorine:

  free chlorine, numeric, mol/L.

- ocl:

  hypochlorite ion, numeric, mol/L.

- combined_chlorine:

  sum of chloramines, numeric, mol/L.

- nh2cl:

  monochloramine, numeric, mol/L.

- nhcl2:

  dichloramine, numeric, mol/L.

- ncl3:

  trichloramine, numeric, mol/L.

- chcl3:

  chloroform, numeric, ug/L.

- chcl2br:

  bromodichloromethane, numeric, ug/L.

- chbr2cl:

  dibromodichloromethane, numeric, ug/L.

- chbr3:

  bromoform, numeric, ug/L.

- tthm:

  total trihalomethanes, numeric, ug/L.

- mcaa:

  chloroacetic acid, numeric, ug/L.

- dmcaa:

  dichloroacetic acid, numeric, ug/L.

- tcaa:

  trichloroacetic acid, numeric, ug/L.

- mbaa:

  bromoacetic acid, numeric, ug/L.

- dbaa:

  dibromoacetic acid, numeric, ug/L.

- haa5:

  sum of haloacetic acids, numeric, ug/L.

- bcaa:

  bromochloroacetic acid, numeric, ug/L.

- cdbaa:

  chlorodibromoacetic acid, numeric, ug/L.

- dcbaa:

  dichlorobromoacetic acid, numeric, ug/L.

- tbaa:

  tribromoacetic acid, numeric, ug/L.

## Details

Carbonate balance is calculated and units are converted to mol/L. Ionic
strength is determined from ions, TDS, or conductivity. Missing values
are handled by defaulting to 0 or NA. Calcium defaults to 65 percent of
the total hardness when not specified. DOC defaults to 95 percent of
TOC.

## Examples

``` r
water_missingions <- define_water(ph = 7, temp = 15, alk = 100, tds = 10)
water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#> Warning: User entered total hardness is >10% different than calculated hardness.
```
