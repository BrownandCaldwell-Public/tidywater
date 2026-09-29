#' @title Create a water class object given water quality parameters
#'
#' @description This function takes user-defined water quality parameters and creates an S4 "water" class object that forms the input and output of all tidywater models.
#'
#' @details Carbonate balance is calculated and units are converted to mol/L. Ionic strength is determined from ions, TDS, or conductivity.
#' Missing values are handled by defaulting to 0 or NA.
#' Calcium defaults to 65 percent of the total hardness when not specified. DOC defaults to 95 percent of TOC.
#' @source Crittenden et al. (2012) equation 5-38 - ionic strength from TDS
#' @source Snoeyink & Jenkins (1980) - ionic strength from conductivity
#' @source Lewis and Randall (1921), Crittenden et al. (2012) equation 5-37 - ionic strength from ion concentrations
#' @source Harned and Owen (1958), Crittenden et al. (2012) equation 5-45 - Temperature correction of dielectric constant (relative permittivity)
#'
#' @param ph Water pH
#' @param temp Temperature in degree C
#' @param alk Alkalinity in mg/L as CaCO3
#' @param tot_hard Total hardness in mg/L as CaCO3
#' @param ca Calcium in mg/L Ca2+
#' @param mg Magnesium in mg/L Mg2+
#' @param na Sodium in mg/L Na+
#' @param k Potassium in mg/L K+
#' @param cl Chloride in mg/L Cl-
#' @param so4 Sulfate in mg/L SO42-
#' @param mno4 Permanganate in mg/L MnO4-
#' @param free_chlorine Free chlorine in mg/L as Cl2. Used when a starting water has a free chlorine residual.
#' @param combined_chlorine Combined chlorine (chloramines) in mg/L as Cl2. Used when a starting water has a chloramine residual.
#' @param tot_po4 Phosphate in mg/L as PO4 3-. Used when a starting water has a phosphate residual.
#' @param tot_nh3 Total ammonia in mg/L as N
#' @param tot_bo4 Total borate (B(OH)4 -) in mg/L as B
#' @param tot_sio4 Total silicate in mg/L as SiO2
#' @param tot_ch3coo Total acetate in mg/L
#' @param tot_citric Total citric acid in mg/L
#' @param tds Total Dissolved Solids in mg/L (optional if ions are known)
#' @param cond Electrical conductivity in uS/cm (optional if ions are known)
#' @param toc Total organic carbon (TOC) in mg/L
#' @param doc Dissolved organic carbon (DOC) in mg/L
#' @param uv254 UV absorbance at 254 nm (cm-1)
#' @param br Bromide in ug/L Br-
#' @param f Fluoride in mg/L F-
#' @param fe Iron in mg/L Fe3+
#' @param al Aluminum in mg/L Al3+
#' @param mn Manganese in ug/L Mn2+
#' @param no3 Nitrate in mg/L as N
#' @param no2 Nitrite in mg/L as N
#'
#' @examples
#' water_missingions <- define_water(ph = 7, temp = 15, alk = 100, tds = 10)
#' water_defined <- define_water(7, 20, 50, 100, 80, 10, 10, 10, 10, tot_po4 = 1)
#'
#' @export
#'
#' @return define_water outputs a water class object where slots are filled or calculated based on input parameters. Water slots have different units than those input into the define_water function, as listed below.
#' \describe{
#'   \item{ph}{pH, numeric, in standard units (SU).}
#'   \item{temp}{temperature, numeric, in °C.}
#'   \item{alk}{alkalinity, numeric, mg/L as CaCO3.}
#'   \item{tds}{total dissolved solids, numeric, mg/L.}
#'   \item{cond}{electrical conductivity, numeric, uS/cm.}
#'   \item{tot_hard}{total hardness, numeric, mg/L as CaCO3.}
#'   \item{kw}{dissociation constant for water, numeric, unitless.}
#'   \item{alk_eq}{total alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{carbonate_alk_eq}{carbonate alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{phosphate_alk_eq}{phosphate alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{ammonium_alk_eq}{ammonium alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{borate_alk_eq}{borate alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{silicate_alk_eq}{silicate alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{hypochlorite_alk_eq}{hypochlorite alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{citric_alk_eq}{citrate alkalinity as equivalents, numeric, equivalent (eq).}
#'   \item{toc}{total organic carbon, numeric, mg/L.}
#'   \item{doc}{dissolved organic carbon, numeric, mg/L.}
#'   \item{bdoc}{biodegradable organic carbon, numeric, mg/L.}
#'   \item{uv254}{light absorption at 254 nm, numeric, cm-1.}
#'   \item{dic}{dissolved inorganic carbon, numeric, mg/L as C.}
#'   \item{is}{ionic strength, numeric, mol/L.}
#'   \item{na}{sodium, numeric, mols/L.}
#'   \item{ca}{calcium, numeric, mols/L.}
#'   \item{mg}{magnesium, numeric, mols/L.}
#'   \item{k}{potassium, numeric, mols/L.}
#'   \item{cl}{chloride, numeric, mols/L.}
#'   \item{so4}{sulfate, numeric, mols/L.}
#'   \item{mno4}{permanganate, numeric, mols/L.}
#'   \item{no3}{nitrate, numeric, mols/L.}
#'   \item{no2}{nitrite, numeric, mols/L.}
#'   \item{h2co3}{carbonic acid (H2CO3*, the sum of dissolved CO2 and H2CO3), numeric, mols/L.}
#'   \item{co2}{dissolved carbon dioxide (H2CO3*, calculated from h2co3), numeric, mg/L as CO2.}
#'   \item{hco3}{bicarbonate, numeric, mols/L.}
#'   \item{co3}{carbonate, numeric, mols/L.}
#'   \item{h2po4}{dihydrogen phosphate, numeric, mols/L.}
#'   \item{hpo4}{hydrogen phosphate, numeric, mols/L.}
#'   \item{po4}{phosphate, numeric, mols/L.}
#'   \item{h2citric}{dihydrogen citrate, numeric, mols/L.}
#'   \item{h1citric}{hydrogen citrate, numeric, mols/L.}
#'   \item{citric}{citrate ion, numeric, mols/L.}
#'   \item{nh4}{ammonium, numeric, mol/L as N.}
#'   \item{h4bo4}{borate, numeric, mol/L.}
#'   \item{h3bo3}{boric acid, numeric, mol/L.}
#'   \item{h3sio4}{trihydrogen silicate, numeric, mol/L.}
#'   \item{h2sio4}{dihydrogen silicate, numeric, mol/L.}
#'   \item{ch3coo}{acetate, numeric, mol/L.}
#'   \item{h}{hydrogen ion, numeric, mol/L.}
#'   \item{oh}{hydroxide ion, numeric, mol/L.}
#'   \item{tot_po4}{total phosphate, numeric, mol/L.}
#'   \item{tot_nh3}{total ammonia, numeric, mol/L.}
#'   \item{tot_co3}{total carbonate, numeric, mol/L.}
#'   \item{tot_bo4}{total borate, numeric, mol/L.}
#'   \item{tot_sio4}{total silicate, numeric, mol/L.}
#'   \item{tot_ch3coo}{total acetate, numeric, mol/L.}
#'   \item{tot_citric}{total citric acid, numeric, mol/L.}
#'   \item{br}{bromide, numeric, mol/L.}
#'   \item{bro3}{bromate, numeric, mol/L.}
#'   \item{f}{fluoride, numeric, mol/L.}
#'   \item{fe}{iron, numeric, mol/L.}
#'   \item{al}{aluminum, numeric, mol/L.}
#'   \item{mn}{manganese, numeric, mol/L.}
#'   \item{free_chlorine}{free chlorine, numeric, mol/L.}
#'   \item{ocl}{hypochlorite ion, numeric, mol/L.}
#'   \item{combined_chlorine}{sum of chloramines, numeric, mol/L.}
#'   \item{nh2cl}{monochloramine, numeric, mol/L.}
#'   \item{nhcl2}{dichloramine, numeric, mol/L.}
#'   \item{ncl3}{trichloramine, numeric, mol/L.}
#'   \item{estimated}{list of parameters that were estimated based on other inputs.}
#'   \item{ks}{data frame with corrected equilibrium constants given defined water initial temperature and ionic strength}
#'   \item{chcl3}{chloroform, numeric, ug/L.}
#'   \item{chcl2br}{bromodichloromethane, numeric, ug/L.}
#'   \item{chbr2cl}{dibromodichloromethane, numeric, ug/L.}
#'   \item{chbr3}{bromoform, numeric, ug/L.}
#'   \item{tthm}{total trihalomethanes, numeric, ug/L.}
#'   \item{mcaa}{chloroacetic acid, numeric, ug/L.}
#'   \item{dcaa}{dichloroacetic acid, numeric, ug/L.}
#'   \item{tcaa}{trichloroacetic acid, numeric, ug/L.}
#'   \item{mbaa}{bromoacetic acid, numeric, ug/L.}
#'   \item{dbaa}{dibromoacetic acid, numeric, ug/L.}
#'   \item{haa5}{sum of haloacetic acids, numeric, ug/L.}
#'   \item{bcaa}{bromochloroacetic acid, numeric, ug/L.}
#'   \item{cdbaa}{chlorodibromoacetic acid, numeric, ug/L.}
#'   \item{dcbaa}{dichlorobromoacetic acid, numeric, ug/L.}
#'   \item{tbaa}{tribromoacetic acid, numeric, ug/L.}
#' }

define_water <- function(
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
  tot_bo4 = 0,
  tot_citric = 0,
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
  no3,
  no2
) {
  # Initialize string for tracking which parameters were estimated
  estimated <- ""

  # Handle missing arguments with warnings (not all parameters are needed for all models).
  if (missing(ph)) {
    ph <- NA_real_
    tidywater_warn("Missing value for {.arg ph}; carbonate balance will not be calculated.")
  }

  if (missing(alk)) {
    alk <- NA_real_
    tidywater_warn("Missing value for {.arg alk}; carbonate balance will not be calculated.")
  }

  tot_hard <- ifelse(missing(tot_hard), NA_real_, tot_hard)
  ca <- ifelse(missing(ca), NA_real_, ca)
  mg <- ifelse(missing(mg), NA_real_, mg)


  if (!is.na(tot_hard) & is.na(ca) & !is.na(mg)) {
    ca <- convert_units(tot_hard - convert_units(mg, "mg", "mg/L", "mg/L CaCO3"), "ca", "mg/L CaCO3", "mg/L")
    tidywater_warn("Missing value for {.arg ca}; estimated from total hardness and magnesium.")
    estimated <- paste(estimated, "ca", sep = "_")
  }

  if (!is.na(tot_hard) & is.na(mg) & !is.na(ca)) {
    mg <- convert_units(tot_hard - convert_units(ca, "ca", "mg/L", "mg/L CaCO3"), "mg", "mg/L CaCO3", "mg/L")
    tidywater_warn("Missing value for {.arg mg}; estimated from total hardness and calcium.")
    estimated <- paste(estimated, "mg", sep = "_")
  }

  if (!is.na(tot_hard) & is.na(mg) & is.na(ca)) {
    ca <- convert_units(tot_hard * 0.65, "ca", "mg/L CaCO3", "mg/L")
    mg <- convert_units(tot_hard * 0.35, "mg", "mg/L CaCO3", "mg/L")
    tidywater_warn("Missing values for {.arg ca} and {.arg mg}; estimated from total hardness using a 65%/35% Ca/Mg split.")
    estimated <- paste(estimated, "ca", sep = "_")
    estimated <- paste(estimated, "mg", sep = "_")
  }

  if (is.na(tot_hard) & !is.na(ca) & is.na(mg)) {
    tot_hard <- calculate_hardness(ca, 0) / .65
    mg <- convert_units(tot_hard - convert_units(ca, "ca", "mg/L", "mg/L CaCO3"), "mg", "mg/L CaCO3", "mg/L")
    tidywater_warn("Missing values for {.arg mg} and {.arg tot_hard}; estimated from calcium using a 65%/35% Ca/Mg split.")
    estimated <- paste(estimated, "tothard", sep = "_")
    estimated <- paste(estimated, "mg", sep = "_")
  } else if (is.na(tot_hard) & !is.na(ca) & !is.na(mg)) {
    tot_hard <- calculate_hardness(ca, mg)
  }

  tds <- ifelse(missing(tds), NA_real_, tds)
  cond <- ifelse(missing(cond), NA_real_, cond)

  # Convert ion concentration inputs to mol/L and fill missing arguments with NA
  ca <- convert_units(ca, "ca")
  mg <- convert_units(mg, "mg")
  na <- ifelse(missing(na), NA_real_, convert_units(na, "na"))
  k <- ifelse(missing(k), NA_real_, convert_units(k, "k"))
  cl <- ifelse(missing(cl), NA_real_, convert_units(cl, "cl"))
  so4 <- ifelse(missing(so4), NA_real_, convert_units(so4, "so4"))
  mno4 <- ifelse(missing(mno4), NA_real_, convert_units(mno4, "mno4"))
  tot_po4 <- convert_units(tot_po4, "po4")
  free_chlorine <- convert_units(free_chlorine, "cl2")
  combined_chlorine <- convert_units(combined_chlorine, "cl2")
  tot_nh3 <- convert_units(tot_nh3, "n")
  tot_bo4 <- convert_units(tot_bo4, "b")
  tot_sio4 <- convert_units(tot_sio4, "sio2")
  tot_ch3coo <- convert_units(tot_ch3coo, "ch3cooh")
  tot_citric <- convert_units(tot_citric, "citric")

  br <- ifelse(missing(br), NA_real_, convert_units(br, "br", "ug/L", "M"))
  f <- ifelse(missing(f), NA_real_, convert_units(f, "f"))
  fe <- ifelse(missing(fe), NA_real_, convert_units(fe, "fe"))
  al <- ifelse(missing(al), NA_real_, convert_units(al, "al"))
  mn <- ifelse(missing(mn), NA_real_, convert_units(mn, "mn", "ug/L", "M"))
  no3 <- ifelse(missing(no3), NA_real_, convert_units(no3, "no3", "mg/L N", "M"))
  no2 <- ifelse(missing(no2), NA_real_, convert_units(no2, "no2", "mg/L N", "M"))

  if (missing(toc) & missing(doc) & missing(uv254)) {
    toc <- NA_real_
    doc <- NA_real_
    uv254 <- NA_real_
  } else if (missing(toc) & missing(doc)) {
    toc <- NA_real_
    doc <- NA_real_
  } else if (missing(toc) & !missing(doc)) {
    tidywater_warn("Missing value for {.arg toc}; DOC assumed to be 95% of TOC.")
    toc <- doc / 0.95
    estimated <- paste(estimated, "toc", sep = "_")
  } else if (missing(doc) & !missing(toc)) {
    tidywater_warn("Missing value for {.arg doc}; defaulting to 95% of TOC.")
    doc <- toc * 0.95
    estimated <- paste(estimated, "doc", sep = "_")
  }

  uv254 <- ifelse(missing(uv254), NA_real_, uv254)

  # Calculate temperature dependent constants

  kw <- K_temp_adjust(discons["water", ]$deltah, discons["water", ]$k, temp)

  h <- 10^-ph # assume activity = concentration to start
  oh <- kw / h # assume activity = concentration to start

  # convert alkalinity input to equivalents/L
  alk_eq <- convert_units(alk, "caco3", startunit = "mg/L CaCO3", endunit = "eq/L")
  # Initial alpha values (not corrected for IS)
  discons <- tidywater::discons

  k1co3 <- K_temp_adjust(discons["k1co3", ]$deltah, discons["k1co3", ]$k, temp)
  k2co3 <- K_temp_adjust(discons["k2co3", ]$deltah, discons["k2co3", ]$k, temp)
  k1po4 <- K_temp_adjust(discons["k1po4", ]$deltah, discons["k1po4", ]$k, temp)
  k2po4 <- K_temp_adjust(discons["k2po4", ]$deltah, discons["k2po4", ]$k, temp)
  k3po4 <- K_temp_adjust(discons["k3po4", ]$deltah, discons["k3po4", ]$k, temp)
  kocl <- K_temp_adjust(discons["kocl", ]$deltah, discons["kocl", ]$k, temp)
  knh4 <- K_temp_adjust(discons["knh4", ]$deltah, discons["knh4", ]$k, temp)
  kbo4 <- K_temp_adjust(discons["kbo4", ]$deltah, discons["kbo4", ]$k, temp)
  k1sio4 <- K_temp_adjust(discons["k1sio4", ]$deltah, discons["k1sio4", ]$k, temp)
  k2sio4 <- K_temp_adjust(discons["k2sio4", ]$deltah, discons["k2sio4", ]$k, temp)
  k1citric <- K_temp_adjust(discons["k1citric", ]$deltah, discons["k1citric", ]$k, temp)
  k2citric <- K_temp_adjust(discons["k2citric", ]$deltah, discons["k2citric", ]$k, temp)
  k3citric <- K_temp_adjust(discons["k3citric", ]$deltah, discons["k3citric", ]$k, temp)

  alpha0 <- calculate_alpha0_carbonate(h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as H2CO3
  alpha1 <- calculate_alpha1_carbonate(h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as HCO3-
  alpha2 <- calculate_alpha2_carbonate(h, data.frame("k1co3" = k1co3, "k2co3" = k2co3)) # proportion of total carbonate as CO32-

  alpha1p <- calculate_alpha1_phosphate(h, data.frame("k1po4" = k1po4, "k2po4" = k2po4, "k3po4" = k3po4)) # proportion of total phosphate as H2PO4-
  alpha2p <- calculate_alpha2_phosphate(h, data.frame("k1po4" = k1po4, "k2po4" = k2po4, "k3po4" = k3po4)) # proportion of total phosphate as HPO4 2-
  alpha3p <- calculate_alpha3_phosphate(h, data.frame("k1po4" = k1po4, "k2po4" = k2po4, "k3po4" = k3po4)) # proportion of total phosphate as PO4 3-

  alpha1citric <- calculate_alpha1_citric(h, data.frame("k1citric" = k1citric, "k2citric" = k2citric, "k3citric" = k3citric)) # proportion of total citric acid as H2Citric-
  alpha2citric <- calculate_alpha2_citric(h, data.frame("k1citric" = k1citric, "k2citric" = k2citric, "k3citric" = k3citric)) # proportion of total citric acid as HCitric 2-
  alpha3citric <- calculate_alpha3_citric(h, data.frame("k1citric" = k1citric, "k2citric" = k2citric, "k3citric" = k3citric)) # proportion of total citric acid as Citric 3-

  alpha1c <- calculate_alpha1_hypochlorite(h, data.frame("kocl" = kocl))
  alpha1n <- calculate_alpha1_ammonia(h, data.frame("knh4" = knh4))
  alpha1b <- calculate_alpha1_borate(h, data.frame("kbo4" = kbo4))
  alpha1s <- calculate_alpha1_silicate(h, data.frame("k1sio4" = k1sio4, "k2sio4" = k2sio4))
  alpha2s <- calculate_alpha2_silicate(h, data.frame("k1sio4" = k1sio4, "k2sio4" = k2sio4))

  # Update total ion values
  h2po4 <- tot_po4 * alpha1p
  hpo4 <- tot_po4 * alpha2p
  po4 <- tot_po4 * alpha3p
  h3po4 <- tot_po4 - (h2po4 + hpo4 + po4)

  h2citric <- tot_citric * alpha1citric
  h1citric <- tot_citric * alpha2citric
  citric <- tot_citric * alpha3citric
  h3citric <- tot_citric - (h2citric + h1citric + citric)

  ocl <- free_chlorine * alpha1c
  nh4 <- tot_nh3 * alpha1n

  h4bo4 <- tot_bo4 * alpha1b
  h3bo3 <- tot_bo4 - h4bo4
  h3sio4 <- tot_sio4 * alpha1s
  h2sio4 <- tot_sio4 * alpha2s

  phosphate_alk_eq <- (-1 * h3po4 + 0 * h2po4 + 1 * hpo4 + 2 * po4)
  citric_alk_eq <- (-1 * h3citric + 0 * h2citric + 1 * h1citric + 2 * citric)

  hypochlorite_alk_eq <- (1 * ocl)
  ammonium_alk_eq <- (1 * nh4)
  borate_alk_eq <- (1 * h4bo4)
  silicate_alk_eq <- (1 * h3sio4 + 2 * h2sio4)
  carbonate_alk_eq <- alk_eq -
    (ammonium_alk_eq + borate_alk_eq + phosphate_alk_eq + silicate_alk_eq + hypochlorite_alk_eq + oh + citric_alk_eq) +
    h

  tot_co3 <- carbonate_alk_eq / (alpha1 + 2 * alpha2)

  # Initialize water to simplify IS calcs
  water <- methods::new(
    "water",
    ph = ph,
    temp = temp,
    alk = alk,
    tds = tds,
    cond = cond,
    tot_hard = tot_hard,
    na = na,
    ca = ca,
    mg = mg,
    k = k,
    cl = cl,
    so4 = so4,
    mno4 = mno4,
    h2co3 = tot_co3 * alpha0,
    hco3 = tot_co3 * alpha1,
    co3 = tot_co3 * alpha2,
    h2po4 = h2po4,
    hpo4 = hpo4,
    po4 = po4,
    ocl = ocl,
    nh4 = nh4,
    h4bo4 = h4bo4,
    h3bo3 = h3bo3,
    h3sio4 = h3sio4,
    h2sio4 = h2sio4,
    h = h,
    oh = oh,
    tot_po4 = tot_po4,
    h2citric = h2citric,
    h1citric = h1citric,
    citric = citric,
    free_chlorine = free_chlorine,
    combined_chlorine = combined_chlorine,
    tot_nh3 = tot_nh3,
    tot_co3 = tot_co3,
    tot_bo4 = tot_bo4,
    tot_sio4 = tot_sio4,
    tot_ch3coo = tot_ch3coo,
    tot_citric = tot_citric,
    kw = kw,
    is = 0,
    alk_eq = alk_eq,
    doc = doc,
    toc = toc,
    uv254 = uv254,
    br = br,
    f = f,
    fe = fe,
    al = al,
    mn = mn,
    no3 = no3,
    no2 = no2
  )

  # Determine ionic strength
  if (!is.na(tds)) {
    water@is <- correlate_ionicstrength(tds, from = "tds")
    water@cond <- correlate_ionicstrength(tds, from = "tds", to = "cond")
    estimated <- paste(estimated, "cond", sep = "_")
  } else if (!is.na(cond)) {
    water@is <- correlate_ionicstrength(cond, from = "cond")
    water@tds <- correlate_ionicstrength(cond, from = "cond", to = "tds")
    estimated <- paste(estimated, "tds", sep = "_")
  } else if (
    is.na(tds) & is.na(cond) & ((!is.na(ca) | !is.na(na)) & (!is.na(cl) | !is.na(so4)) & alk > 0) & !is.na(ph)
  ) {
    water@is <- calculate_ionicstrength(water)
    water@tds <- correlate_ionicstrength(water@is, from = "is", to = "tds")
    estimated <- paste(estimated, "tds", sep = "_")
    water@cond <- correlate_ionicstrength(water@is, from = "is", to = "cond")
    estimated <- paste(estimated, "cond", sep = "_")
  } else {
    tidywater_warn("Major ions, TDS, and conductivity are all missing; assuming ideal conditions. Ionic strength will be {.val NA} and activity coefficients will be 1.")
    water@is <- NA_real_
  }

  # Eq constants
  water@ks <- correct_k(water)

  # Recalculate H and OH concentration (not activity)
  gamma1 <- calculate_activity(1, water@is, water@temp)
  h <- h / gamma1
  oh <- oh / gamma1
  water@h <- h
  water@oh <- oh

  # Carbonate and phosphate ions and ocl ions
  alpha0 <- calculate_alpha0_carbonate(h, water@ks)
  alpha1 <- calculate_alpha1_carbonate(h, water@ks) # proportion of total carbonate as HCO3-
  alpha2 <- calculate_alpha2_carbonate(h, water@ks) # proportion of total carbonate as CO32-

  alpha0p <- calculate_alpha0_phosphate(h, water@ks)
  alpha1p <- calculate_alpha1_phosphate(h, water@ks)
  alpha2p <- calculate_alpha2_phosphate(h, water@ks)
  alpha3p <- calculate_alpha3_phosphate(h, water@ks)

  alpha0citric <- calculate_alpha0_citric(h, water@ks)
  alpha1citric <- calculate_alpha1_citric(h, water@ks)
  alpha2citric <- calculate_alpha2_citric(h, water@ks)
  alpha3citric <- calculate_alpha3_citric(h, water@ks)

  water@h2po4 <- tot_po4 * alpha1p
  water@hpo4 <- tot_po4 * alpha2p
  water@po4 <- tot_po4 * alpha3p
  h3po4 <- tot_po4 * alpha0p

  water@h2citric <- tot_citric * alpha1citric
  water@h1citric <- tot_citric * alpha2citric
  water@citric <- tot_citric * alpha3citric
  h3citric <- tot_citric * alpha0citric

  water@ocl <- free_chlorine * calculate_alpha1_hypochlorite(h, water@ks)
  water@nh4 <- tot_nh3 * calculate_alpha1_ammonia(h, water@ks)
  water@ch3coo <- tot_ch3coo * calculate_alpha1_acetate(h, water@ks)

  water@h4bo4 <- tot_bo4 * calculate_alpha1_borate(h, water@ks)
  water@h3bo3 <- tot_bo4 - water@h4bo4
  water@h3sio4 <- tot_sio4 * calculate_alpha1_silicate(h, water@ks)
  water@h2sio4 <- tot_sio4 * calculate_alpha2_silicate(h, water@ks)

  # Calculate individual and total alkalinity
  water@phosphate_alk_eq <- (-1 * h3po4 + 0 * water@h2po4 + 1 * water@hpo4 + 2 * water@po4)
  water@citric_alk_eq <- (-1 * h3citric + 0 * water@h2citric + 1 * water@h1citric + 2 * water@citric)
  water@hypochlorite_alk_eq <- (1 * water@ocl)
  water@ammonium_alk_eq <- (1 * water@nh4)
  water@borate_alk_eq <- (1 * water@h4bo4)
  water@silicate_alk_eq <- (1 * water@h3sio4 + 2 * water@h2sio4)
  water@carbonate_alk_eq <- alk_eq -
    (water@ammonium_alk_eq +
      water@borate_alk_eq +
      water@phosphate_alk_eq +
      water@citric_alk_eq +
      water@silicate_alk_eq +
      water@hypochlorite_alk_eq +
      water@oh) +
    water@h

  water@tot_co3 <- water@carbonate_alk_eq / (alpha1 + 2 * alpha2)
  water@h2co3 <- water@tot_co3 * alpha0
  water@co2 <- water@h2co3 * tidywater::mweights$co2 * 1000
  water@hco3 <- water@tot_co3 * alpha1
  water@co3 <- water@tot_co3 * alpha2
  water@dic <- water@tot_co3 * tidywater::mweights$dic * 1000

  # Add all estimated values to water slot
  water@estimated <- estimated

  return(water)
}

#' Apply `define_water` within a dataframe and output a column of `water` class to be chained to other tidywater functions
#'
#' This function allows [define_water] to be added to a piped data frame.
#' Its output is a `water` class, and can therefore be chained with "downstream" tidywater functions.
#'@rdname define_water
#' @param df a data frame containing columns with all the desired parameters with column names matching argument names in define_water
#' @param output_water name of the output column storing updated parameters with the class, water. Default is "defined".
#' @param pluck_cols Extract primary water slots (ph, alk, doc, uv254) into new numeric columns for easy access. Default to FALSE.
#' @param water_prefix Append the output_water name to the start of the plucked columns. Default is TRUE.
#'
#' @seealso [define_water]
#'
#' @examples
#' \donttest{
#' example_df <- water_df %>%
#'   define_water_df() %>%
#'   balance_ions_df()
#'
#' example_df <- water_df %>%
#'   define_water_df(output_water = "This is a column of water") %>%
#'   balance_ions_df(input_water = "This is a column of water")
#' }
#'
#' @export
#' @returns A data frame containing a water class column.

define_water_df <- function(
  df,
  output_water = "defined",
  pluck_cols = FALSE,
  water_prefix = TRUE,
  ph= "use_col",
  temp = "use_col",
  alk = "use_col",
  tot_hard = "use_col",
  ca = "use_col",
  mg = "use_col",
  na = "use_col",
  k = "use_col",
  cl = "use_col",
  so4 = "use_col",
  mno4 = "use_col",
  free_chlorine = "use_col",
  combined_chlorine = "use_col",
  tot_po4 = "use_col",
  tot_nh3 = "use_col",
  tot_bo4 = "use_col",
  tot_sio4 = "use_col",
  tot_ch3coo = "use_col",
  tot_citric = "use_col",
  tds = "use_col",
  cond = "use_col",
  toc = "use_col",
  doc = "use_col",
  uv254 = "use_col",
  br = "use_col",
  f = "use_col",
  fe = "use_col",
  al = "use_col",
  mn = "use_col",
  no3 = "use_col",
  no2 = "use_col"
) {

# Process unquoted column names
  ph <- tryCatch(ph, error = function(e) enquo(ph))
  temp <- tryCatch(temp, error = function(e) enquo(temp))
  alk <- tryCatch(alk, error = function(e) enquo(alk))
  tot_hard <- tryCatch(tot_hard, error = function(e) enquo(tot_hard))
  ca <- tryCatch(ca, error = function(e) enquo(ca))
  mg <- tryCatch(mg, error = function(e) enquo(mg))
  na <- tryCatch(na, error = function(e) enquo(na))
  k <- tryCatch(k, error = function(e) enquo(k))
  cl <- tryCatch(cl, error = function(e) enquo(cl))
  so4 <- tryCatch(so4, error = function(e) enquo(so4))
  mno4 <- tryCatch(mno4, error = function(e) enquo(mno4))
  free_chlorine <- tryCatch(free_chlorine, error = function(e) enquo(free_chlorine))
  combined_chlorine <- tryCatch(combined_chlorine, error = function(e) enquo(combined_chlorine))
  tot_po4 <- tryCatch(tot_po4, error = function(e) enquo(tot_po4))
  tot_nh3 <- tryCatch(tot_nh3, error = function(e) enquo(tot_nh3))
  tot_bo4 <- tryCatch(tot_bo4, error = function(e) enquo(tot_bo4))
  tot_sio4 <- tryCatch(tot_sio4, error = function(e) enquo(tot_sio4))
  tot_ch3coo <- tryCatch(tot_ch3coo, error = function(e) enquo(tot_ch3coo))
  tot_citric <- tryCatch(tot_citric, error = function(e) enquo(tot_citric))
  tds <- tryCatch(tds, error = function(e) enquo(tds))
  cond <- tryCatch(cond, error = function(e) enquo(cond))
  toc <- tryCatch(toc, error = function(e) enquo(toc))
  doc <- tryCatch(doc, error = function(e) enquo(doc))
  uv254 <- tryCatch(uv254, error = function(e) enquo(uv254))
  br <- tryCatch(br, error = function(e) enquo(br))
  f <- tryCatch(f, error = function(e) enquo(f))
  fe <- tryCatch(fe, error = function(e) enquo(fe))
  al <- tryCatch(al, error = function(e) enquo(al))
  mn <- tryCatch(mn, error = function(e) enquo(mn))
  no3 <- tryCatch(no3, error = function(e) enquo(no3))
  no2 <- tryCatch(no2, error = function(e) enquo(no2))

arguments <- construct_helper(
 df,
 all_args = list(
    "ph"=ph,
    "temp"=temp,
    "alk"=alk,
    "tot_hard"=tot_hard,
    "ca"=ca,
    "mg"=mg,
    "na"=na,
    "k"=k,
    "cl"=cl,
    "so4"=so4,
    "mno4"=mno4,
    "free_chlorine"=free_chlorine,
    "combined_chlorine"=combined_chlorine,
    "tot_po4"=tot_po4,
    "tot_nh3"=tot_nh3,
    "tot_bo4"=tot_bo4,
    "tot_sio4"=tot_sio4,
    "tot_ch3coo"=tot_ch3coo,
    "tot_citric"=tot_citric,
    "tds"=tds,
    "cond"=cond,
    "toc"=toc,
    "doc"=doc,
    "uv254"=uv254,
    "br"=br,
    "f"=f,
    "fe"=fe,
    "al"=al,
    "mn"=mn,
    "no3"=no3,
    "no2"=no2
  ))

  final_names <- arguments$final_names

  if (length(arguments$new_cols) > 0) {
    df <- merge(df, as.data.frame(arguments$new_cols), by = NULL)
  }



  water_input <- df[, names(df) %in% unname(final_names)]

col_map <- final_names[final_names %in% names(water_input)]
names(water_input)[match(col_map, names(water_input))] <- names(col_map)

  warning_counts<-list()
  df[[output_water]] <- lapply(seq_len(nrow(df)), function(i) {
    withCallingHandlers(do.call(define_water, water_input[i, ]),warning = function(w) {
      msg <- conditionMessage(w)
      warning_counts[[msg]] <<- (warning_counts[[msg]] %||% 0) + 1L
      invokeRestart("muffleWarning")
    }
  )
  })


  for (msg in names(warning_counts)) {
    tidywater_warn("{msg} ({warning_counts[[msg]]} row{?s} affected.)")
  }

   output <- df[, !names(df) %in% unname(final_names), drop = FALSE]

  if (pluck_cols) {
    output <- output |>
      pluck_water(c(output_water), c("ph", "alk", "doc", "uv254"))
    if (!water_prefix) {
      names(output) <- gsub(paste0(output_water, "_"), "", names(output))
    }
  }

  return(output)
}
