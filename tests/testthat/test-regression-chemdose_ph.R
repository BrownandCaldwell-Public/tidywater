# Regression pins: chemdose_ph ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.
# These are characterization tests: they pin current behavior, including known bugs.

# One realistic dose (mg/L as chemical, or as noted in ?chemdose_ph) per argument.
regression_single_doses <- function() {
  list(
    hcl = list(hcl = 10),
    h2so4 = list(h2so4 = 15),
    h3po4 = list(h3po4 = 5),
    hno3 = list(hno3 = 8),
    # KNOWN-BUG #521: acetic acid TOC/DOC change is lost
    ch3cooh_knownbug521 = list(ch3cooh = 10),
    citric = list(citric = 5),
    co2 = list(co2 = 10),
    naoh = list(naoh = 10),
    caoh2 = list(caoh2 = 15),
    mgoh2 = list(mgoh2 = 10),
    na2co3 = list(na2co3 = 20),
    nahco3 = list(nahco3 = 20),
    caco3_add = list(caco3 = 20),
    caco3_remove = list(caco3 = -10),
    caso4 = list(caso4 = 20),
    caocl2 = list(caocl2 = 3),
    cacl2 = list(cacl2 = 20),
    cl2 = list(cl2 = 3),
    naocl = list(naocl = 3),
    nh4oh = list(nh4oh = 1),
    nh42so4 = list(nh42so4 = 1),
    alum = list(alum = 30),
    ferricchloride = list(ferricchloride = 25),
    ferricsulfate = list(ferricsulfate = 30),
    ach = list(ach = 20),
    pacl = list(pacl = 10),
    kmno4 = list(kmno4 = 1),
    naf = list(naf = 1),
    na3po4 = list(na3po4 = 3)
  )
}

dose_all_single <- function(water) {
  lapply(regression_single_doses(), function(d) {
    suppressWarnings(do.call(chemdose_ph, c(list(water), d)))
  })
}

test_that("regression: chemdose_ph every chemical, typical water", {
  testthat::skip_on_cran()
  expect_golden(dose_all_single(regression_waters()$typical), "chemdose_ph_single_typical")
})

test_that("regression: chemdose_ph every chemical, low_alk water", {
  testthat::skip_on_cran()
  expect_golden(dose_all_single(regression_waters()$low_alk), "chemdose_ph_single_low_alk")
})

test_that("regression: chemdose_ph every chemical, hard_high_alk water", {
  testthat::skip_on_cran()
  expect_golden(dose_all_single(regression_waters()$hard_high_alk), "chemdose_ph_single_hard_high_alk")
})

test_that("regression: chemdose_ph every chemical, ideal water (IS = NA)", {
  testthat::skip_on_cran()
  expect_golden(dose_all_single(regression_waters()$ideal), "chemdose_ph_single_ideal")
})

test_that("regression: chemdose_ph every chemical, cold water", {
  testthat::skip_on_cran()
  expect_golden(dose_all_single(regression_waters()$cold), "chemdose_ph_single_cold")
})

test_that("regression: chemdose_ph zero dose on every scenario water", {
  testthat::skip_on_cran()
  out <- lapply(regression_waters(), function(w) suppressWarnings(chemdose_ph(w)))
  expect_golden(out, "chemdose_ph_zero_dose")
})

test_that("regression: chemdose_ph multi-chemical combinations", {
  testthat::skip_on_cran()
  w <- regression_waters()
  expect_golden(
    suppressWarnings(list(
      typical_hcl_alum = chemdose_ph(w$typical, hcl = 5, alum = 30),
      typical_naoh_co2 = chemdose_ph(w$typical, naoh = 10, co2 = 5),
      typical_finishing = chemdose_ph(w$typical, naocl = 2, naf = 0.8, na3po4 = 2, naoh = 3),
      typical_cl2_nh4oh = chemdose_ph(w$typical, cl2 = 4, nh4oh = 1),
      low_alk_lime_co2 = chemdose_ph(w$low_alk, caoh2 = 20, co2 = 15),
      low_alk_fecl3_naoh = chemdose_ph(w$low_alk, ferricchloride = 20, naoh = 8),
      hard_lime_soda = chemdose_ph(w$hard_high_alk, caoh2 = 150, na2co3 = 50),
      hard_h2so4_kmno4 = chemdose_ph(w$hard_high_alk, h2so4 = 30, kmno4 = 2),
      high_toc_ferricsulfate_caoh2 = chemdose_ph(w$high_toc, ferricsulfate = 60, caoh2 = 20),
      high_toc_ach_pacl = chemdose_ph(w$high_toc, ach = 15, pacl = 10),
      warm_hno3_h3po4 = chemdose_ph(w$warm, hno3 = 5, h3po4 = 3),
      ideal_everything = chemdose_ph(
        w$ideal,
        hcl = 2, h2so4 = 2, h3po4 = 1, hno3 = 1, citric = 1, co2 = 2, naoh = 5, caoh2 = 3,
        mgoh2 = 1, na2co3 = 3, nahco3 = 3, caco3 = 2, caso4 = 2, caocl2 = 1, cacl2 = 2,
        cl2 = 1, naocl = 1, alum = 5, ferricchloride = 5, ferricsulfate = 5, ach = 3, pacl = 2,
        kmno4 = 0.5, naf = 0.5, na3po4 = 1
      ),
      sequential_typical = chemdose_ph(chemdose_ph(chemdose_ph(w$typical, alum = 25), caoh2 = 10), naocl = 2)
    )),
    "chemdose_ph_combos"
  )
})

test_that("regression: chemdose_ph hcl dose sweep", {
  testthat::skip_on_cran()
  w <- regression_waters()$typical
  doses <- c(0.5, 2, 5, 20, 50, 100)
  out <- lapply(doses, function(d) suppressWarnings(chemdose_ph(w, hcl = d)))
  names(out) <- paste0("hcl_", doses)
  expect_golden(out, "chemdose_ph_hcl_sweep")
})

test_that("regression: chemdose_ph softening correction", {
  testthat::skip_on_cran()
  hard <- regression_waters()$hard_high_alk
  doc_water <- suppressWarnings(define_water(ph = 7, temp = 25, alk = 100, tot_hard = 350))
  expect_golden(
    suppressWarnings(list(
      hard_remove100_corrected = chemdose_ph(hard, caco3 = -100, softening_correction = TRUE),
      hard_remove100_uncorrected = chemdose_ph(hard, caco3 = -100, softening_correction = FALSE),
      hard_remove200_corrected = chemdose_ph(hard, caco3 = -200, softening_correction = TRUE),
      hard_remove200_uncorrected = chemdose_ph(hard, caco3 = -200, softening_correction = FALSE),
      hard_lime_remove_corrected = chemdose_ph(hard, caoh2 = 100, caco3 = -150, softening_correction = TRUE),
      hard_add_corrected_ignored = chemdose_ph(hard, caco3 = 50, softening_correction = TRUE),
      doc_example_corrected = chemdose_ph(doc_water, caco3 = -100, softening_correction = TRUE)
    )),
    "chemdose_ph_softening"
  )
})

test_that("regression: chemdose_ph TDS < 0 clamp", {
  testthat::skip_on_cran()
  low_tds <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100, tot_hard = 150, tds = 20))
  low_cond <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 100, tot_hard = 150, cond = 30))
  expect_golden(
    suppressWarnings(list(
      tds_clamped = chemdose_ph(low_tds, caco3 = -100),
      tds_clamped_corrected = chemdose_ph(low_tds, caco3 = -100, softening_correction = TRUE),
      cond_clamped = chemdose_ph(low_cond, caco3 = -80),
      tds_not_clamped = chemdose_ph(low_tds, caco3 = -5)
    )),
    "chemdose_ph_tds_clamp"
  )
})

test_that("regression: chemdose_ph waters with existing residuals", {
  testthat::skip_on_cran()
  nut <- regression_waters()$nutrients
  orgacid <- suppressWarnings(define_water(
    ph = 7, temp = 20, alk = 60, tds = 200, ca = 20, mg = 5, na = 15, cl = 20, so4 = 20,
    tot_ch3coo = 3, tot_citric = 2
  ))
  chloraminated <- suppressWarnings(define_water(
    ph = 8, temp = 15, alk = 90, tds = 250, tot_nh3 = 0.5, combined_chlorine = 2, free_chlorine = 0.2
  ))
  expect_golden(
    suppressWarnings(list(
      nutrients_hcl = chemdose_ph(nut, hcl = 10),
      nutrients_naoh = chemdose_ph(nut, naoh = 10),
      nutrients_cl2 = chemdose_ph(nut, cl2 = 3),
      nutrients_nh4oh = chemdose_ph(nut, nh4oh = 1),
      nutrients_h3po4 = chemdose_ph(nut, h3po4 = 3),
      nutrients_na3po4 = chemdose_ph(nut, na3po4 = 3),
      nutrients_citric = chemdose_ph(nut, citric = 3),
      orgacid_hcl = chemdose_ph(orgacid, hcl = 10),
      orgacid_naoh = chemdose_ph(orgacid, naoh = 10),
      # KNOWN-BUG #535: waters with acetate do not round-trip pH, even at zero dose.
      orgacid_citric = chemdose_ph(orgacid, citric = 5),
      # KNOWN-BUG #521: acetic acid TOC/DOC change is lost
      orgacid_ch3cooh_knownbug521 = chemdose_ph(orgacid, ch3cooh = 5),
      chloraminated_naoh = chemdose_ph(chloraminated, naoh = 5),
      chloraminated_nh42so4 = chemdose_ph(chloraminated, nh42so4 = 0.5)
    )),
    "chemdose_ph_residuals"
  )
})

test_that("regression: chemdose_ph_df with plucked columns", {
  testthat::skip_on_cran()
  out <- suppressWarnings(
    water_df %>%
      dplyr::slice_head(n = 4) %>%
      define_water_df() %>%
      dplyr::mutate(hcl = c(2, 4, 6, 8)) %>%
      chemdose_ph_df(naoh = 3, pluck_cols = TRUE)
  )
  expect_golden(list(df = out), "chemdose_ph_df")
})
