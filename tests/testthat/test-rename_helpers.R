test_that("_chain works", {
  expect_warning(define_water_chain(water_df))
  water0 <- suppressWarnings(
    water_df %>%
      transform(
        br = 50,
        tot_po4 = 2
      ) %>%
      dplyr::select(-free_chlorine) %>%
      define_water_chain()
  )

  expect_warning(balance_ions_chain(water0))

  expect_warning(biofilter_toc_chain(water0, ebct = 10))

  expect_warning(chemdose_chloramine_chain(water0, time = 10, cl2 = 3, nh3 = .5))

  expect_warning(chemdose_chlordecay_chain(water0, cl2_dose = 1, time = 8))

  expect_warning(chemdose_dbp_chain(water0, cl2 = 2, time = 8))

  expect_warning(chemdose_ph_chain(water0, hcl = 1))

  expect_warning(chemdose_toc_chain(water0, alum = 5))

  expect_warning(decarbonate_ph_chain(water0, co2_removed = 0.5))

  expect_warning(modify_water_chain(water0, slot = "ca", value = 20, units = "mg/L"))

  expect_warning(ozonate_bromate_chain(water0, dose = 1.5, time = 5))

  expect_warning(pac_toc_chain(water0, dose = 15, time = 50))

  expect_warning(calculate_corrosion_once(water0))

  expect_warning(chemdose_dbp_once(water0, cl2 = 2, time = 8))

  expect_warning(chemdose_ph_once(water0, hcl = 1))

  expect_warning(chemdose_toc_once(water0, alum = 5))

  expect_warning(dissolve_cu_once(water0))

  expect_warning(dissolve_pb_once(water0))

  expect_warning(
    water0 %>%
      dplyr::slice(1) %>%
      solvect_chlorine_once(time = 30, residual = 1, baffle = 0.7)
  )

  expect_warning(solvect_o3_once(water0, time = 10, dose = 2, kd = -0.5, baffle = 0.9))

  expect_warning(
    water0 %>%
      dplyr::slice(1) %>%
      solvedose_alk_once(target_alk = 150, chemical = "naoh")
  )

  expect_warning(solvedose_ph_once(water0, target_ph = 8.5, chemical = "caoh2"))

  expect_warning(solveresid_o3_once(water0, dose = 2, time = 10))

  water1 <- suppressWarnings(chemdose_ph_chain(water0, hcl = 1))
  expect_warning(blend_waters_chain(water1, waters = c("defined_water", "dosed_chem_water"), ratios = c(0.5, 0.5)))
})

test_that("deprecated wrappers pass arguments to the matching _df parameters", {
  testthat::skip_on_cran()
  water0 <- water_df %>%
    dplyr::slice(1:3) %>%
    transform(br = 50) %>%
    define_water_df("defined_water")

  toc_once <- suppressWarnings(chemdose_toc_once(water0, alum = 30))
  toc_df <- chemdose_toc_df(
    water0,
    "defined_water",
    "coagulated_water",
    pluck_cols = TRUE,
    water_prefix = FALSE,
    alum = 30
  )
  expect_equal(toc_once, toc_df)
  expect_false(isTRUE(all.equal(toc_once$doc, water0$defined_water[[1]]@doc)))

  toc_chain <- suppressWarnings(chemdose_toc_chain(water0, alum = 30, coeff = "Ferric"))
  toc_chain_df <- chemdose_toc_df(
    water0,
    "defined_water",
    "coagulated_water",
    keep_dose_cols = TRUE,
    alum = 30,
    coeff = "Ferric"
  )
  expect_equal(toc_chain, toc_chain_df)

  ph_once <- suppressWarnings(chemdose_ph_once(water0, hcl = 10, naoh = 5))
  ph_df <- chemdose_ph_df(
    water0,
    "defined_water",
    "dosed_chem_water",
    na_to_zero = FALSE,
    pluck_cols = TRUE,
    water_prefix = FALSE,
    hcl = 10,
    naoh = 5,
    softening_correction = FALSE
  )
  expect_equal(ph_once, ph_df)

  dbp_prefix <- suppressWarnings(chemdose_dbp_once(water0, cl2 = 2, time = 8))
  dbp_noprefix <- suppressWarnings(chemdose_dbp_once(water0, cl2 = 2, time = 8, water_prefix = FALSE))
  expect_equal(
    dbp_prefix,
    chemdose_dbp_df(water0, "defined_water", "disinfected_water", pluck_cols = TRUE, keep_dose_cols = TRUE, cl2 = 2, time = 8)
  )
  expect_false(identical(names(dbp_prefix), names(dbp_noprefix)))
})
