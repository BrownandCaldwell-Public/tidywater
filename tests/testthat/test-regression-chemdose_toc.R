# Regression pins: chemdose_toc ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.

toc_water <- function(ph, doc = 3.5, uv254 = 0.1, toc = doc + 0.2) {
  suppressWarnings(define_water(
    ph = ph, temp = 20, alk = 60, tot_hard = 80, na = 15, cl = 20, so4 = 20, tds = 200,
    toc = toc, doc = doc, uv254 = uv254
  ))
}

toc_coag_grid <- function(coag, dose = 30) {
  coeffs <- c("Alum", "Ferric", "General Alum", "General Ferric", "Low DOC")
  grid <- expand.grid(coeff = coeffs, ph = c(6, 7, 8, 9), stringsAsFactors = FALSE)
  out <- lapply(seq_len(nrow(grid)), function(i) {
    args <- list(water = toc_water(grid$ph[i]), coeff = grid$coeff[i])
    args[[coag]] <- dose
    suppressWarnings(do.call(chemdose_toc, args))
  })
  names(out) <- paste0(coag, "_", gsub(" ", "", grid$coeff), "_ph", grid$ph)
  out
}

test_that("regression: chemdose_toc alum x coeff x pH", {
  expect_golden(toc_coag_grid("alum"), "chemdose_toc_alum_grid")
})

test_that("regression: chemdose_toc ferricchloride x coeff x pH", {
  expect_golden(toc_coag_grid("ferricchloride"), "chemdose_toc_ferricchloride_grid")
})

test_that("regression: chemdose_toc ferricsulfate x coeff x pH", {
  expect_golden(toc_coag_grid("ferricsulfate"), "chemdose_toc_ferricsulfate_grid")
})

test_that("regression: chemdose_toc ach x coeff x pH", {
  expect_golden(toc_coag_grid("ach"), "chemdose_toc_ach_grid")
})

test_that("regression: chemdose_toc pacl x coeff x pH", {
  expect_golden(toc_coag_grid("pacl"), "chemdose_toc_pacl_grid")
})

test_that("regression: chemdose_toc custom coeff, SUVA cap, no coagulant, mixed coagulants", {
  custom <- data.frame(k1 = -0.05, k2 = 0.6, x1 = 150, x2 = -40, x3 = 3, b = 0.1)
  high_suva <- function(ph) toc_water(ph, doc = 2, uv254 = 0.15) # SUVA = 7.5 -> capped at 5
  suva5 <- function(ph) toc_water(ph, doc = 2, uv254 = 0.1) # SUVA exactly 5
  expect_golden(
    suppressWarnings(list(
      custom_alum_ph6 = chemdose_toc(toc_water(6), alum = 30, coeff = custom),
      custom_alum_ph7 = chemdose_toc(toc_water(7), alum = 30, coeff = custom),
      custom_ferric_ph8 = chemdose_toc(toc_water(8), ferricchloride = 30, coeff = custom),
      custom_ferric_ph9 = chemdose_toc(toc_water(9), ferricchloride = 30, coeff = custom),
      highsuva_alum_ph6 = chemdose_toc(high_suva(6), alum = 30),
      highsuva_alum_ph7 = chemdose_toc(high_suva(7), alum = 30),
      highsuva_ferric_ph7 = chemdose_toc(high_suva(7), ferricchloride = 30, coeff = "Ferric"),
      highsuva_alum_ph9 = chemdose_toc(high_suva(9), alum = 30),
      suva5_alum_ph7 = chemdose_toc(suva5(7), alum = 30),
      lowsuva_alum_ph7 = chemdose_toc(toc_water(7, doc = 4, uv254 = 0.04), alum = 30),
      lowsuva_lowdoc_ph7 = chemdose_toc(toc_water(7, doc = 1.5, uv254 = 0.02), alum = 20, coeff = "Low DOC"),
      nocoag_ph7 = chemdose_toc(toc_water(7)),
      nocoag_ph9 = chemdose_toc(toc_water(9)),
      alum_and_ferric_ph7 = chemdose_toc(toc_water(7), alum = 15, ferricchloride = 15, coeff = "General Alum"),
      all_coag_ph6.5 = chemdose_toc(
        toc_water(6.5), alum = 5, ferricchloride = 5, ferricsulfate = 5, ach = 5, pacl = 5,
        coeff = "General Ferric"
      ),
      alum_dose5_ph7 = chemdose_toc(toc_water(7), alum = 5),
      alum_dose100_ph7 = chemdose_toc(toc_water(7), alum = 100)
    )),
    "chemdose_toc_custom_suva"
  )
})

test_that("regression: chemdose_toc caoh2 softening branch", {
  expect_golden(
    suppressWarnings(list(
      caoh2_only_ph7 = chemdose_toc(toc_water(7), caoh2 = 50),
      caoh2_only_ph10 = chemdose_toc(toc_water(10), caoh2 = 50),
      caoh2_alum_ph7 = chemdose_toc(toc_water(7), alum = 20, caoh2 = 50),
      caoh2_alum_ph10 = chemdose_toc(toc_water(10), alum = 20, caoh2 = 100),
      caoh2_ferric_ph10 = chemdose_toc(toc_water(10), ferricchloride = 20, caoh2 = 100, coeff = "Ferric"),
      caoh2_ferricsulfate_ph9 = chemdose_toc(toc_water(9), ferricsulfate = 10, caoh2 = 20),
      caoh2_ach_pacl_ph8 = chemdose_toc(toc_water(8), ach = 10, pacl = 10, caoh2 = 30),
      caoh2_alum_highsuva_ph10 = chemdose_toc(toc_water(10, doc = 2, uv254 = 0.15), alum = 20, caoh2 = 50),
      caoh2_alum_lowdose_ph10 = chemdose_toc(toc_water(10), alum = 2, caoh2 = 5)
    )),
    "chemdose_toc_caoh2"
  )
})
