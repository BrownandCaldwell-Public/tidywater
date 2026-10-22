# Regression pins: dissolve_pb ----
# Golden files pin the controlling solid and total dissolved lead (M). See tests/testthat/README.md.

pb_water <- function(ph = 7.5, alk = 50, tot_po4 = 0, cl = 20, so4 = 20, temp = 20, tds = NA_real_) {
  suppressWarnings(define_water(
    ph = ph, temp = temp, alk = alk, ca = 20, mg = 5, na = 20, cl = cl, so4 = so4,
    tot_po4 = tot_po4, tds = tds
  ))
}

test_that("regression: dissolve_pb pH x DIC x phosphate sweep", {
  testthat::skip_on_cran()
  grid <- expand.grid(ph = c(6, 7, 8, 9, 10.5), alk = c(5, 50, 300), po4 = c(0, 1, 4))
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(dissolve_pb(pb_water(ph = grid$ph[i], alk = grid$alk[i], tot_po4 = grid$po4[i])))
  })
  names(out) <- paste0("ph", grid$ph, "_alk", grid$alk, "_po4", grid$po4)
  expect_golden(out, "dissolve_pb_sweep")
})

test_that("regression: dissolve_pb chloride and sulfate controlled waters", {
  testthat::skip_on_cran()
  expect_golden(
    suppressWarnings(list(
      high_cl_ph6 = dissolve_pb(pb_water(ph = 6, alk = 5, cl = 5000, tds = 9000)),
      high_cl_ph7 = dissolve_pb(pb_water(ph = 7, alk = 5, cl = 5000, tds = 9000)),
      high_cl_po4_ph7 = dissolve_pb(pb_water(ph = 7, alk = 20, tot_po4 = 2, cl = 3000, tds = 6000)),
      high_so4_ph6 = dissolve_pb(pb_water(ph = 6, alk = 5, so4 = 5000, tds = 9000)),
      high_so4_ph5 = dissolve_pb(pb_water(ph = 5, alk = 2, so4 = 3000, tds = 6000)),
      zero_cl_so4 = dissolve_pb(suppressWarnings(define_water(ph = 7.5, temp = 20, alk = 50, tds = 150))),
      cold_po4 = dissolve_pb(pb_water(ph = 7.2, alk = 40, tot_po4 = 1, temp = 5)),
      warm_po4 = dissolve_pb(pb_water(ph = 7.8, alk = 80, tot_po4 = 3, temp = 30)),
      acid_high_po4_nocl_ph4.5 = dissolve_pb(pb_water(ph = 4.5, alk = 1, tot_po4 = 20, cl = NA_real_, tds = 200)),
      acid_high_po4_nocl_ph5.5 = dissolve_pb(pb_water(ph = 5.5, alk = 2, tot_po4 = 10, cl = NA_real_, tds = 200)),
      # alk below hydroxide alkalinity: carbonate is floored at 0 (#537).
      alkaline_lowcarb_ph11 = dissolve_pb(pb_water(ph = 11, alk = 1, tds = 200))
    )),
    "dissolve_pb_cl_so4"
  )
})

test_that("regression: dissolve_pb equilibrium constant options", {
  testthat::skip_on_cran()
  waters <- list(
    po4_ph7 = pb_water(ph = 7, alk = 20, tot_po4 = 3),
    po4_ph8 = pb_water(ph = 8, alk = 30, tot_po4 = 1),
    po4_highcl_ph7 = pb_water(ph = 7, alk = 20, tot_po4 = 3, cl = 2000, tds = 4000),
    highcl_ph6 = pb_water(ph = 6, alk = 5, cl = 5000, tds = 9000),
    highcl_ph8 = pb_water(ph = 8, alk = 5, cl = 5000, tds = 9000),
    # No chloride: pyromorphite/laurionite drop out so hydroxypyromorphite can control
    po4_nocl_ph7 = pb_water(ph = 7, alk = 20, tot_po4 = 3, cl = NA_real_, tds = 200),
    po4_nocl_ph8.5 = pb_water(ph = 8.5, alk = 20, tot_po4 = 1, cl = NA_real_, tds = 200)
  )
  opts <- expand.grid(
    hp = c("Schock", "Zhu"), pyro = c("Topolska", "Xie"), laur = c("Nasanen", "Lothenbach"),
    stringsAsFactors = FALSE
  )
  cases <- list()
  for (w in names(waters)) {
    for (i in seq_len(nrow(opts))) {
      nm <- paste0(w, "_", opts$hp[i], "_", opts$pyro[i], "_", opts$laur[i])
      cases[[nm]] <- suppressWarnings(dissolve_pb(
        waters[[w]], hydroxypyromorphite = opts$hp[i], pyromorphite = opts$pyro[i], laurionite = opts$laur[i]
      ))
    }
  }
  expect_golden(cases, "dissolve_pb_constants")
})
