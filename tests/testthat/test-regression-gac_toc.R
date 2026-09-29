# Regression pins: gac_toc ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.

gac_water <- function(ph = 7.5, toc = 3, doc = toc * 0.95) {
  suppressWarnings(define_water(
    ph = ph, temp = 20, alk = 60, tot_hard = 80, na = 15, cl = 20, so4 = 20, tds = 200,
    toc = toc, doc = doc, uv254 = 0.06
  ))
}

test_that("regression: gac_toc Zachman media x ebct x bed_vol (coag pretreat)", {
  w <- gac_water()
  grid <- expand.grid(media = c("12x40", "8x30"), ebct = c(10, 20), bed_vol = c(5000, 15000, 30000),
                      stringsAsFactors = FALSE)
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(gac_toc(w, ebct = grid$ebct[i], model = "Zachman", media_size = grid$media[i],
                             bed_vol = grid$bed_vol[i]))
  })
  names(out) <- paste0("zachman_", grid$media, "_ebct", grid$ebct, "_bv", grid$bed_vol)
  expect_golden(out, "gac_toc_zachman")
})

test_that("regression: gac_toc WTP and pretreat options", {
  w <- gac_water()
  low <- gac_water(ph = 8.5, toc = 1.2)
  expect_golden(
    suppressWarnings(list(
      wtp_ebct10_bv3000 = gac_toc(w, ebct = 10, model = "WTP", bed_vol = 3000),
      wtp_ebct10_bv12000 = gac_toc(w, ebct = 10, model = "WTP", bed_vol = 12000),
      wtp_ebct20_bv12000 = gac_toc(w, ebct = 20, model = "WTP", bed_vol = 12000),
      wtp_ebct20_bv50000 = gac_toc(w, ebct = 20, model = "WTP", bed_vol = 50000),
      wtp_lowtoc_ebct10_bv35000 = gac_toc(low, ebct = 10, model = "WTP", bed_vol = 35000),
      wtp_ebct10_bv12000_other = gac_toc(w, ebct = 10, model = "WTP", bed_vol = 12000, pretreat = "ozone_bio"),
      zachman_12x40_ebct10_bv15000_other = gac_toc(w, bed_vol = 15000, pretreat = "other"),
      zachman_8x30_ebct20_bv8000_other = gac_toc(gac_water(ph = 6.5, toc = 5), ebct = 20, media_size = "8x30",
                                                 bed_vol = 8000, pretreat = "other"),
      zachman_12x40_ebct20_bv8000_coag_ph6.5 = gac_toc(gac_water(ph = 6.5, toc = 5), ebct = 20, bed_vol = 8000)
    )),
    "gac_toc_wtp_pretreat"
  )
})
