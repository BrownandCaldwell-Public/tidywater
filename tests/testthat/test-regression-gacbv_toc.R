# Regression pins: gacbv_toc ----
# Golden files pin the returned bed volumes. See tests/testthat/README.md.

gac_water <- function(ph = 7.5, toc = 3, doc = toc * 0.95) {
  suppressWarnings(define_water(
    ph = ph, temp = 20, alk = 60, tot_hard = 80, na = 15, cl = 20, so4 = 20, tds = 200,
    toc = toc, doc = doc, uv254 = 0.06
  ))
}

test_that("regression: gacbv_toc Zachman and WTP with vector target_doc", {
  w <- gac_water(toc = 3, doc = 2.85)
  w_low <- gac_water(ph = 7, toc = 2, doc = 1.2)
  targets <- c(0.9, 1.2, 1.5, 1.9)
  targets_low <- c(0.3, 0.5, 0.7)
  expect_golden(
    suppressWarnings(list(
      zachman_12x40_ebct10 = gacbv_toc(w, target_doc = targets),
      zachman_12x40_ebct20 = gacbv_toc(w, ebct = 20, target_doc = targets),
      zachman_8x30_ebct10 = gacbv_toc(w, media_size = "8x30", target_doc = targets),
      zachman_8x30_ebct20 = gacbv_toc(w, ebct = 20, media_size = "8x30", target_doc = targets),
      zachman_12x40_lowdoc = gacbv_toc(w_low, target_doc = targets_low),
      wtp_ebct10 = gacbv_toc(w, model = "WTP", target_doc = c(0.6, 1, 1.5, 1.9)),
      wtp_ebct20 = gacbv_toc(w, ebct = 20, model = "WTP", target_doc = c(0.6, 1, 1.5, 1.9)),
      wtp_lowdoc = gacbv_toc(w_low, model = "WTP", target_doc = targets_low),
      wtp_scalar = gacbv_toc(w, model = "WTP", target_doc = 1.2)
    )),
    "gacbv_toc"
  )
})
