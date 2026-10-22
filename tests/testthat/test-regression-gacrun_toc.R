# Regression pins: gacrun_toc ----
# Golden files pin the full breakthrough curves (bv, x_norm). See tests/testthat/README.md.

gac_water <- function(ph = 7.5, toc = 3, doc = toc * 0.95) {
  suppressWarnings(define_water(
    ph = ph, temp = 20, alk = 60, tot_hard = 80, na = 15, cl = 20, so4 = 20, tds = 200,
    toc = toc, doc = doc, uv254 = 0.06
  ))
}

test_that("regression: gacrun_toc Zachman media x ebct", {
  testthat::skip_on_cran()
  waters <- list(ph7.5_toc3 = gac_water(), ph6.5_toc5 = gac_water(ph = 6.5, toc = 5))
  cases <- list()
  for (w in names(waters)) {
    for (media in c("12x40", "8x30")) {
      for (ebct in c(10, 20)) {
        cases[[paste0("zachman_", media, "_ebct", ebct, "_", w)]] <-
          suppressWarnings(gacrun_toc(waters[[w]], ebct = ebct, model = "Zachman", media_size = media))
      }
    }
  }
  expect_golden(cases, "gacrun_toc_zachman")
})

test_that("regression: gacrun_toc WTP ebct 10 and 20", {
  testthat::skip_on_cran()
  w <- gac_water(ph = 7.5, toc = 3)
  expect_golden(
    suppressWarnings(list(
      wtp_ebct10_toc3 = gacrun_toc(w, ebct = 10, model = "WTP"),
      wtp_ebct20_toc3 = gacrun_toc(w, ebct = 20, model = "WTP")
    )),
    "gacrun_toc_wtp"
  )
})

test_that("regression: gacrun_toc WTP toc <= 1.5 (bv to 40000)", {
  testthat::skip_on_cran()
  w <- gac_water(ph = 8.5, toc = 1.2)
  expect_golden(
    suppressWarnings(list(
      wtp_ebct10_toc1.2 = gacrun_toc(w, ebct = 10, model = "WTP"),
      wtp_ebct20_toc1.2 = gacrun_toc(w, ebct = 20, model = "WTP")
    )),
    "gacrun_toc_wtp_lowtoc"
  )
})

test_that("regression: gacrun_toc WTP custom bvs and other ebct", {
  testthat::skip_on_cran()
  expect_golden(
    suppressWarnings(list(
      wtp_custom_bvs = gacrun_toc(gac_water(ph = 7, toc = 4), ebct = 10, model = "WTP", bvs = c(1000, 30000, 1000)),
      wtp_custom_bvs_lowtoc = gacrun_toc(gac_water(ph = 7, toc = 1.2), ebct = 20, model = "WTP", bvs = c(500, 50000, 2500)),
      wtp_ebct15_ph6.2 = gacrun_toc(gac_water(ph = 6.2, toc = 6), ebct = 15, model = "WTP", bvs = c(2000, 20000, 1000)),
      wtp_toc1.5_edge = gacrun_toc(gac_water(ph = 7.5, toc = 1.5), ebct = 10, model = "WTP", bvs = c(2000, 40000, 2000))
    )),
    "gacrun_toc_wtp_custom"
  )
})

test_that("regression: gacrun_toc_df", {
  testthat::skip_on_cran()
  df <- suppressWarnings(define_water_df(water_df[1:3, ]))
  drop_water <- function(out) out[, !vapply(out, is.list, logical(1)), drop = FALSE]
  zach <- suppressWarnings(gacrun_toc_df(df))
  wtp <- suppressWarnings(gacrun_toc_df(df, model = "WTP", ebct = 20, bvs = c(2000, 20000, 2000)))
  keep <- function(out) drop_water(out)[, grepl("_bv$|_x_norm$|^ebct$|^model$|^media_size$", names(drop_water(out))), drop = FALSE]
  expect_golden(
    list(zachman_default = keep(zach), wtp_ebct20_custom_bvs = keep(wtp)),
    "gacrun_toc_df"
  )
})
