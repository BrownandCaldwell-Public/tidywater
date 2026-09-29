# Regression pins: biofilter_toc ----
# Golden files pin every slot of the output water (including bdoc). See tests/testthat/README.md.

biof_water <- function(temp, doc = 3.2, toc = 3.5) {
  suppressWarnings(define_water(
    ph = 7.5, temp = temp, alk = 80, tot_hard = 100, na = 20, cl = 25, so4 = 30, tds = 220,
    toc = toc, doc = doc, uv254 = 0.08
  ))
}

biof_grid <- function(ozonated) {
  # Temperature band edges are 10 and 20 (<= 10, <= 20, > 20)
  grid <- expand.grid(temp = c(5, 10, 10.01, 15, 20, 20.01, 25), ebct = c(2, 10, 30))
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(biofilter_toc(biof_water(grid$temp[i]), ebct = grid$ebct[i], ozonated = ozonated))
  })
  names(out) <- paste0(if (ozonated) "oz" else "nonoz", "_t", grid$temp, "_ebct", grid$ebct)
  out
}

test_that("regression: biofilter_toc ozonated temperature x ebct", {
  expect_golden(biof_grid(TRUE), "biofilter_toc_ozonated")
})

test_that("regression: biofilter_toc non-ozonated temperature x ebct", {
  expect_golden(biof_grid(FALSE), "biofilter_toc_nonozonated")
})

test_that("regression: biofilter_toc edge inputs", {
  expect_golden(
    suppressWarnings(list(
      ebct0 = biofilter_toc(biof_water(15), ebct = 0),
      ebct_long = biofilter_toc(biof_water(25), ebct = 120, ozonated = FALSE),
      high_doc = biofilter_toc(biof_water(18, doc = 9, toc = 10), ebct = 15),
      doc_only = biofilter_toc(suppressWarnings(define_water(ph = 7, temp = 12, alk = 50, doc = 2.5)), ebct = 8)
    )),
    "biofilter_toc_edges"
  )
})
