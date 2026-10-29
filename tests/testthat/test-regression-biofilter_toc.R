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
  testthat::skip_on_cran()
  expect_golden(biof_grid(TRUE), "biofilter_toc_ozonated")
})

test_that("regression: biofilter_toc non-ozonated temperature x ebct", {
  testthat::skip_on_cran()
  expect_golden(biof_grid(FALSE), "biofilter_toc_nonozonated")
})

test_that("regression: biofilter_toc edge inputs", {
  testthat::skip_on_cran()
  expect_golden(
    suppressWarnings(list(
      ebct_short = biofilter_toc(biof_water(15), ebct = 0.5),
      ebct_long = biofilter_toc(biof_water(25), ebct = 120, ozonated = FALSE),
      high_doc = biofilter_toc(biof_water(18, doc = 9, toc = 10), ebct = 15),
      doc_only = biofilter_toc(suppressWarnings(define_water(ph = 7, temp = 12, alk = 50, doc = 2.5)), ebct = 8)
    )),
    "biofilter_toc_edges"
  )
})

# Reuse (Peterson & Summers 2021) is TOC-based, so the reuse water has TOC > DOC.
reuse_water <- function(temp = 20) biof_water(temp, doc = 6, toc = 6.5)

test_that("regression: biofilter_toc reuse media x ozone dose x ebct", {
  testthat::skip_on_cran()
  # ozone/TOC = 0, 0.46, 1.23 (validated 0 - 1.2); ebct validated 5 - 30 min
  grid <- expand.grid(media = c("gac", "anthracite", "sand"), ozone = c(0, 3, 8), ebct = c(2, 15, 30), stringsAsFactors = FALSE)
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(biofilter_toc(
      reuse_water(), ebct = grid$ebct[i], reuse = TRUE, media_type = grid$media[i], ozone_dose = grid$ozone[i]
    ))
  })
  names(out) <- paste0(grid$media, "_o3", grid$ozone, "_ebct", grid$ebct)
  expect_golden(out, "biofilter_toc_reuse")
})

test_that("regression: biofilter_toc ATP model (Terry et al. 2024)", {
  # Biofilter media ATP is typically 100 - 1000 ng/cm3.
  testthat::skip_on_cran()
  modes <- list(
    drinking = list(reuse = FALSE, ozonated = TRUE),
    reuse_oz = list(reuse = TRUE, ozonated = TRUE),
    reuse_nonoz = list(reuse = TRUE, ozonated = FALSE)
  )
  grid <- expand.grid(mode = names(modes), atp = c(100, 1000), temp = c(5, 22, 30), ebct = 15, stringsAsFactors = FALSE)
  grid <- rbind(grid, expand.grid(mode = names(modes), atp = 1000, temp = 22, ebct = c(3, 30), stringsAsFactors = FALSE))
  out <- lapply(seq_len(nrow(grid)), function(i) {
    m <- modes[[grid$mode[i]]]
    suppressWarnings(biofilter_toc(
      reuse_water(grid$temp[i]), ebct = grid$ebct[i], reuse = m$reuse, ozonated = m$ozonated, atp = grid$atp[i]
    ))
  })
  names(out) <- paste0(grid$mode, "_atp", grid$atp, "_t", grid$temp, "_ebct", grid$ebct)
  expect_golden(out, "biofilter_toc_atp")
})

test_that("regression: biofilter_toc reuse and ATP options", {
  testthat::skip_on_cran()
  expect_golden(
    suppressWarnings(list(
      reuse_nonoz = biofilter_toc(reuse_water(), ebct = 10, reuse = TRUE, ozonated = FALSE),
      reuse_media_upper = biofilter_toc(reuse_water(), ebct = 10, reuse = TRUE, media_type = "GAC", ozone_dose = 2),
      reuse_cold = biofilter_toc(reuse_water(5), ebct = 10, reuse = TRUE, ozone_dose = 2),
      reuse_doc_only = biofilter_toc(suppressWarnings(define_water(ph = 7, temp = 12, alk = 50, doc = 2.5)), ebct = 8, reuse = TRUE),
      atp_doc_only = biofilter_toc(suppressWarnings(define_water(ph = 7, temp = 12, alk = 50, doc = 2.5)), ebct = 8, atp = 500)
    )),
    "biofilter_toc_reuse_options"
  )

  # The ATP model ignores ozone_dose and media_type.
  expect_identical(
    suppressWarnings(biofilter_toc(reuse_water(), ebct = 10, reuse = TRUE, atp = 500, ozone_dose = 4, media_type = "gac")),
    suppressWarnings(biofilter_toc(reuse_water(), ebct = 10, reuse = TRUE, atp = 500))
  )
})

test_that("regression: biofilter_toc argument errors", {
  testthat::skip_on_cran()
  expect_error(biofilter_toc(biof_water(15), ebct = 0), "ebct")
  expect_error(biofilter_toc(biof_water(15), ebct = -5), "ebct")
  expect_error(biofilter_toc(biof_water(15), ebct = 10, ozone_dose = -1), "ozone_dose")
  expect_error(biofilter_toc(biof_water(15), ebct = 10, ozonated = "yes"), "ozonated")
  expect_error(suppressWarnings(biofilter_toc(reuse_water(), ebct = 10, reuse = TRUE, media_type = "clay")), "media_type")
  expect_error(biofilter_toc(biof_water(15), ebct = 10, atp = -1), "Invalid combination")
})
