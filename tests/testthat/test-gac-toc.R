# GAC_TOC ----

test_that("No water defined, no default listed", {
  expect_error(gac_toc(media_size = "8x30", ebct = 10)) # argument water is missing, with no default
})