# GAC_TOC ----

test_that("No water defined, no default listed", {
  expect_error(gac_toc(media_size = "8x30", ebct = 10)) # argument water is missing, with no default
})

test_that("pac_toc returns error if inputs are misspelled or missing.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  water2 <- suppressWarnings(define_water(temp = 25, tot_hard = 100, toc = 3.5)) # ph is not defined
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 25, tot_hard = 100))# toc is not defined
  
  expect_error(gac_toc(water1, media_size = "11x40"))
  expect_error(gac_toc(water1, ebct = 15))
  expect_error(gac_toc(water1, option = c("plot", "test")))
  expect_error(gac_toc(water2))
  expect_error(gac_toc(water3))
})