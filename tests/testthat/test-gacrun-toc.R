# GACRUN-TOC ----

test_that("No water defined, no default listed", {
  water <- water_df[1, ]

  expect_error(gac_toc(media_size = "8x30", ebct = 10)) # argument water is missing, with no default
  expect_error(gac_toc(water)) # water is not a defined water object
})

test_that("gacrun_toc returns error if inputs are misspelled or missing.", {
  water1 <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))
  water2 <- suppressWarnings(define_water(temp = 25, tot_hard = 100, toc = 3.5)) # ph is not defined
  water3 <- suppressWarnings(define_water(ph = 7.5, temp = 25, tot_hard = 100)) # toc is not defined

  expect_error(gacrun_toc(water1, media_size = "11x40", model = "Zachman"))
  expect_error(gacrun_toc(water1, ebct = 15, model = "Zachman"))
  expect_error(gacrun_toc(water1, model = "Zachmann"))
  expect_error(gacrun_toc(water2, model = "Zachman"))
  expect_error(gacrun_toc(water3, model = "Zachman"))
})

test_that("gacrun_toc defaults to correct values.", {
  water <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))

  plot1 <- gacrun_toc(water)
  plot2 <- gacrun_toc(water, ebct = 10, model = "Zachman", media_size = "12x40")

  expect_true(identical(plot1, plot2))
})

test_that("gacrun_toc works.", {
  water <- suppressWarnings(define_water(ph = 7.5, toc = 3.5))

  plot1 <- gacrun_toc(water, model = "WTP")
  plot2 <- gacrun_toc(water, model = "Zachman")
  plot3 <- gacrun_toc(water, ebct = 20, model = "WTP")
  plo4 <- gacrun_toc(water, model = "WTP", media_size = "8x30")
  plot5 <- gacrun_toc(water, model = "Zachman", media_size = "8x30")

  expect_true(is.data.frame(plot1))
  expect_false(identical(plot1, plot2))
  expect_false(identical(plot1, plot3))
  expect_true(identical(plot1, plo4)) # media size isn't used in WTP model
  expect_false(identical(plot2, plot5)) # media size is used in Zachman model
})
