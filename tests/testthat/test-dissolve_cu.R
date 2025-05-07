#dissolve_cu

test_that("DIC or alk is required for dissolve_cu", {
  water1 <- suppressWarnings(define_water(ph = 8, toc = 2.5))
  water2 <- suppressWarnings(define_water(ph = 8, alk = 45))
  water3 <- suppressWarnings(define_water(ph = 7.5, dic = 20))
  
  expect_error(dissolve_cu(water1))
  expect_warning(dissolve_cu(water2))
  expect_no_error(dissolve_cu(water3))
})
