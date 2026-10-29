# Checks for the golden-file helper itself (helper-regression.R) ----

write_golden <- function(tbl) {
  path <- tempfile(fileext = ".txt")
  golden_write(tbl, path)
  path
}

test_that("golden_table flattens waters, data frames, lists, and scalars", {
  testthat::skip_on_cran()
  water <- suppressWarnings(define_water(ph = 7, temp = 20, alk = 50))
  tbl <- golden_table(list(
    w = water,
    df = data.frame(a = 1:2, b = c("x", NA)),
    l = list(p = 1.5, q = c(r = 2, s = NA)),
    s = 3
  ))

  expect_true(all(c("case", "output", "value") %in% names(tbl)))
  expect_equal(tbl$value[tbl$case == "w" & tbl$output == "ph"], "7")
  expect_true("ks.k1co3" %in% tbl$output)
  expect_equal(tbl$value[tbl$case == "w" & tbl$output == "na"], "NA")
  expect_equal(tbl$output[tbl$case == "df"], c("[1].a", "[1].b", "[2].a", "[2].b"))
  expect_equal(tbl$value[tbl$case == "df"], c("1", "x", "2", "NA"))
  expect_equal(tbl$output[tbl$case == "l"], c("p", "q.r", "q.s"))
  expect_equal(tbl$value[tbl$case == "s"], "3")
})

test_that("golden_format keeps 15 significant digits", {
  testthat::skip_on_cran()
  expect_equal(golden_format(1 / 3), "0.333333333333333")
  expect_equal(golden_format(1.23456789012345e-12), "1.23456789012345e-12")
  expect_equal(golden_format(c(NA, Inf, -Inf)), c("NA", "Inf", "-Inf"))
  expect_equal(golden_format("two\nlines"), "two\\nlines")
})

test_that("golden files round-trip text containing '=' and ' = '", {
  testthat::skip_on_cran()
  tbl <- data.frame(case = "a", output = c("x", "y"), value = c("TOC <= 2", "a = b"))
  path <- write_golden(tbl)
  expect_equal(golden_read(path)$key, c("a :: x", "a :: y"))
  expect_equal(golden_read(path)$value, c("TOC <= 2", "a = b"))
  expect_true(golden_compare(1e-8, 0)(path, path))
})

test_that("golden_compare applies tolerance and catches changes", {
  testthat::skip_on_cran()
  old <- data.frame(case = "a", output = c("x", "y", "z"), value = c("1", "NA", "text"))
  same <- write_golden(old)
  near <- write_golden(transform(old, value = c("1.0000000001", "NA", "text")))
  far <- write_golden(transform(old, value = c("1.001", "NA", "text")))
  na_changed <- write_golden(transform(old, value = c("1", "0", "text")))
  text_changed <- write_golden(transform(old, value = c("1", "NA", "other")))
  rows_changed <- write_golden(old[1:2, ])
  old <- write_golden(old)

  cmp <- golden_compare(rel_tol = 1e-8, abs_tol = 0)
  expect_true(cmp(old, same))
  expect_true(cmp(old, near))
  expect_false(suppressMessages(cmp(old, far)))
  expect_false(suppressMessages(cmp(old, na_changed)))
  expect_false(suppressMessages(cmp(old, text_changed)))
  expect_false(suppressMessages(cmp(old, rows_changed)))
  expect_true(golden_compare(rel_tol = 0, abs_tol = 0.01)(old, far))
})
