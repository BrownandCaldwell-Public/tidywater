# Regression pins: pac_toc ----
# Golden files pin every slot of the output water. See tests/testthat/README.md.

pac_water <- function(doc = 3.2, toc = doc + 0.3) {
  suppressWarnings(define_water(
    ph = 7.5, temp = 20, alk = 60, tot_hard = 80, na = 15, cl = 20, so4 = 20, tds = 200,
    toc = toc, doc = doc, uv254 = 0.08
  ))
}

pac_grid <- function(type) {
  # dose < 5 and time < 10 are warn regions; dose/time 0 returns input water
  grid <- expand.grid(dose = c(0, 2, 5, 15, 30), time = c(5, 10, 30, 60))
  w <- pac_water()
  out <- lapply(seq_len(nrow(grid)), function(i) {
    suppressWarnings(pac_toc(w, dose = grid$dose[i], time = grid$time[i], type = type))
  })
  names(out) <- paste0(type, "_dose", grid$dose, "_time", grid$time)
  out
}

test_that("regression: pac_toc bituminous dose x time", {
  expect_golden(pac_grid("bituminous"), "pac_toc_bituminous")
})

test_that("regression: pac_toc wood dose x time", {
  expect_golden(pac_grid("wood"), "pac_toc_wood")
})

test_that("regression: pac_toc lignite dose x time", {
  expect_golden(pac_grid("lignite"), "pac_toc_lignite")
})

# KNOWN-BUG #540: the DOC < 1.3 clamp can raise DOC, and UV254 can go negative or change at dose 0.
test_that("regression: pac_toc DOC clamp and water edge cases", {
  expect_golden(
    suppressWarnings(list(
      doc1.0_clamp_bituminous = pac_toc(pac_water(doc = 1.0), dose = 10, time = 30),
      doc1.0_clamp_wood = pac_toc(pac_water(doc = 1.0), dose = 10, time = 30, type = "wood"),
      doc1.0_clamp_lignite = pac_toc(pac_water(doc = 1.0), dose = 10, time = 30, type = "lignite"),
      doc1.29_clamp = pac_toc(pac_water(doc = 1.29), dose = 20, time = 20),
      doc1.3_edge = pac_toc(pac_water(doc = 1.3), dose = 20, time = 20),
      doc5.4_edge = pac_toc(pac_water(doc = 5.4), dose = 20, time = 20),
      doc7_above = pac_toc(pac_water(doc = 7), dose = 20, time = 20, type = "WOOD"),
      toc_lt_doc = pac_toc(pac_water(doc = 3, toc = 2.5), dose = 10, time = 30),
      toc_na = pac_toc(suppressWarnings(define_water(ph = 7, temp = 20, alk = 50, doc = 3, uv254 = 0.07)),
                       dose = 10, time = 30)
    )),
    "pac_toc_edges"
  )
})
