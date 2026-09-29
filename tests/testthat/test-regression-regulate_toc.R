# Regression pins: regulate_toc ----
# Golden files pin every column of the output data frame (including the comment text).
# See tests/testthat/README.md.

toc_waters <- function(toc_raw, toc_fin, alk = 80, doc_raw = toc_raw * 0.9, uv_raw = 0.1,
                       doc_fin = toc_fin * 0.9, uv_fin = 0.05, tthm = NA_real_, haa5 = NA_real_) {
  raw <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = alk, tds = 200, toc = toc_raw, doc = doc_raw, uv254 = uv_raw))
  fin <- suppressWarnings(define_water(ph = 7.5, temp = 20, alk = alk, tds = 200, toc = toc_fin, doc = doc_fin, uv254 = uv_fin))
  fin@tthm <- tthm
  fin@haa5 <- haa5
  list(raw = raw, fin = fin)
}


test_that("regression: regulate_toc numeric alternative compliance and guard rails", {
  expect_golden(
    suppressWarnings(list(
      alt_i_raw_toc_1.5 = regulate_toc(alk_raw = 80, toc_raw = 1.5, toc_finished = 1),
      alt_i_raw_toc_exactly_2 = regulate_toc(alk_raw = 80, toc_raw = 2, toc_finished = 1.9),
      alt_i_raw_toc_2_no_removal = regulate_toc(alk_raw = 80, toc_raw = 2, toc_finished = 2.5),
      alt_ii_fin_toc_exactly_2 = regulate_toc(alk_raw = 30, toc_raw = 6, toc_finished = 2),
      alt_ii_fin_toc_1.2 = regulate_toc(alk_raw = 30, toc_raw = 10, toc_finished = 1.2),
      # Numeric inputs carry no DBP/SUVA info, so (iii)-(vi) cannot trigger here.
      no_alt_standard_met = regulate_toc(alk_raw = 80, toc_raw = 3.5, toc_finished = 2.5),
      removal_zero = regulate_toc(alk_raw = 80, toc_raw = 5, toc_finished = 5),
      removal_negative = regulate_toc(alk_raw = 80, toc_raw = 5, toc_finished = 5.5),
      removal_negative_skip = regulate_toc(alk_raw = 80, toc_raw = 5, toc_finished = 5.5, skip_alternate = TRUE),
      skip_raw_toc_1.5 = regulate_toc(alk_raw = 80, toc_raw = 1.5, toc_finished = 1, skip_alternate = TRUE),
      skip_raw_toc_exactly_2 = regulate_toc(alk_raw = 80, toc_raw = 2, toc_finished = 1, skip_alternate = TRUE),
      skip_fin_toc_1.2 = regulate_toc(alk_raw = 30, toc_raw = 10, toc_finished = 1.2, skip_alternate = TRUE),
      # Character fallback: alk 0 matches no table row (alk_min is exclusive).
      char_fallback_alk0 = regulate_toc(alk_raw = 0, toc_raw = 5, toc_finished = 4),
      char_fallback_alk0_skip = regulate_toc(alk_raw = 0, toc_raw = 3, toc_finished = 2.5, skip_alternate = TRUE),
      char_fallback_alk_negative = regulate_toc(alk_raw = -5, toc_raw = 9, toc_finished = 6)
    )),
    "regulate_toc_numeric_alt"
  )
})

test_that("regression: regulate_toc every TOC x alkalinity table cell and boundary", {
  # skip_alternate = TRUE so each cell reaches the standard compliance table.
  tocs <- c(2.5, 4, 6, 8, 12)
  alks <- c(0.5, 30, 60, 90, 120, 200)
  removals <- c(10, 20, 30, 40, 55)
  grid <- expand.grid(toc = tocs, alk = alks, removal = removals)
  out <- lapply(seq_len(nrow(grid)), function(i) {
    fin <- grid$toc[i] * (1 - grid$removal[i] / 100)
    suppressWarnings(regulate_toc(alk_raw = grid$alk[i], toc_raw = grid$toc[i], toc_finished = fin, skip_alternate = TRUE))
  })
  names(out) <- with(grid, paste0("toc", toc, "_alk", alk, "_rem", removal))
  expect_golden(out, "regulate_toc_table")
})

test_that("regression: regulate_toc removal exactly at requirement", {
  req <- tidywater::toc_compliance_table
  cells <- data.frame(toc = c(3, 3, 3, 6, 6, 6, 10, 10, 10), alk = c(30, 90, 150, 30, 90, 150, 30, 90, 150))
  cells$req <- vapply(seq_len(nrow(cells)), function(i) {
    with(req, required_compliance[cells$toc[i] > toc_min & cells$toc[i] <= toc_max &
      cells$alk[i] > alk_min & cells$alk[i] <= alk_max])
  }, numeric(1))
  out <- list()
  for (i in seq_len(nrow(cells))) {
    for (delta in c(-0.1, 0, 0.1)) {
      # Choose toc_finished so the removal hits the requirement (floating-point exact where possible).
      fin <- cells$toc[i] - cells$toc[i] * (cells$req[i] + delta) / 100
      nm <- paste0("toc", cells$toc[i], "_alk", cells$alk[i], "_req", cells$req[i], "_delta", delta)
      out[[nm]] <- suppressWarnings(regulate_toc(
        alk_raw = cells$alk[i], toc_raw = cells$toc[i], toc_finished = fin, skip_alternate = TRUE
      ))
    }
  }
  expect_golden(out, "regulate_toc_at_requirement")
})

test_that("regression: regulate_toc water-object inputs (alternative paths iii-vi)", {
  cases <- list(
    # (iii) toc_raw < 4, alk > 60, TTHM < 40 or HAA5 < 30 (only one needs to be low)
    alt_iii_low_tthm = toc_waters(3.5, 3, alk = 80, tthm = 30, haa5 = 50),
    alt_iii_low_haa5 = toc_waters(3.5, 3, alk = 80, tthm = 70, haa5 = 20),
    # alk exactly 60 fails (iii)'s strict > 60, so falls through to (iv)
    alt_iv_alk_exactly_60 = toc_waters(3.5, 3, alk = 60, tthm = 30, haa5 = 20),
    # (iv) both DBPs low
    alt_iv_both_low = toc_waters(6, 5, alk = 40, tthm = 30, haa5 = 20),
    not_iv_tthm_exactly_40 = toc_waters(6, 5, alk = 40, tthm = 40, haa5 = 20, uv_raw = 0.3, uv_fin = 0.2),
    # (v) raw SUVA < 2
    alt_v_raw_suva = toc_waters(6, 5, alk = 40, uv_raw = 0.08, tthm = 70, haa5 = 50),
    # (vi) treated SUVA < 2
    alt_vi_fin_suva = toc_waters(6, 5, alk = 40, uv_raw = 0.3, uv_fin = 0.06, tthm = 70, haa5 = 50),
    # No DBPs set: warns and skips (iii)/(iv); SUVA also high -> standard compliance
    no_dbps_standard_met = toc_waters(6, 3.2, alk = 40, uv_raw = 0.3, uv_fin = 0.2),
    no_dbps_not_met = toc_waters(6, 4.5, alk = 40, uv_raw = 0.3, uv_fin = 0.2),
    # (i) and (ii) via water objects
    alt_i_water = toc_waters(1.8, 1.5, alk = 40),
    alt_ii_water = toc_waters(5, 1.9, alk = 40),
    # skip_alternate with water objects that would otherwise pass (iv)
    skip_alt_water = toc_waters(6, 5, alk = 40, tthm = 30, haa5 = 20),
    # water objects override explicit numeric args (warns)
    override_numeric = toc_waters(6, 5, alk = 40, tthm = 30, haa5 = 20),
    # raw doc missing -> SUVA can't be computed
    missing_raw_doc = toc_waters(6, 5, alk = 40, doc_raw = NA_real_, uv_raw = NA_real_, tthm = 70, haa5 = 50)
  )
  out <- lapply(names(cases), function(nm) {
    x <- cases[[nm]]
    suppressWarnings(
      if (nm == "skip_alt_water") {
        regulate_toc(raw_water = x$raw, treated_water = x$fin, skip_alternate = TRUE)
      } else if (nm == "override_numeric") {
        regulate_toc(alk_raw = 200, toc_raw = 1, toc_finished = 0.5, raw_water = x$raw, treated_water = x$fin)
      } else {
        regulate_toc(raw_water = x$raw, treated_water = x$fin)
      }
    )
  })
  names(out) <- names(cases)
  expect_golden(out, "regulate_toc_water_objects")
})

test_that("regression: regulate_toc_df numeric and water paths", {
  df <- data.frame(
    alk_raw = c(80, 30, 0, 150, 90, 50),
    toc_raw = c(1.5, 6, 5, 9, 3.5, 5),
    toc_finished = c(1, 4, 4, 6, 2.5, 5.5)
  )
  num <- suppressWarnings(regulate_toc_df(df))
  # KNOWN-BUG #529
  num_skip <- suppressWarnings(regulate_toc_df(df, skip_alternate = TRUE))
  const_args <- suppressWarnings(regulate_toc_df(data.frame(id = 1:2), alk_raw = 100, toc_raw = 5, toc_finished = 3.4))

  a <- toc_waters(6, 5, alk = 40, tthm = 30, haa5 = 20)
  b <- toc_waters(6, 3.2, alk = 40, uv_raw = 0.3, uv_fin = 0.2)
  c <- toc_waters(1.8, 1.5, alk = 40)
  wdf <- data.frame(id = 1:3)
  wdf$raw <- list(a$raw, b$raw, c$raw)
  wdf$fin <- list(a$fin, b$fin, c$fin)
  wat <- suppressWarnings(regulate_toc_df(wdf, input_raw_water = "raw", input_treated_water = "fin"))
  wat_skip <- suppressWarnings(regulate_toc_df(wdf, input_raw_water = "raw", input_treated_water = "fin", skip_alternate = TRUE))
  # Pin only the result columns (water columns are pinned elsewhere).
  keep <- function(d) d[, setdiff(names(d), c("raw", "fin")), drop = FALSE]
  expect_golden(
    list(
      numeric = num,
      numeric_skip_knownbug529 = num_skip,
      constant_args = const_args,
      water = keep(wat),
      water_skip = keep(wat_skip)
    ),
    "regulate_toc_df"
  )
})
