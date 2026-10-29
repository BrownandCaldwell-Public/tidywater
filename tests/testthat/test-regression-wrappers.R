# Regression checks: _df helpers and deprecated _chain/_once wrappers ----
# The base functions are pinned numerically in the other test-regression-*.R files. These tests
# check that every _df helper gives exactly the same result as mapping the base function over
# the rows, and that deprecated wrappers forward each argument to the parameter of the same name.

# Wrapper argument forwarding (static check) ----

find_df_call <- function(expr) {
  if (is.call(expr)) {
    head <- expr[[1]]
    if (is.symbol(head) && grepl("_df$", as.character(head))) {
      return(expr)
    }
    for (e in as.list(expr)[-1]) {
      found <- find_df_call(e)
      if (!is.null(found)) {
        return(found)
      }
    }
  }
  NULL
}

wrapper_forwarding <- function() {
  ns <- asNamespace("tidywater")
  wrappers <- sort(ls(ns, pattern = "_(chain|once)$"))
  out <- lapply(wrappers, function(w) {
    f <- get(w, ns)
    call <- find_df_call(body(f))
    target <- get(as.character(call[[1]]), ns)
    args <- as.list(match.call(target, call))[-1]
    shifted <- character()
    for (nm in names(args)) {
      v <- args[[nm]]
      if (is.symbol(v) && as.character(v) %in% names(formals(f)) && as.character(v) != nm) {
        shifted <- c(shifted, paste0(as.character(v), " -> ", nm))
      }
    }
    passed <- vapply(args, function(v) if (is.symbol(v)) as.character(v) else "", character(1))
    dropped <- setdiff(names(formals(f)), c(passed, "df"))
    list(wrapper = w, shifted = shifted, dropped = dropped)
  })
  names(out) <- wrappers
  out
}

test_that("regression: deprecated wrappers forward arguments to same-named parameters", {
  testthat::skip_on_cran()
  fwd <- wrapper_forwarding()
  shifted <- names(Filter(function(x) length(x$shifted) > 0, fwd))
  dropped <- names(Filter(function(x) length(x$dropped) > 0, fwd))

  expect_equal(shifted, character())
  expect_equal(dropped, character())
})

# _df helpers match the base function ----

regression_df <- function() {
  waters <- regression_waters()
  waters <- waters[c("typical", "low_alk", "hard_high_alk", "nutrients")]
  # Bromate and ozone models need bromide.
  waters <- lapply(waters, function(w) {
    if (is.na(w@br)) w <- suppressWarnings(modify_water(w, "br", 40, "ug/L"))
    w
  })
  data.frame(id = names(waters), defined = I(unname(waters)))
}

# Compares a list of waters (or data frames) by their full golden representation.
expect_same_outputs <- function(actual, expected) {
  names(actual) <- names(expected) <- paste0("row", seq_along(expected))
  expect_identical(golden_table(actual), golden_table(expected))
}

map_base <- function(df, f, ...) {
  lapply(df$defined, function(w) suppressWarnings(f(w, ...)))
}

test_that("regression: water-returning _df helpers match base functions", {
  testthat::skip_on_cran()
  df <- regression_df()
  run <- function(f_df, ...) suppressWarnings(f_df(df, output_water = "out", ...))$out

  expect_same_outputs(run(balance_ions_df, anion = "so4", cation = "ca"), map_base(df, balance_ions, anion = "so4", cation = "ca"))
  expect_same_outputs(run(biofilter_toc_df, ebct = 12, ozonated = FALSE), map_base(df, biofilter_toc, ebct = 12, ozonated = FALSE))
  expect_same_outputs(
    run(chemdose_chloramine_df, time = 10, cl2 = 4, nh3 = 1),
    map_base(df, chemdose_chloramine, time = 10, cl2 = 4, nh3 = 1)
  )
  expect_same_outputs(
    run(chemdose_chlordecay_df, cl2_dose = 3, time = 12, treatment = "coag", cl_type = "chlorine"),
    map_base(df, chemdose_chlordecay, cl2_dose = 3, time = 12, treatment = "coag", cl_type = "chlorine")
  )
  expect_same_outputs(
    run(chemdose_dbp_df, cl2 = 3, time = 24, treatment = "coag", cl_type = "chlorine", location = "ds"),
    map_base(df, chemdose_dbp, cl2 = 3, time = 24, treatment = "coag", cl_type = "chlorine", location = "ds")
  )
  expect_same_outputs(
    run(chemdose_ph_df, alum = 25, naoh = 5, softening_correction = FALSE),
    map_base(df, chemdose_ph, alum = 25, naoh = 5)
  )
  expect_same_outputs(
    run(chemdose_toc_df, ferricchloride = 30, coeff = "Ferric"),
    map_base(df, chemdose_toc, ferricchloride = 30, coeff = "Ferric")
  )
  # chemdose_inorgdemand needs fe, mn, and tot_nh3, which the shared waters lack.
  df_inorg <- data.frame(id = c("fe_mn", "nh3_cl"))
  df_inorg$defined <- I(list(
    suppressWarnings(define_water(ph = 7, temp = 20, alk = 70, tds = 200, fe = 1, mn = 200, tot_nh3 = 0)),
    suppressWarnings(define_water(ph = 7.5, temp = 15, alk = 50, tds = 150, fe = 0.5, mn = 50, tot_nh3 = 0.2, free_chlorine = 1.5))
  ))
  expect_same_outputs(
    suppressWarnings(chemdose_inorgdemand_df(df_inorg, output_water = "out", cl2_dose = 4))$out,
    map_base(df_inorg, chemdose_inorgdemand, cl2_dose = 4)
  )
  expect_same_outputs(
    suppressWarnings(chemdose_inorgdemand_df(df_inorg, output_water = "out", cl2_dose = 4, use_chlorine_slot = TRUE))$out,
    map_base(df_inorg, chemdose_inorgdemand, cl2_dose = 4, use_chlorine_slot = TRUE)
  )
  expect_same_outputs(run(decarbonate_ph_df, co2_removed = 0.8), map_base(df, decarbonate_ph, co2_removed = 0.8))
  expect_same_outputs(
    run(gac_toc_df, model = "Zachman", media_size = "12x40", ebct = 10, bed_vol = 10000, pretreat = "coag"),
    map_base(df, gac_toc, model = "Zachman", media_size = "12x40", ebct = 10, bed_vol = 10000, pretreat = "coag")
  )
  expect_same_outputs(
    run(modify_water_df, slot = "na", value = 40, units = "mg/L"),
    map_base(df, modify_water, slot = "na", value = 40, units = "mg/L")
  )
  df_cols <- df
  df_cols$slot <- "na"
  df_cols$value <- 40
  df_cols$units <- "mg/L"
  expect_same_outputs(
    suppressWarnings(modify_water_df(df_cols, output_water = "out"))$out,
    map_base(df, modify_water, slot = "na", value = 40, units = "mg/L")
  )
  expect_same_outputs(run(opensys_ph_df, partialpressure = 10^-3), map_base(df, opensys_ph, partialpressure = 10^-3))
  expect_same_outputs(
    run(ozonate_bromate_df, dose = 2, time = 10, model = "Ozekin"),
    map_base(df, ozonate_bromate, dose = 2, time = 10, model = "Ozekin")
  )
  expect_same_outputs(run(pac_toc_df, dose = 15, time = 30, type = "wood"), map_base(df, pac_toc, dose = 15, time = 30, type = "wood"))
})

test_that("regression: _df helpers read arguments from columns", {
  testthat::skip_on_cran()
  df <- regression_df()
  df$alum <- c(0, 10, 30, 60)
  df$naoh <- c(5, 0, 2, 0)
  from_cols <- suppressWarnings(chemdose_ph_df(df, output_water = "out"))$out
  base <- Map(function(w, alum, naoh) suppressWarnings(chemdose_ph(w, alum = alum, naoh = naoh)), df$defined, df$alum, df$naoh)
  expect_same_outputs(from_cols, base)

  df$dose <- c(1, 2, 3, 4)
  df$time <- c(5, 10, 15, 20)
  from_cols <- suppressWarnings(ozonate_bromate_df(df, output_water = "out", model = "Sohn"))$out
  base <- Map(function(w, dose, time) suppressWarnings(ozonate_bromate(w, dose = dose, time = time, model = "Sohn")), df$defined, df$dose, df$time)
  expect_same_outputs(from_cols, base)

  df$kdoc <- c(1.67e4, 2.5e4, 3.6e4, 2e4)
  expect_identical(
    suppressWarnings(scavenge_oh_df(df, dose = dose, kdoc_adjust = kdoc))$oh_scavenging,
    unlist(Map(function(w, dose, kdoc) suppressWarnings(scavenge_oh(w, dose = dose, kdoc_adjust = kdoc)), df$defined, df$dose, df$kdoc))
  )
})

test_that("regression: pluck_cols gives the same values as the water column", {
  testthat::skip_on_cran()
  df <- regression_df()
  out <- suppressWarnings(chemdose_ph_df(df, output_water = "out", alum = 25, pluck_cols = TRUE))
  waters <- out$out
  expect_equal(names(out), c("id", "defined", "out", "out_ph", "out_alk"))
  for (slot in c("ph", "alk")) {
    expect_identical(out[[paste0("out_", slot)]], vapply(waters, methods::slot, numeric(1), slot))
  }
})

test_that("regression: blend_waters_df matches blend_waters", {
  testthat::skip_on_cran()
  df <- regression_df()
  df$second <- rev(df$defined)
  out <- suppressWarnings(blend_waters_df(df, c("defined", "second"), c(0.3, 0.7), output_water = "out"))$out
  base <- Map(function(a, b) suppressWarnings(blend_waters(list(a, b), c(0.3, 0.7))), df$defined, df$second)
  expect_same_outputs(out, base)
})

test_that("regression: numeric-output _df helpers match base functions", {
  testthat::skip_on_cran()
  df <- regression_df()
  # _df helpers add "<input_water>_<name>" columns; compare them by name to the base output.
  expect_prefixed_match <- function(out, base) {
    base <- do.call(rbind, base)
    cols <- grep("^defined_", names(out), value = TRUE)
    expect_setequal(sub("^defined_", "", cols), names(base))
    for (col in cols) {
      expect_identical(out[[col]], base[[sub("^defined_", "", col)]], label = col)
    }
  }

  expect_prefixed_match(
    suppressWarnings(calculate_corrosion_df(df, form = "aragonite")),
    map_base(df, calculate_corrosion, form = "aragonite")
  )
  expect_prefixed_match(suppressWarnings(dissolve_cu_df(df)), map_base(df, dissolve_cu))
  expect_prefixed_match(
    setNames(suppressWarnings(dissolve_pb_df(df, pyromorphite = "Xie")), c("id", "defined", "defined_controlling_solid", "defined_tot_dissolved_pb")),
    map_base(df, dissolve_pb, pyromorphite = "Xie")
  )
  expect_prefixed_match(
    suppressWarnings(solvect_chlorine_df(df, time = 30, residual = 1.5, baffle = 0.4)),
    map_base(df, solvect_chlorine, time = 30, residual = 1.5, baffle = 0.4)
  )
  expect_prefixed_match(
    suppressWarnings(solvect_o3_df(df, time = 10, dose = 2, kd = -0.3, baffle = 0.6)),
    map_base(df, solvect_o3, time = 10, dose = 2, kd = -0.3, baffle = 0.6)
  )
  expect_identical(
    suppressWarnings(solveresid_o3_df(df, dose = 2, time = 10))$o3resid,
    unlist(map_base(df, solveresid_o3, dose = 2, time = 10))
  )
  expect_identical(
    suppressWarnings(solvedose_ph_df(df, target_ph = 8.5, chemical = "naoh"))$dose,
    unlist(map_base(df, solvedose_ph, target_ph = 8.5, chemical = "naoh"))
  )
  expect_identical(
    suppressWarnings(solvedose_alk_df(df, target_alk = 150, chemical = "nahco3"))$dose,
    unlist(map_base(df, solvedose_alk, target_alk = 150, chemical = "nahco3"))
  )
  expect_identical(
    suppressWarnings(scavenge_oh_df(df, dose = 10))$oh_scavenging,
    unlist(map_base(df, scavenge_oh, dose = 10))
  )
})
