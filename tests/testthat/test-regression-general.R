# Regression pins: helpers in R/general.R and solve_ph ----
# Golden files pin current numeric output. See tests/testthat/README.md.

test_that("regression: correct_k across waters and temperatures", {
  testthat::skip_on_cran()
  w <- regression_waters()
  extra <- suppressWarnings(list(
    t1_tds300 = define_water(ph = 7, temp = 1, alk = 50, tds = 300),
    t35_tds300 = define_water(ph = 7, temp = 35, alk = 50, tds = 300),
    t25_is_na = define_water(ph = 7, temp = 25, alk = 50),
    t10_is_na = define_water(ph = 7, temp = 10, alk = 50),
    t25_high_is = define_water(ph = 7, temp = 25, alk = 50, tds = 2000)
  ))
  out <- lapply(c(w, extra), function(x) suppressWarnings(correct_k(x)))
  expect_golden(out, "correct_k")
})

test_that("regression: alpha functions over pH and temperature", {
  testthat::skip_on_cran()
  alpha_fns <- c(
    "calculate_alpha0_carbonate", "calculate_alpha1_carbonate", "calculate_alpha2_carbonate",
    "calculate_alpha0_phosphate", "calculate_alpha1_phosphate", "calculate_alpha2_phosphate",
    "calculate_alpha3_phosphate", "calculate_alpha1_hypochlorite", "calculate_alpha1_ammonia",
    "calculate_alpha1_borate", "calculate_alpha1_silicate", "calculate_alpha2_silicate",
    "calculate_alpha1_acetate", "calculate_alpha0_citric", "calculate_alpha1_citric",
    "calculate_alpha2_citric", "calculate_alpha3_citric"
  )
  ph <- seq(4, 11, by = 0.5)
  h <- 10^-ph
  out <- list()
  for (temp in c(10, 25)) {
    ks <- suppressWarnings(define_water(ph = 7, temp = temp, alk = 50, tds = 200))@ks
    for (fn in alpha_fns) {
      vals <- get(fn)(h, ks)
      names(vals) <- paste0("ph", ph)
      out[[paste0("t", temp, "_", sub("calculate_", "", fn))]] <- vals
    }
  }
  expect_golden(out, "alpha_functions")
})

test_that("regression: calculate_activity", {
  testthat::skip_on_cran()
  grid <- expand.grid(z = 1:3, is = c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5), temp = c(5, 25, 35))
  out <- lapply(seq_len(nrow(grid)), function(i) calculate_activity(grid$z[i], grid$is[i], grid$temp[i]))
  names(out) <- paste0("z", grid$z, "_is", grid$is, "_t", grid$temp)
  out$z1_isNA_t25 <- calculate_activity(1, NA, 25)
  out$z2_isNA_t5 <- calculate_activity(2, NA, 5)
  out$z3_isNA_t35 <- calculate_activity(3, NA, 35)
  expect_golden(out, "calculate_activity")
})

test_that("regression: calculate_ionicstrength on scenario waters", {
  testthat::skip_on_cran()
  out <- lapply(regression_waters(), calculate_ionicstrength)
  expect_golden(out, "calculate_ionicstrength")
})

test_that("regression: correlate_ionicstrength all conversions", {
  testthat::skip_on_cran()
  vals <- c(0, 0.005, 150, 500, 2000)
  combos <- list(
    c("cond", "is"), c("tds", "is"), c("is", "tds"),
    c("is", "cond"), c("tds", "cond"), c("cond", "tds")
  )
  out <- list()
  for (cb in combos) {
    res <- correlate_ionicstrength(vals, from = cb[1], to = cb[2])
    names(res) <- paste0("v", vals)
    out[[paste0(cb[1], "_to_", cb[2])]] <- res
  }
  out$default_args <- correlate_ionicstrength(500)
  expect_golden(out, "correlate_ionicstrength")
})

test_that("regression: K_temp_adjust for every dissociation constant", {
  testthat::skip_on_cran()
  temps <- c(0, 5, 15, 25, 35)
  out <- lapply(rownames(discons), function(r) {
    res <- K_temp_adjust(discons[r, ]$deltah, discons[r, ]$k, temps)
    names(res) <- paste0("t", temps)
    res
  })
  names(out) <- rownames(discons)
  expect_golden(out, "K_temp_adjust")
})

test_that("regression: calculate_hardness types and units", {
  testthat::skip_on_cran()
  inputs <- list(
    `mg/L` = c(ca = 40, mg = 12),
    `g/L` = c(ca = 0.04, mg = 0.012),
    `ug/L` = c(ca = 40000, mg = 12000),
    M = c(ca = 1e-3, mg = 5e-4),
    mM = c(ca = 1, mg = 0.5),
    uM = c(ca = 1000, mg = 500),
    `meq/L` = c(ca = 2, mg = 1),
    `eq/L` = c(ca = 0.002, mg = 0.001),
    `mg/L CaCO3` = c(ca = 100, mg = 50)
  )
  out <- list()
  for (u in names(inputs)) {
    for (type in c("total", "ca")) {
      out[[paste0(type, "_", u)]] <- suppressWarnings(
        calculate_hardness(inputs[[u]][["ca"]], inputs[[u]][["mg"]], type = type, startunit = u)
      )
    }
  }
  out$total_default <- calculate_hardness(50, 10)
  out$total_vector <- calculate_hardness(c(10, 50, 100), c(2, 10, 30))
  out$total_na_mg <- calculate_hardness(50, NA)
  expect_golden(out, "calculate_hardness")
})

test_that("regression: calc_suva", {
  testthat::skip_on_cran()
  expect_golden(
    list(
      typical = calc_suva(3.2, 0.1),
      high = calc_suva(7.5, 0.35),
      vector = calc_suva(c(1, 2, 4), c(0.02, 0.05, 0.2)),
      na_doc = calc_suva(NA, 0.1)
    ),
    "calc_suva"
  )
})

# Formulas x units matrix. Combinations that error under current code (e.g. equivalents
# for formulas with no known charge) are dropped rather than pinned.
convert_units_matrix <- function(formulas, units, from, to) {
  out <- list()
  for (f in formulas) {
    for (s in from) {
      for (e in to) {
        res <- tryCatch(suppressWarnings(convert_units(1.5, f, s, e)), error = function(err) NULL)
        if (!is.null(res)) out[[paste0(f, " [", s, " -> ", e, "]")]] <- res
      }
    }
  }
  out
}

test_that("regression: convert_units formulas x units", {
  testthat::skip_on_cran()
  units <- c(
    "ng/L", "ug/L", "mg/L", "g/L",
    "ng/L CaCO3", "ug/L CaCO3", "mg/L CaCO3", "g/L CaCO3",
    "ng/L N", "ug/L N", "mg/L N", "g/L N",
    "M", "mM", "uM", "nM", "neq/L", "ueq/L", "meq/L", "eq/L"
  )
  formulas <- c(
    "ca", "mg", "na", "k", "cl", "so4", "hco3", "co3", "po4", "hpo4", "caco3", "n", "nh4",
    "no3", "h2so4", "alum", "al2o3", "ferricchloride", "citric", "h3bo3", "dic", "b", "f",
    "cl2", "sio2"
  )
  expect_golden(
    convert_units_matrix(formulas, units, from = "mg/L", to = units),
    "convert_units_from_mgl"
  )
  expect_golden(
    convert_units_matrix(formulas, units, from = units, to = "M"),
    "convert_units_to_M"
  )
  expect_golden(
    convert_units_matrix(
      c("ca", "mg", "hco3", "nh4", "no3", "n"),
      units,
      from = c("mg/L CaCO3", "mg/L N", "meq/L", "mM"),
      to = units
    ),
    "convert_units_cross"
  )
})

test_that("regression: convert_units string normalization and vectors", {
  testthat::skip_on_cran()
  expect_golden(
    suppressWarnings(list(
      lower_case_units = convert_units(50, "ca", "mg/l", "m"),
      spaces_in_units = convert_units(50, "ca", "mg / L", "mg / L CaCO3"),
      as_caco3 = convert_units(50, "ca", "mg/L", "mg/L as CaCO3"),
      upper_formula = convert_units(50, "CA", "mg/L", "mM"),
      spaced_formula = convert_units(50, " so4 ", "mg/L", "meq/L"),
      meq_lower = convert_units(2, "mg", "meq/l", "mg/l"),
      fuzzy_formula = convert_units(50, "caco", "mg/L", "M"),
      vector_value = convert_units(c(0, 1, 10, 100), "na", "mg/L", "M"),
      na_value = convert_units(NA, "na", "mg/L", "M"),
      negative_value = convert_units(-10, "caco3", "mg/L", "M"),
      default_units = convert_units(50, "ca")
    )),
    "convert_units_normalization"
  )
  # Unrecognized units no longer fuzzy-match to a nearby unit (#561).
  expect_error(convert_units(50, "ca", "mg/L", "mg/L CaCO"), "Units not supported")
})

test_that("regression: convert_flow units and specific gravity", {
  testthat::skip_on_cran()
  vol <- c(
    "MGS", "MGM", "MGH", "MGD", "gps", "gpm", "gph", "gpd", "cfs", "cfm", "cfh", "cfd",
    "L/s", "L/m", "L/h", "L/d", "m3/s", "m3/m", "m3/h", "m3/day"
  )
  mass <- c(
    "kg/s", "kg/m", "kg/min", "kg/h", "kg/d", "kg/day", "g/s", "g/m", "g/min", "g/h", "g/d",
    "g/day", "lb/s", "lbs/s", "lb/m", "lbs/m", "lb/min", "lbs/min", "lb/h", "lbs/h", "lb/d",
    "lbs/d", "lb/day", "lbs/day"
  )
  out <- list()
  for (u in vol) {
    out[[paste0(u, " -> gpm")]] <- convert_flow(10, u, "gpm")
    out[[paste0("gpm -> ", u)]] <- convert_flow(10, "gpm", u)
    out[[paste0(u, " -> lb/day sg1")]] <- convert_flow(10, u, "lb/day")
    out[[paste0(u, " -> kg/h sg1.2")]] <- convert_flow(10, u, "kg/h", sg = 1.2)
  }
  for (u in mass) {
    out[[paste0(u, " -> lb/min")]] <- convert_flow(10, u, "lb/min")
    out[[paste0("lb/min -> ", u)]] <- convert_flow(10, "lb/min", u)
    out[[paste0(u, " -> MGD sg1")]] <- convert_flow(10, u, "MGD")
    out[[paste0(u, " -> L/s sg1.4")]] <- convert_flow(10, u, "L/s", sg = 1.4)
  }
  out[["MGD -> m3/day vector"]] <- convert_flow(c(1, 5, 40), "MGD", "m3/day")
  out[["mgd lower -> gpm"]] <- convert_flow(1, "mgd", "gpm")
  expect_golden(out, "convert_flow")
})

test_that("regression: solve_ph directly", {
  testthat::skip_on_cran()
  w <- regression_waters()
  orgacid <- suppressWarnings(define_water(
    ph = 7, temp = 20, alk = 60, tds = 200, ca = 20, mg = 5, na = 15, cl = 20, so4 = 20,
    tot_ch3coo = 3, tot_citric = 2
  ))
  citric_only <- suppressWarnings(define_water(ph = 4.5, temp = 25, alk = 5, tds = 100, tot_citric = 10))
  acetate_only <- suppressWarnings(define_water(ph = 6, temp = 15, alk = 20, tds = 100, tot_ch3coo = 10))
  out <- lapply(c(w, list(orgacid = orgacid, citric_only = citric_only, acetate_only = acetate_only)), function(x) {
    suppressWarnings(solve_ph(x))
  })
  out <- c(out, suppressWarnings(list(
    typical_na_dose = solve_ph(w$typical, na_dose = 2e-4),
    typical_cl_dose = solve_ph(w$typical, cl_dose = 2e-4),
    typical_so4_dose = solve_ph(w$typical, so4_dose = 1e-4),
    typical_ca_mg_dose = solve_ph(w$typical, ca_dose = 1e-4, mg_dose = 5e-5),
    typical_mno4_no3_dose = solve_ph(w$typical, mno4_dose = 1e-5, no3_dose = 1e-4),
    orgacid_cl_dose = solve_ph(orgacid, cl_dose = 5e-4),
    ideal_na_dose = solve_ph(w$ideal, na_dose = 1e-4)
  )))
  expect_golden(out, "solve_ph")
})
