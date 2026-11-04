# Golden-file regression helpers ----
#
# These tests pin the package's *current* numeric output so refactors can't silently
# change results. They are not validation against literature; reference checks live in
# the regular test files. See tests/testthat/README.md for the workflow.
#
# Usage:
#   expect_golden(list(case_a = water1, case_b = df2), "chemdose_ph_chemicals")
#
# Each output is flattened into long rows of (case, output, value) and written as a text
# snapshot in tests/testthat/_snaps/<test file>/<name>.txt. On later runs, numeric values
# are compared cell-by-cell with |new - old| <= abs_tol + rel_tol * |old|, and everything
# else (text, NA) must match exactly. Rows added or removed are failures.

# Format numbers with enough digits to round-trip, and consistently across platforms.
golden_format <- function(x) {
  if (is.numeric(x)) {
    out <- ifelse(is.na(x), "NA", ifelse(is.nan(x), "NaN", sprintf("%.15g", x)))
    out[is.infinite(x)] <- ifelse(x[is.infinite(x)] > 0, "Inf", "-Inf")
    out
  } else if (is.logical(x)) {
    ifelse(is.na(x), "NA", as.character(x))
  } else {
    # Keep one value per line.
    x <- gsub("\r?\n", "\\\\n", as.character(x))
    ifelse(is.na(x), "NA", x)
  }
}

# Flatten one output into a data frame with columns output, value.
golden_flatten <- function(x) {
  if (methods::is(x, "water")) {
    slots <- setdiff(methods::slotNames(x), "ks")
    vals <- lapply(slots, function(s) methods::slot(x, s))
    out <- data.frame(output = slots, value = vapply(vals, golden_format, character(1)))
    if (nrow(x@ks) > 0) {
      ks <- golden_flatten(as.list(x@ks[1, , drop = FALSE]))
      ks$output <- paste0("ks.", ks$output)
      out <- rbind(out, ks)
    }
    out
  } else if (is.data.frame(x)) {
    # Nested water columns (from _df helpers) are flattened per row.
    rows <- lapply(seq_len(nrow(x)), function(i) {
      row <- lapply(names(x), function(col) {
        v <- x[[col]][[i]]
        f <- golden_flatten(v)
        f$output <- if (length(f$output) == 1 && f$output == "") col else paste0(col, ".", f$output)
        f
      })
      row <- do.call(rbind, row)
      if (nrow(x) > 1) row$output <- paste0("[", i, "].", row$output)
      row
    })
    do.call(rbind, rows)
  } else if (is.list(x)) {
    if (is.null(names(x))) names(x) <- seq_along(x)
    parts <- lapply(names(x), function(nm) {
      f <- golden_flatten(x[[nm]])
      f$output <- if (length(f$output) == 1 && f$output == "") nm else paste0(nm, ".", f$output)
      f
    })
    do.call(rbind, parts)
  } else if (is.atomic(x) && length(x) == 1 && is.null(names(x))) {
    data.frame(output = "", value = golden_format(x))
  } else if (is.atomic(x)) {
    nms <- if (is.null(names(x))) as.character(seq_along(x)) else names(x)
    data.frame(output = nms, value = golden_format(unname(x)))
  } else {
    stop("Don't know how to flatten object of class ", class(x)[1])
  }
}

golden_table <- function(cases) {
  stopifnot(is.list(cases), !is.null(names(cases)), all(names(cases) != ""))
  parts <- lapply(names(cases), function(nm) {
    f <- golden_flatten(cases[[nm]])
    f$output[f$output == ""] <- "value"
    data.frame(case = nm, output = f$output, value = f$value)
  })
  do.call(rbind, parts)
}

# Golden files are plain text, one value per line: `case :: output = value`.
# (The repo .gitignore blocks *.csv, and one-line-per-value gives the clearest diffs.)
golden_write <- function(tbl, path) {
  writeLines(paste0(tbl$case, " :: ", tbl$output, " = ", tbl$value), path, useBytes = TRUE)
}

golden_read <- function(path) {
  lines <- readLines(path, warn = FALSE)
  # Keys never contain " = ", so split on the first one; values may contain "=".
  m <- regmatches(lines, regexec("^(.*?) = (.*)$", lines, perl = TRUE))
  data.frame(
    key = vapply(m, `[`, character(1), 2),
    value = vapply(m, `[`, character(1), 3)
  )
}

# Returns a compare function for expect_snapshot_file().
golden_compare <- function(rel_tol, abs_tol) {
  function(old, new) {
    a <- golden_read(old)
    b <- golden_read(new)
    key_a <- a$key
    key_b <- b$key
    if (!identical(key_a, key_b)) {
      missing <- setdiff(key_a, key_b)
      added <- setdiff(key_b, key_a)
      message(
        "Golden rows changed.",
        if (length(missing)) paste0("\n  removed: ", paste(utils::head(missing, 10), collapse = ", ")),
        if (length(added)) paste0("\n  added: ", paste(utils::head(added, 10), collapse = ", "))
      )
      return(FALSE)
    }
    na <- suppressWarnings(as.numeric(a$value))
    nb <- suppressWarnings(as.numeric(b$value))
    numeric <- !is.na(na) & !is.na(nb) & is.finite(na) & is.finite(nb)
    ok <- ifelse(numeric, abs(nb - na) <= abs_tol + rel_tol * abs(na), a$value == b$value)
    if (all(ok)) {
      return(TRUE)
    }
    bad <- which(!ok)
    detail <- sprintf("  %s: %s -> %s", key_a[bad], a$value[bad], b$value[bad])
    message(
      "Golden values changed (rel_tol = ", rel_tol, ", abs_tol = ", abs_tol, "), ",
      length(bad), " cell(s):\n",
      paste(utils::head(detail, 25), collapse = "\n"),
      if (length(bad) > 25) sprintf("\n  ... and %d more", length(bad) - 25)
    )
    FALSE
  }
}

expect_golden <- function(cases, name, rel_tol = 1e-8, abs_tol = 0) {
  tbl <- golden_table(cases)
  path <- tempfile(fileext = ".txt")
  golden_write(tbl, path)
  expect_snapshot_file(
    path,
    name = paste0(name, ".txt"),
    compare = golden_compare(rel_tol, abs_tol)
  )
}

# Standard scenario waters ----
# Used across regression tests so failures can be traced to a known input. Each is also
# pinned directly in test-regression-define_water.R.
regression_waters <- function() {
  suppressWarnings(list(
    typical = define_water(
      ph = 7.5, temp = 20, alk = 80, tot_hard = 120, ca = 32, mg = 10, na = 20, k = 4,
      cl = 30, so4 = 40, tds = 250, toc = 3.5, doc = 3.2, uv254 = 0.1, br = 50
    ),
    low_alk = define_water(
      ph = 6.8, temp = 15, alk = 15, tot_hard = 25, na = 5, k = 1, cl = 8, so4 = 6,
      toc = 2.5, doc = 2.3, uv254 = 0.06
    ),
    hard_high_alk = define_water(
      ph = 7.9, temp = 22, alk = 250, tot_hard = 350, ca = 100, mg = 25, na = 60, k = 8,
      cl = 90, so4 = 120, cond = 900, toc = 4, doc = 3.8, uv254 = 0.08
    ),
    high_toc = define_water(
      ph = 6.5, temp = 25, alk = 40, tot_hard = 60, na = 10, cl = 15, so4 = 10, tds = 150,
      toc = 8, doc = 7.5, uv254 = 0.35, br = 120
    ),
    cold = define_water(
      ph = 7.2, temp = 5, alk = 60, tot_hard = 80, ca = 22, mg = 6, na = 12, k = 2,
      cl = 18, so4 = 20, cond = 250, toc = 2, doc = 1.9, uv254 = 0.04
    ),
    warm = define_water(
      ph = 8.2, temp = 30, alk = 120, tot_hard = 150, na = 30, k = 5, cl = 40, so4 = 50,
      tds = 400, toc = 3, doc = 2.8, uv254 = 0.07, br = 80
    ),
    nutrients = define_water(
      ph = 7.4, temp = 18, alk = 70, tot_hard = 90, na = 15, k = 3, cl = 20, so4 = 25,
      tds = 220, toc = 3, doc = 2.8, uv254 = 0.09, tot_po4 = 1.5, tot_nh3 = 0.8,
      tot_bo4 = 0.3, tot_sio4 = 12, free_chlorine = 1
    ),
    ideal = define_water(ph = 7, temp = 25, alk = 50, toc = 3, uv254 = 0.08)
  ))
}
