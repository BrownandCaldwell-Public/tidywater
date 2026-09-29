# Helper to grab the text of the first warning an expression emits.
warn_text <- function(expr) {
  conditionMessage(tryCatch(expr, warning = function(w) w))
}

test_that("tidywater_warn matches cli_warn for static templates", {
  testthat::skip_on_cran()
  templates <- list(
    "No coagulants dosed; final water will equal input water.",
    "Use {.fn chemdose_chloramine} for breakpoint calculations.",
    "Sodium-containing chemical dosed, but {.field na} slot is {.val NA}; slot not updated.",
    "Existing slot overridden. To sum results instead, set {.code use_chlorine_slot = TRUE}.",
    c("Some parameters are missing and will be {.val NA}:", i = "First note.", i = "Second note.")
  )

  for (template in templates) {
    expect_equal(warn_text(tidywater_warn(template)), warn_text(cli::cli_warn(template)))
  }
})

test_that("tidywater_warn still interpolates dynamic templates", {
  testthat::skip_on_cran()
  ph <- 8.5
  expect_equal(
    warn_text(tidywater_warn("pH was {ph}.")),
    warn_text(cli::cli_warn("pH was {ph}."))
  )
  expect_match(warn_text(tidywater_warn("pH was {ph}.")), "pH was 8.5.")

  n <- 3
  expect_match(warn_text(tidywater_warn("{n} row{?s} affected.")), "3 rows affected.")

  arg <- "coeff"
  expect_match(warn_text(tidywater_warn("Check {.arg {arg}}.")), "Check `coeff`.")
})
