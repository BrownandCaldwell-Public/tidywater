# Helper to grab the text of the first warning an expression emits.
warn_text <- function(expr) {
  conditionMessage(tryCatch(expr, warning = function(w) w))
}

test_that("tidywater_warn matches cli_warn for static templates", {
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

test_that("tidywater_warn caches static templates but still emits every time", {
  template <- "A uniquely worded static warning used only by this test."
  expect_warning(tidywater_warn(template), "uniquely worded")
  expect_warning(tidywater_warn(template), "uniquely worded")
  expect_equal(warn_text(tidywater_warn(template)), warn_text(cli::cli_warn(template)))
})

test_that("tidywater_warn still interpolates dynamic templates", {
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

test_that("is_static_message only accepts templates that render identically", {
  expect_true(is_static_message("No braces here."))
  expect_true(is_static_message("Check {.arg coeff} and {.val NA}."))
  expect_true(is_static_message(c("Heading {.field ph}:", i = "A bullet.")))

  expect_false(is_static_message("pH was {water@ph}."))
  expect_false(is_static_message("{n} row{?s} affected."))
  expect_false(is_static_message("Check {.arg {arg}}."))
  expect_false(is_static_message(c("Heading:", i = "Value is {x}.")))
})

test_that("cached warning text is re-rendered when console width changes", {
  template <- "A static warning used to check that the cache respects console width."

  old <- options(cli.width = 40)
  on.exit(options(old), add = TRUE)
  expect_equal(warn_text(tidywater_warn(template)), warn_text(cli::cli_warn(template)))

  # A cache keyed only on the template would keep serving the narrow text here.
  options(cli.width = 200)
  expect_equal(warn_text(tidywater_warn(template)), warn_text(cli::cli_warn(template)))
})
