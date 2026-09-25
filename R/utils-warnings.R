# Warning helpers
# These functions keep cli-formatted warning text without paying cli's
# formatting cost on every call.

# cli renders a message before it signals the condition, which takes several
# milliseconds. That is irrelevant for a single call, but the _df helpers and
# the uniroot-based solvers call the underlying models thousands of times, and
# the cost is paid even when the warning is immediately suppressed (see
# solvedose_ph) or deduplicated. Almost every tidywater warning is a fixed
# template that renders identically every time, so the rendered text is cached
# and reused instead of being rebuilt on each call.
warning_cache <- new.env(parent = emptyenv())

# A cli template renders the same way every time as long as it only uses inline
# classes applied to literal text ("{.arg coeff}") and never interpolates a
# variable ("{water@ph}") or pluralizes ("{?s}").
is_static_message <- function(message) {
  if (!any(grepl("{", message, fixed = TRUE))) {
    return(TRUE)
  }
  # Drop inline classes with literal contents; anything left is dynamic.
  stripped <- gsub("[{][.][a-zA-Z_]+ [^{}]*[}]", "", message)
  !any(grepl("[{}]", stripped))
}

# Rendered text for a static template, or NA if the template is dynamic and has
# to go through cli every time. Cached per template, ANSI color support, and
# console width, since those are what the rendering depends on.
render_warning <- function(message) {
  collapsed <- if (is.null(names(message)) && length(message) == 1L) {
    message
  } else {
    paste0(names(message), "\r", message, collapse = "\n")
  }
  key <- paste0(cli::num_ansi_colors(), "/", cli::console_width(), "/", collapsed)
  cached <- warning_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  rendered <- if (is_static_message(message)) {
    paste(cli::format_message(message), collapse = "\n")
  } else {
    NA_character_
  }
  assign(key, rendered, envir = warning_cache)
  rendered
}

#' Emit a cli-formatted warning
#'
#' Drop-in replacement for [cli::cli_warn()] that reuses the rendered text of
#' static message templates. The warning text is identical to what
#' [cli::cli_warn()] produces; only the time spent building it differs.
#'
#' @param message Message to display, as passed to [cli::cli_warn()].
#' @param ... Passed to [cli::cli_warn()].
#' @param .envir Environment used to evaluate glue expressions in `message`.
#'
#' @returns Called for the warning it emits.
#' @noRd
tidywater_warn <- function(message, ..., .envir = parent.frame()) {
  if (...length() == 0L) {
    rendered <- render_warning(message)
    if (!is.na(rendered)) {
      warning(rendered, call. = FALSE)
      return(invisible(NULL))
    }
  }
  cli::cli_warn(message, ..., .envir = .envir)
}
