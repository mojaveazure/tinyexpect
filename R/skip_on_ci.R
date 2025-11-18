
#' Stop Testing on CI
#'
#' Conditionally stop testing a \CRANpkg{tinytest} test file if running on CI.
#' This is determined by checking if the environment variable \dQuote{\code{CI}}
#' set and is a non-\code{FALSE} value
#'
#' @return If called within a \CRANpkg{tinytest} test running on CI, triggers
#' an exit condition; otherwise, either a string saying \dQuote{\code{On CI}}
#' or \code{NULL} invisibly
#'
#' @export
#'
#' @template section-skip-exit
#'
#' @family skip
#'
#' @templateVar fxn skip_on_ci
#' @template link-testthat
#'
#' @examplesIf requireNamespace("withr", quietly = TRUE)
#' withr::with_envvar(c(CI = "true"), skip_on_ci())
#'
skip_on_ci <- function() {
  if (.is_var(var = 'CI')) {
    return(skip(message = "On CI"))
  }
  return(invisible(x = NULL))
}

#' @rdname skip_on_ci
#' @export
#'
exit_on_ci <- skip_on_ci
