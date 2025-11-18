
#' Stop Testing Under \CRANpkg{covr}
#'
#' Conditionally stop testing a \CRANpkg{tinytest} test file if running
#' under \CRANpkg{covr}. This is determined by checking if the environment
#' variable \dQuote{\code{R_COVR}} set and is a non-\code{FALSE} value
#'
#' @return If called within a \CRANpkg{tinytest} test running under
#' \CRANpkg{covr}, triggers an exit condition; otherwise, either a
#' string saying \dQuote{\code{On covr}} or \code{NULL} invisibly
#'
#' @export
#'
#' @template section-skip-exit
#'
#' @family skip
#'
#' @templateVar fxn skip_on_covr
#' @template link-testthat
#'
#' @examplesIf requireNamespace("withr", quietly = TRUE)
#' withr::with_envvar(c(R_COVR = "true"), skip_on_covr())
#'
skip_on_covr <- function() {
  if (.is_var(var = 'R_COVR')) {
    return(skip(message = "On covr"))
  }
  return(invisible(x = NULL))
}

#' @rdname skip_on_covr
#' @export
#'
exit_on_covr <- skip_on_covr
