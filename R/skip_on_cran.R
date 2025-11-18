
#' Stop Testing on CRAN
#'
#' Conditionally stop testing a \CRANpkg{tinytest} test file if running
#' under \CRANpkg{covr}. This is determined by checking if
#' \itemize{
#'  \item \R is in a non-\link[base]{interactive} session, and
#'  \item the environment variable \dQuote{\code{NOT_CRAN}} is unset or is not
#'   a \code{TRUE} value
#' }
#'
#' @return If called within a \CRANpkg{tinytest} test running on CRAN in a
#' \link[base:interactive]{non-interactive session}, triggers an exit
#' condition; otherwise, either a string saying \dQuote{\code{On CRAN}}
#' or \code{NULL} invisibly
#'
#' @export
#'
#' @template section-skip-exit
#'
#' @family skip
#'
#' @templateVar fxn skip_on_cran
#' @template link-testthat
#'
#' @examplesIf !interactive() && requireNamespace("withr", quietly = TRUE)
#' withr::with_envvar(c(NOT_CRAN = "false"), skip_on_cran())
#'
skip_on_cran <- function() {
  if (!interactive() && !.is_var(var = 'NOT_CRAN')) {
    return(skip(message = "On CRAN"))
  }
  return(invisible(x = NULL))
}

#' @rdname skip_on_cran
#' @export
#'
exit_on_cran <- skip_on_cran
