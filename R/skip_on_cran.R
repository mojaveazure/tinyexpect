
#' Stop Testing on CRAN
#'
#' @return If called within a \CRANpkg{tinytest} test running on CRAN in a
#' \link[base:interactive ]{non-interactive session}, triggers an exit
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
#' @examples
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_envvar(c(NOT_CRAN = 'false'), skip_on_cran())
#' }
#'
skip_on_cran <- \() if (!interactive() && !.is_var(var = 'NOT_CRAN')) skip(message = "On CRAN")

#' @rdname skip_on_cran
#' @export
#'
exit_on_cran <- skip_on_cran
