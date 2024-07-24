
#' Stop Testing on CI
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
#' @examples
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_envvar(c(CI = 'true'), skip_on_ci())
#' }
#'
skip_on_ci <- \() if (.is_var(var = 'CI')) skip(message = "On CI")

#' @rdname skip_on_ci
#' @export
#'
exit_on_ci <- skip_on_ci
