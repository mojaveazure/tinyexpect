
#' Stop Testing Under \CRANpkg{covr}
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
#' @examples
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_envvar(c(R_COVR = 'true'), skip_on_covr())
#' }
#'
skip_on_covr <- \() if (.is_var(var = 'R_COVR')) skip(message = "On covr")

#' @rdname skip_on_covr
#' @export
#'
exit_on_covr <- skip_on_covr
