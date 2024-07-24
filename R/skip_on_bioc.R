
#' Stop Testing on the Bioconductor Build System
#'
#' @return If called within a \CRANpkg{tinytest} test running on the
#' Bioconductor Build System, triggers an exit condition; otherwise, either
#' a string saying \dQuote{\code{On Bioconductor}} or \code{NULL} invisibly
#'
#' @export
#'
#' @family skip
#'
#' @templateVar fxn skip_on_bioc
#' @template link-testthat
#'
#' @examples
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::with_envvar(c(IS_BIOC_BUILD_MACHINE = 'true'), skip_on_bioc())
#' }
#'
skip_on_bioc <- \() if (.is_var(var = 'IS_BIOC_BUILD_MACHINE'))   skip(message = "On Bioconductor")

#' @rdname skip_on_bioc
#' @export
#'
exit_on_bioc <- skip_on_bioc
