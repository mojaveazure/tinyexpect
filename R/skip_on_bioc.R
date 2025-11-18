
#' Stop Testing on the Bioconductor Build System
#'
#' Conditionally stop testing a \CRANpkg{tinytest} test file if running on
#' the Bioconductor Build System. This is determined by checking if the
#' environment variable \dQuote{\code{IS_BIOC_BUILD_MACHINE}} set and is
#' a non-\code{FALSE} value
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
#' @examplesIf requireNamespace("withr", quietly = TRUE)
#' withr::with_envvar(c(IS_BIOC_BUILD_MACHINE = "true"), skip_on_bioc())
#'
skip_on_bioc <- function() {
  if (.is_var(var = 'IS_BIOC_BUILD_MACHINE')) {
    return(skip(message = "On Bioconductor"))
  }
  return(invisible(x = NULL))
}

#' @rdname skip_on_bioc
#' @export
#'
exit_on_bioc <- skip_on_bioc
