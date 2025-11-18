
#' Stop Testing if a Required Package is Not Installed
#'
#' Conditionally stop testing a \CRANpkg{tinytest} test file if a required
#' package is not available or not of a minimum required version
#'
#' @param pkg Name of package to check for
#' @param minimum_version Optional minimum required version of \code{pkg}
#' @param quietly Attempt to find the package quietly; passed to
#' \code{\link[base]{requireNamespace}()}
#'
#' @return If called within a \CRANpkg{tinytest} test and \code{pkg} is either
#' not installed or not at least \code{minimum_version}, triggers an exit
#' condition; otherwise, returns one of
#' \itemize{
#'  \item A string saying that \code{pkg} is not installed
#'  \item A string saying that \code{pkg} is installed, but not
#'   at least \code{minimum_version}
#'  \item \code{NULL} invisibly
#' }
#'
#' @export
#'
#' @template section-skip-exit
#'
#' @family skip
#'
#' @templateVar fxn skip_if_not_installed
#' @template link-testthat
#'
#' @examples
#' (pkg <- paste(sample(letters, size = 7L, replace = TRUE), collapse = ""))
#' skip_if_not_installed(pkg)
#'
#' skip_if_not_installed("tinyexpect", minimum_version = "99.0.1")
#'
skip_if_not_installed <- function(pkg, minimum_version = NULL, quietly = TRUE) {
  if (!requireNamespace(pkg, quietly = quietly)) {
    return(skip(message = sprintf(fmt = "%s cannot be loaded", pkg)))
  }
  if (!is.null(minimum_version)) {
    installed <- utils::packageVersion(pkg = pkg)
    if (installed < minimum_version) {
      return(skip(message = sprintf(
        fmt = "Installed version of %s is %s, but %s is required",
        pkg,
        installed,
        minimum_version
      )))
    }
  }
  return(invisible(x = NULL))
}

#' @rdname skip_if_not_installed
#' @export
#'
exit_if_not_installed <- skip_if_not_installed
