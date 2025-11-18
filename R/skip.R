
#' Stop Testing
#'
#' Unconditionally stop testing a \CRANpkg{tinytest} test file, preventing
#' additional tests in the file from running without triggering a failure. This
#' is the low-level exit function for other \code{skip_()} functions in
#' \pkg{tinyexpect}
#'
#' @param message A message describing why the test file is being skipped
#'
#' @return If called within a \CRANpkg{tinytest} test, triggers an exit
#' condition; otherwise, returns \code{message}
#'
#' @export
#'
#' @family skip
#'
#' @templateVar fxn skip
#' @template link-testthat
#'
#' @seealso \code{\link[tinytest:exit_file]{tinytest::exit_file}()}
#'
#' @examples
#' skip()
#'
skip <- function(message = 'Skipping') {
  if (!(is.character(x = message) && length(x = message) == 1L)) {
    stop(simpleError(
      message = "'message' must be a single character value",
      call = sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
    ))
  }
  fxn <- get(x = 'capture_exit', envir = asNamespace(ns = 'tinytest'))()
  parents <- Filter(
    f = function(idx) {
      return('exit_file' %in% names(x = sys.frame(which = idx)) &&
        is.function(x = sys.frame(which = idx)$exit_file) &&
        identical(
          x = body(fun = sys.frame(which = idx)$exit_file),
          y = body(fun = fxn)
        ))
    },
    x = if (isFALSE(x = getOption(x = 'tinyexpect.skip', default = TRUE))) {
      sys.nframe()
    } else {
      unique(x = sys.parents())
    }
  )
  if (!length(x = parents)) {
    return(message)
  }
  if (length(x = parents) > 1L) {
    warning(
      "Multiple tinytest environments found, using the highest up",
      immediate. = TRUE
    )
    parents <- parents[1L]
  }
  return(sys.frame(which = parents)$exit_file(message))
}
