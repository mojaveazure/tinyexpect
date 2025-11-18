
#' Expect Invisible
#'
#' @template param-current
#' @template param-dots-ignored
#' @template param-info
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family visibility
#'
#' @templateVar fxn expect_invisible
#' @template link-testthat
#'
#' @seealso \code{\link{withVisible}()}, \code{\link{invisible}()}
#'
#' @examples
#' f <- function() invisible()
#' expect_invisible(f()) # Pass
#'
#' g <- function() NULL
#' expect_invisible(g()) # Fail
#'
expect_invisible <- function(current, ..., info = NA_character_) {
  whoami <- sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
  if (...length()) {
    stop(.dots_err(call = whoami))
  }
  if (!(is.character(x = info) && length(x = info) == 1L)) {
    stop(.info_err(call = whoami))
  }
  call <- sys.call(which = sys.parent(n = 0L))
  return(tinytest(
    result = isFALSE(x = withVisible(x = current)$visible),
    call = call,
    diff = "Expected invisible result, got a visible object",
    short = 'attr',
    info = info
  ))
}
