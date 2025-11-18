
#' Expect Visible
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
#' @templateVar fxn expect_visible
#' @template link-testthat
#'
#' @seealso \code{\link{withVisible}()}
#'
#' @examples
#' f <- function() invisible()
#' expect_visible(f()) # Fail
#'
#' g <- function() NULL
#' expect_visible(g()) # Pass
#'
#'
expect_visible <- function(current, ..., info = NA_character_) {
  whoami <- sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
  if (...length()) {
    stop(.dots_err(call = whoami))
  }
  if (!(is.character(x = info) && length(x = info) == 1L)) {
    stop(.info_err(call = whoami))
  }
  call <- sys.call(which = sys.parent(n = 0L))
  return(tinytest(
    result = isTRUE(x = withVisible(x = current)$visible),
    call = call,
    diff = "Expected visible result, got an invisible object",
    short = 'attr',
    info = info
  ))
}
