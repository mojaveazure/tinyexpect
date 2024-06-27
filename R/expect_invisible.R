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
#' f <- \() invisible()
#' expect_invisible(f())
#'
#' g <- \() NULL
#' expect_invisible(g())
#'
expect_invisible <- function(current, ..., info = NA_character_) {
  rlang::check_dots_empty0(...)
  stopifnot(rlang::is_scalar_character(x = info))
  call <- sys.call(which = sys.parent(n = 0L))
  return(tinytest(
    result = isFALSE(x = withVisible(x = current)$visible),
    call = call,
    diff = "Expected invisible result, got a visible object",
    short = 'attr',
    info = info
  ))
}
