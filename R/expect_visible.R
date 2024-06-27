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
#' f <- \() invisible()
#' expect_visible(f())
#'
#' g <- \() NULL
#' expect_visible(g())
#'
#'
expect_visible <- function(current, ..., info = NA_character_) {
  rlang::check_dots_empty0(...)
  stopifnot(rlang::is_scalar_character(x = info))
  call <- sys.call(which = sys.parent(n = 0L))
  return(tinytest(
    result = isTRUE(x = withVisible(x = current)$visible),
    call = call,
    diff = "Expected visible result, got an invisible object",
    short = 'attr',
    info = info
  ))
}
