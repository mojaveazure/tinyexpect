#' Expect Object Type
#'
#' Check that \code{typeof(current)} is \code{type}
#'
#' @template param-current
#' @param type \code{[character(1L)]} Expected base type (as given by
#' \code{\link{typeof}()})
#' @template param-dots-ignored
#' @template param-info
#'
#' @inherit tinytest::expect_equal return
#'
#' @export
#'
#' @family inheritance
#'
#' @templateVar fxn expect_type
#' @template link-testthat
#'
#' @examples
#' expect_type(1L, "integer")
#' expect_type(1.0, "integer")
#'
expect_type <- function(current, type, ..., info = NA_character_) {
  rlang::check_dots_empty0(...)
  stopifnot(
    rlang::is_scalar_character(x = type) && nzchar(x = type),
    rlang::is_scalar_character(x = info)
  )
  call <- sys.call(which = sys.parent(n = 0L))
  ct <- typeof(x = current)
  return(tinytest(
    result = identical(x = ct, y = type),
    call = call,
    diff = sprintf(fmt = "Expected object of type '%s', got '%s'", type, ct),
    short = 'attr',
    info = info
  ))
}
