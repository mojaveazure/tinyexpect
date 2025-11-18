
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
#' expect_type(1L, "integer") # Pass
#' expect_type(1.0, "integer") # Fail
#'
expect_type <- function(current, type, ..., info = NA_character_) {
  whoami <- sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
  if (...length()) {
    stop(.dots_err(call = whoami))
  }
  if (!(is.character(x = info) && length(x = info) == 1L)) {
    stop(.info_err(call = whoami))
  }
  if (!.is_string(x = type)) {
    stop(simpleError(
      message = "'type' must be a single non-empty string",
      call = whoami
    ))
  }
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
