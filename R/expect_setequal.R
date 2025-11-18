
#' Expect Set Equality
#'
#' Expectation predicate for testing that the \link[base]{sets} of two vectors
#' are equal
#'
#' @inheritParams expect_contains
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family set
#'
#' @templateVar fxn expect_setequal
#' @template link-testthat
#'
#' @examples
#' expect_setequal(letters, target = rev(letters)) # Pass
#' expect_setequal(letters, target = "b") # Fail
#' expect_setequal("b", target = letters) # Fail
#'
expect_setequal <- function(
  current,
  target,
  ...,
  info = NA_character_
) {
  whoami <- sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
  if (...length()) {
    stop(.dots_err(call = whoami))
  }
  if (!(is.character(x = info) && length(x = info) == 1L)) {
    stop(.info_err(call = whoami))
  }
  if (!(.is_vector(x = current) && .is_vector(x = target))) {
    stop(simpleError(
      message = "Both 'current' and 'target' must be vectors",
      call = whoami
    ))
  }
  if (!(is.null(x = names(x = current)) && is.null(x = names(x = target)))) {
    warning(simpleWarning(
      message = "'expect_setequal()' ignores names",
      call = whoami
    ))
  }
  call <- sys.call(which = sys.parent(n = 0L))
  return(tinytest(
    result = setequal(x = current, y = target),
    call = call,
    diff = "Expected 'current' and 'target' to be identical sets",
    short = "xcpt",
    info = info
  ))
}
