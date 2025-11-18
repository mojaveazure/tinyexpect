
#' Expect a Superset
#'
#' Expectation predicate for testing that a given vector contains all values
#' present in another vector
#'
#' @param current \code{[R vector]} A \link[base]{vector} to be tested
#' @param target \code{[R vector]} A \link[base]{vector} to be compared against
#' @template param-dots-ignored
#' @template param-info
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family set
#'
#' @templateVar fxn expect_contains
#' @template link-testthat
#'
#' @examples
#' expect_contains(letters, target = rev(letters)) # Pass
#' expect_contains(letters, target = "b") # Pass
#' expect_contains("b", target = letters) # Fail
#'
expect_contains <- function(
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

  call <- sys.call(which = sys.parent(n = 0L))
  return(tinytest(
    result = all(target %in% current),
    call = call,
    diff = "Expected 'current' to contain all values of 'target'",
    short = "xcpt",
    info = info
  ))
}
