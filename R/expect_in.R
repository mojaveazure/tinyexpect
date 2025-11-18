
#' Expect a Subset
#'
#' Expectation predicate for testing that a vector is fully contained within
#' another vector
#'
#' @inheritParams expect_contains
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family set
#'
#' @templateVar fxn expect_in
#' @template link-testthat
#'
#' @examples
#' expect_in(letters, target = rev(letters)) # Pass
#' expect_in(letters, target = "b") # Fail
#' expect_in("b", target = letters) # Pass
#'
expect_in <- function(
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
    result = all(current %in% target),
    call = call,
    diff = "Expected all values of 'current' to be in 'target'",
    short = "xcpt",
    info = info
  ))
}
