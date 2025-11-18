
#' Expect Names
#'
#' Expectation predicate for testing the \link[base]{names} of an object
#'
#' @template param-current
#' @param target \code{[missing, character, or NULL]} One of the following:
#' \itemize{
#'  \item Left \link[base]{missing} to test that \code{current} has names
#'  \item A character vector giving the expected names of \code{current}
#'  \item \code{NULL} to test that \code{current} is unnamed
#' }
#' @template param-dots-ignored
#' @param ignore.order \code{[logical(1L)]} Ignore the order of
#' \code{names(current)} with respect to \code{target}
#' @param ignore.case \code{[logical(1L)]} Ignore upper- vs lower-case
#' discrepancies between \code{names(current)} and \code{target}
#' @template param-info
#'
#' @template return-tinytest
#'
#' @export
#'
#' @templateVar fxn expect_named
#' @template link-testthat
#'
#' @examples
#' (x <- c(a = 1L, b = 2L, c = 3L))
#' (y <- c(4L, 5L, 6L))
#'
#' expect_named(x) # Pass
#' expect_named(y) # Fail
#'
#' expect_named(x, target = NULL) # Fail
#' expect_named(y, target = NULL) # Pass
#'
#' expect_named(x, target = c("a", "b", "c")) # Pass
#' expect_named(y, target = c("a", "b", "c")) # Fail
#'
#' expect_named(x, target = c("b", "a", "c")) # Fail
#' expect_named(x, target = c("b", "a", "c"), ignore.order = TRUE) # Pass
#'
#' expect_named(x, target = c("A", "B", "C")) # Fail
#' expect_named(x, target = c("A", "B", "C"), ignore.case = TRUE) # Pass
#'
expect_named <- function(
  current,
  target,
  ...,
  ignore.order = FALSE,
  ignore.case = FALSE,
  info = NA_character_
) {
  whoami <- sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
  if (...length()) {
    stop(.dots_err(call = whoami))
  }
  if (!(is.character(x = info) && length(x = info) == 1L)) {
    stop(.info_err(call = whoami))
  }
  .tf_err <- function(x) {
    return(simpleError(
      message = sprintf(fmt = "'%s' must be TRUE or FALSE", x),
      call = whoami
    ))
  }
  if (!(isTRUE(x = ignore.order) || isFALSE(x = ignore.order))) {
    stop(.tf_err(x = 'ignore.order'))
  }
  if (!(isTRUE(x = ignore.case) || isFALSE(x = ignore.case))) {
    stop(.tf_err(x = 'ignore.case'))
  }
  call <- sys.call(which = sys.parent(n = 0L))
  nm <- names(x = current)
  if (missing(x = target)) {
    res <- !is.null(x = nm)
    diff <- "Expected 'current' to be named"
  } else if (is.null(x = target)) {
    res <- is.null(x = nm)
    diff <- "Expected 'current' to be unnamed"
  } else {
    if (!is.character(x = target)) {
      stop(simpleError(
        message = "'target' must be a character vector",
        call = whoami
      ))
    }
    if (ignore.order) {
      nm <- sort(x = nm)
      target <- sort(x = target)
    }
    if (ignore.case) {
      nm <- tolower(x = nm)
      target <- tolower(x = target)
    }
    res <- identical(x = nm, y = target)
    diff <- "Expected 'names(current)' to match 'target'"
  }
  return(tinytest(
    result = res,
    call = call,
    diff = diff,
    short = 'attr',
    info = info
  ))
}
