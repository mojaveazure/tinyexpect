
#' Expect Map Equality
#'
#' Expectation predicate for testing that the named \link[base]{sets} of
#' two vectors are equal
#'
#' @inheritParams expect_contains
#' @param tolerance \code{[numeric]} Test equality to machine rounding; passed
#' to \code{\link[base]{all.equal}()}
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family set
#'
#' @templateVar fxn expect_mapequal
#' @template link-testthat
#'
#' @examples
#' (x <- `names<-`(letters, letters))
#' expect_mapequal(x, target = rev(x)) # Pass
#' expect_mapequal(letters, target = c(b = "b")) # Fail
#' expect_mapequal(c(b = "b"), target = x) # Fail
#'
expect_mapequal <- function(
  current,
  target,
  ...,
  info = NA_character_,
  tolerance = sqrt(x = .Machine$double.eps)
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
  if (!length(x = current) && !length(x = target)) {
    warning(warningCondition(
      message = "Both 'current' and 'target' are empty",
      call = whoami
    ))
    return(tinytest(result = TRUE, call = call, short = 'xcpt', info = info))
  }
  # Check names
  .dup_error <- function(x) {
    return(simpleError(
      message = sprintf(fmt = "Names of '%s' must be unique and non-empty", x),
      call = whoami
    ))
  }
  nmc <- names(x = current)
  if (anyDuplicated(x = nmc) || !all(nzchar(x = nmc))) {
    stop(.dup_error(x = 'current'))
  }
  nmt <- names(x = target)
  if (anyDuplicated(x = nmt) || !all(nzchar(x = nmt))) {
    stop(.dup_error(x = 'target'))
  }
  if (is.null(x = nmc) || is.null(x = nmt)) {
    return(tinytest(
      result = FALSE,
      call = call,
      diff = "Both 'current' and 'target' must be named",
      short = "xcpt",
      info = info
    ))
  }
  # Do the test
  if (!setequal(x = nmc, y = nmt)) {
    res <- FALSE
    diff <- sprintf(
      fmt = "Expected names to be identical between 'current' and 'target', %s",
      ifelse(
        test = length(x = setdiff(x = nmt, y = nmc)),
        yes = "observed names in 'target' not present in 'current'",
        no = "observed names in 'current' not present in 'target'"
      )
    )
  } else {
    res <- isTRUE(x = all.equal(
      target = target,
      current = current[nmt],
      tolerance = tolerance
    ))
    diff <- "Expected 'current' to have identical name/value pairs as 'target'"
  }
  return(tinytest(
    result = res,
    call = call,
    diff = diff,
    short = "xcpt",
    info = info
  ))
}
