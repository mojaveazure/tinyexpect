
#' @inherit expect_compare title description params return
#'
#' @name expect_comparisons
#' @rdname expect_comparisons
#'
#' @seealso \CRANpkg{testthat} equivalents:
#' \code{\link[testthat:comparison-expectations]{expect_lt}()},
#' \code{\link[testthat:comparison-expectations]{expect_lte}()},
#' \code{\link[testthat:comparison-expectations]{expect_gt}()}, and
#' \code{\link[testthat:comparison-expectations]{expect_gte}()}
#'
#'
NULL

#' @rdname expect_comparisons
#' @export
#'
#' @examples
#' # Expect strictly less than
#' expect_lt(1L, 3L) # Pass
#' expect_lt(3L, 1L) # Fail
#' expect_lt(1L, 1L) # Fail
#'
expect_lt <- function(current, target, ..., info = NA_character_) {
  call <- sys.call(which = sys.parent(n = 0L))
  return(expect_compare(
    current = current,
    target = target,
    ...,
    operator = '<',
    info = info,
    call = call
  ))
}

#' @rdname expect_comparisons
#' @export
#'
#' @examples
#' # Expect less than or equal to
#' expect_lte(1L, 3L) # Pass
#' expect_lte(3L, 1L) # Fail
#' expect_lte(1L, 1L) # Pass
#'
expect_lte <- function(current, target, ..., info = NA_character_) {
  call <- sys.call()
  return(expect_compare(
    current = current,
    target = target,
    ...,
    operator = '<=',
    info = info,
    call = call
  ))
}

#' @rdname expect_comparisons
#' @export
#'
#' @examples
#' # Expect strictly greater than
#' expect_gt(1L, 3L) # Fail
#' expect_gt(3L, 1L) # Pass
#' expect_gt(1L, 1L) # Fail
#'
expect_gt <- function(current, target, ..., info = NA_character_) {
  call <- sys.call(which = sys.parent(n = 0L))
  return(expect_compare(
    current = current,
    target = target,
    ...,
    operator = '>',
    info = info,
    call = call
  ))
}

#' @rdname expect_comparisons
#' @export
#'
#' @examples
#' # Expect greater than or equal to
#' expect_gte(1L, 3L) # Fail
#' expect_gte(3L, 1L) # Pass
#' expect_gte(1L, 1L) # Pass
#'
expect_gte <- function(current, target, ..., info = NA_character_) {
  call <- sys.call(which = sys.parent(n = 0L))
  return(expect_compare(
    current = current,
    target = target,
    ...,
    operator = '>=',
    info = info,
    call = call
  ))
}
