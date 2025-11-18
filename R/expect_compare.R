
#' Expect a Comparison to Hold
#'
#' Expectation predicate for testing based
#' on \link[base:Comparison]{comparisons}
#'
#' @template param-current
#' @param target \code{[R object or expression]} Expected outcome
#' @template param-dots-ignored
#' @param operator \code{[character(1L)]} Name of comparison function for
#' expectation; choose from:
#' \itemize{
#'  \item \dQuote{\code{<}} for strictly less than
#'  \item \dQuote{\code{<=}} for less than or equal to
#'  \item \dQuote{\code{>}} strictly greater than
#'  \item \dQuote{\code{>=}} for greater than or equal to
#'  \item \dQuote{\code{==}} for equal to
#'  \item \dQuote{\code{!=}} for not equal to
#' }
#' @template param-info
#' @param call \code{[call]} An optional \code{\link[base]{call}} object for
#' for expectation predicates that wrap \code{expect_compare()}
#'
#' @template return-tinytest
#'
#' @keywords internal
#'
#' @export
#'
#' @seealso \link[base:Comparison]{Comparison operators} in \R
#'
#' @seealso Convenience functions: \code{\link{expect_lt}()},
#' \code{\link{expect_lte}()}, \code{\link{expect_gt}()}, and
#' \code{\link{expect_gte}()}
#'
#' @examples
#' # Expect strictly less than
#' expect_compare(1L, 3L, operator = "<") # Pass
#' expect_compare(3L, 1L, operator = "<") # Fail
#' expect_compare(1L, 1L, operator = "<") # Fail
#'
#' # Expect less than or equal to
#' expect_compare(1L, 3L, operator = "<=") # Pass
#' expect_compare(3L, 1L, operator = "<=") # Fail
#' expect_compare(1L, 1L, operator = "<=") # Pass
#'
#' # Expect strictly greater than
#' expect_compare(1L, 3L, operator = ">") # Fail
#' expect_compare(3L, 1L, operator = ">") # Pass
#' expect_compare(1L, 1L, operator = ">") # Fail
#'
#' # Expect greater than or equal to
#' expect_compare(1L, 3L, operator = ">=") # Fail
#' expect_compare(3L, 1L, operator = ">=") # Pass
#' expect_compare(1L, 1L, operator = ">=") # Pass
#'
#' # Expect equal to
#' expect_compare(1L, 3L, operator = "==") # Fail
#' expect_compare(3L, 1L, operator = "==") # Fail
#' expect_compare(1L, 1L, operator = "==") # Pass
#'
#' # Expect not equal to
#' expect_compare(1L, 3L, operator = "!=") # Pass
#' expect_compare(3L, 1L, operator = "!=") # Pass
#' expect_compare(1L, 1L, operator = "!=") # Fail
#'
expect_compare <- function(
  current,
  target,
  ...,
  operator,
  info = NA_character_,
  call = NULL
) {
  whoami <- sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
  if (...length()) {
    stop(.dots_err(call = whoami))
  }
  if (!(is.character(x = info) && length(x = info) == 1L)) {
    stop(.info_err(call = whoami))
  }
  if (is.null(x = call)) {
    call <- sys.call(which = sys.parent(n = 0L))
  }
  if (!is.call(x = call)) {
    stop(simpleError(message = "'call' must be a call", call = whoami))
  }
  msg <- switch(
    EXPR = operator,
    '<' = 'strictly less than',
    '<=' = 'less than or equal to',
    '>' = 'strictly greater than',
    '>=' = 'greater than or equal to',
    '==' = 'equal to',
    '!=' = 'not equal to',
    stop(simpleError(
      message = sprintf(fmt = "Unknown operator '%s'", operator),
      call = whoami
    ))
  )
  op <- match.fun(FUN = operator, descend = FALSE)
  cmp <- op(current, target)
  if (!(is.logical(x = cmp) && length(x = cmp) == 1L)) {
    stop(simpleError(
      message = "Result of comparison must be a single logical value",
      call = whoami
    ))
  }
  return(tinytest(
    result = isTRUE(x = cmp),
    call = call,
    diff = sprintf(fmt = "Expected %s to be %s %s", current, msg, target),
    short = 'xcpt',
    info = info
  ))
}
