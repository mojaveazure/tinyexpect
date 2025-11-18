
#' Expect No Conditions to be Thrown
#'
#' Expectation predicate for testing that a given function does not raise
#' a \link[base]{condition}
#'
#' @template param-current
#' @param pattern \code{[character(1L)]} A regular expression to match the
#' condition message; any conditions thrown with a message matching
#' \code{pattern} will trigger a failure while all other conditions
#' thrown will trigger a success. Must be a single character string
#' @param class \code{[character]} Optional vector of condition classes that
#' should \strong{not} be thrown; conditions thrown that inherit from
#' \code{class} will trigger a failure while all other conditions thrown
#' will trigger a success. If \code{NULL}, any condition thrown will
#' trigger a failure
#' @template param-dots-ignored
#' @template param-info
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family condition
#'
#' @templateVar fxn expect_no_condition
#' @template link-testthat
#'
#' @examples
#' expect_no_condition("tomato") # Pass
#'
#' expect_no_condition(message("test")) # Fail
#'
#' expect_no_condition(message("test"), pattern = "test") # Fail
#' expect_no_condition(message("test"), pattern = "tomato") # Pass
#'
#' expect_no_condition(message("test"), class = "message") # Fail
#' expect_no_condition(message("test"), class = "error") # Pass
#'
#' expect_no_condition(message("test"), pattern = "test", class = "message") # Fail
#' expect_no_condition(message("test"), pattern = "tomato", class = "message") # Pass
#' expect_no_condition(message("test"), pattern = "test", class = "error") # Pass
#' expect_no_condition(message("test"), pattern = "tomato", class = "error") # Pass
#'
expect_no_condition <- function(
  current,
  pattern = NULL,
  class = NULL,
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
  if (!(is.null(x = pattern) || .is_string(x = pattern))) {
    stop(simpleError(
      message = "'pattern' must be a single, non-empty string",
      call = whoami
    ))
  }
  if (!(is.null(x = class) || is.character(x = class))) {
    stop(simpleError(
      message = "'class' must be a character vector",
      call = whoami
    ))
  }
  current <- rlang::enquo0(arg = current)
  thrown <- NULL
  call <- sys.call(which = sys.parent(n = 0L))
  tryCatch(
    expr = rlang::eval_bare(
      expr = rlang::quo_get_expr(quo = current),
      env = rlang::quo_get_env(quo = current)
    ),
    condition = function(cnd) thrown <<- cnd
  )
  if (is.null(x = thrown)) {
    return(tinytest(
      result = TRUE,
      call = call,
      diff = "Expected no condition to be thrown, caught no conditions",
      short = 'xcpt',
      info = info
    ))
  }
  diff <- 'Expected no conditions'
  if (is.null(x = class) && is.null(x = pattern)) {
    return(tinytest(
      result = FALSE,
      call = call,
      diff = paste(diff, 'to be thrown'),
      short = 'xcpt',
      info = info
    ))
  }
  if (!is.null(x = class)) {
    diff <- paste(
      diff,
      'inheriting from',
      paste(sQuote(x = class), collapse = ', ')
    )
  }
  if (!is.null(x = pattern)) {
    diff <- paste(diff, 'matching the pattern', sQuote(x = pattern))
  }
  matches <- if (!is.null(x = pattern)) {
    grepl(pattern = pattern, x = conditionMessage(c = thrown))
  } else {
    !is.null(x = class)
  }
  cls <- if (!is.null(x = class)) {
    inherits(x = thrown, what = class)
  } else {
    !is.null(x = pattern)
  }
  return(tinytest(
    result = !matches || !cls,
    call = call,
    diff = paste(diff, 'to be thrown'),
    short = 'xcpt',
    info = info
  ))
}
