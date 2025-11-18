
#' Expect a Condition to be Thrown
#'
#' Expectation predicate for testing that a given function call throws a
#' \link[base]{condition}
#'
#' @template param-current
#' @param pattern \code{[character(1L)]} A regular expression to match the
#' condition message; any condition thrown that does not match \code{pattern}
#' will trigger a failure. Must be a single character string
#' @param class \code{[character]} Optional vector of classes from which the
#' condition should inherit; any conditions thrown that do no inherit from
#' \code{class} will trigger a failure
#' @template param-dots-ignored
#' @template param-info
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family condition
#'
#' @templateVar fxn expect_condition
#' @template link-testthat
#'
#' @examples
#' expect_condition(message("test message")) # Pass
#' expect_condition(warning("test warning")) # Pass
#' expect_condition(stop("test error")) # Pass
#'
#' expect_condition("tomato") # Fail
#' expect_condition(message("test message"), class = "specialMessage") # Fail
#' expect_condition(message("test message"), pattern = "tomato") # Fail
#'
expect_condition <- function(
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
  # if (!is.environment(x = envir)) {
  #   stop(simpleError(
  #     message = "'envir' must be an environment",
  #     call = whoami
  #   ))
  # }
  # https://github.com/satijalab/seurat/issues/2799
  current <- rlang::enquo0(arg = current)
  # current <- substitute(expr = current, env = envir)
  thrown <- NULL
  call <- sys.call(which = sys.parent(n = 0L))
  tryCatch(
    expr = rlang::eval_bare(
      expr = rlang::quo_get_expr(quo = current),
      env = rlang::quo_get_env(quo = current)
    ),
    condition = function(cnd) thrown <<- cnd
  )
  # tryCatch(
  #   expr = eval(expr = current, envir = envir),
  #   condition = function(cnd) thrown <<- cnd
  # )
  if (is.null(x = thrown)) {
    return(tinytest(
      result = FALSE,
      call = call,
      diff = "Expected a condition to be thrown, but none were",
      short = 'xcpt',
      info = info
    ))
  }
  matches <- if (!is.null(x = pattern)) {
    grepl(pattern = pattern, x = conditionMessage(c = thrown))
  } else {
    TRUE
  }
  cls <- if (!is.null(x = class)) {
    inherits(x = thrown, what = class)
  } else {
    TRUE
  }
  diff <- 'Expected a condition'
  if (!is.null(x = class)) {
    diff <- sprintf(
      fmt = "%s inheriting from %s",
      diff,
      paste0(sQuote(x = class, q = FALSE), collapse = ", ")
    )
  }
  if (!is.null(x = pattern)) {
    diff <- sprintf(fmt = "%s matching the pattern '%s'", diff, pattern)
  }
  return(tinytest(
    result = matches && cls,
    call = call,
    diff = sprintf(fmt = "%s to be thrown", diff),
    short = 'xcpt',
    info = info
  ))
}
