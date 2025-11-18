
#' Expect S4 Object
#'
#' Check that \code{current} is an S4 object that is of class \code{class}
#'
#' @inheritParams expect_s3_class
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family inheritance
#'
#' @templateVar fxn expect_s4_class
#' @template link-testthat
#'
#' @examplesIf requireNamespace("Matrix", quietly = TRUE)
#' expect_s4_class(Matrix::Matrix(), "Matrix") # Pass
#'
#' @examples
#' expect_s4_class(data.frame(), "data.frame") # Fail
#' expect_s4_class(data.frame(), NA) # Pass
#'
expect_s4_class <- function(current, class, ..., info = NA_character_) {
  whoami <- sprintf(fmt = "%s()", as.character(x = sys.call())[1L])
  if (...length()) {
    stop(.dots_err(call = whoami))
  }
  if (!(is.character(x = info) && length(x = info) == 1L)) {
    stop(.info_err(call = whoami))
  }
  cls <- .is_na(x = class) ||
    (is.character(x = class) && all(nzchar(x = class)))
  if (!cls) {
    stop(simpleError(
      message = "'class' must be a non-empty character vector",
      call = whoami
    ))
  }
  call <- sys.call(which = sys.parent(n = 0L))
  # Test that `current` is not an S4 object
  if (.is_na(x = class)) {
    res <- !isS4(current)
    return(tinytest(
      result = res,
      call = call,
      diff = ifelse(
        test = res,
        yes = NA_character_,
        no = "Expected non-S4 object, got an S4 object"
      ),
      short = 'attr',
      info = info
    ))
  }
  # If `current` is not an S4 object, fail out
  if (!isS4(current)) {
    return(tinytest(
      result = FALSE,
      call = call,
      diff = "Expected an S4 object, got a non-S4 object",
      short = 'attr',
      info = info
    ))
  }
  # Test that `current` is of class `class`
  return(tinytest(
    result = any(vapply(
      X = class,
      FUN = isa,
      FUN.VALUE = logical(length = 1L),
      x = current
    )),
    call = call,
    diff = sprintf(
      fmt = ifelse(
        test = base::class(x = current) %in% class,
        yes = "Expected S4 object of class %s, got '%s'",
        no = "Expected S4 object that is of or inherits from %s, got '%s'"
      ),
      paste0(sQuote(x = class, q = FALSE), collapse = '/'),
      base::class(x = current)
    ),
    short = 'attr',
    info = info
  ))
}
