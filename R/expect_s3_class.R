
#' Expect S3 Object
#'
#' Check that \code{current} is an S3 object that
#' \link[base:inherits]{inherits} from \code{class}
#'
#' @template param-current
#' @param class \code{[character]} Expected class of \code{current}; may
#' optionally pass \code{NA} to assert that \code{current} is \strong{not}
#' an object
#' @template param-dots-ignored
#' @template param-info
#' @param exact \code{[logical(1L)]} If \code{TRUE}, check that
#' \code{class(current)} is identical to \code{class}; otherwise,
#' check that \code{current} \link[base:inherits]{inherits} \code{class}
#'
#' @template return-tinytest
#'
#' @export
#'
#' @family inheritance
#'
#' @templateVar fxn expect_s3_class
#' @template link-testthat
#'
#' @examples
#' expect_s3_class(data.frame(), "data.frame") # Pass
#' expect_s3_class(1L, "integer") # Fail
#' expect_s3_class(1L, NA) # Pass
#'
#' @examplesIf requireNamespace("Matrix", quietly = TRUE)
#' expect_s3_class(Matrix::Matrix(), "Matrix") # Fail
#' expect_s3_class(Matrix::Matrix(), NA) # Pass
#'
expect_s3_class <- function(
  current,
  class,
  ...,
  info = NA_character_,
  exact = FALSE
) {
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
  if (!(isTRUE(x = exact) || isFALSE(x = exact))) {
    stop(simpleError(message = "'exact' must be TRUE or FALSE", call = whoami))
  }
  call <- sys.call(which = sys.parent(n = 0L))
  is_s3 <- function(x) {
    return(is.object(x = x) && !isS4(x))
  }
  # Test that `current` is not an S3 object
  if (.is_na(x = class)) {
    return(tinytest(
      result = !is_s3(x = current),
      call = call,
      diff = "Expected object that is not an S3 object, got an S3 object",
      short = 'attr',
      info = info
    ))
  }
  # If `current` is not an S3 object, fail out
  if (!is_s3(x = current)) {
    return(tinytest(
      result = FALSE,
      call = call,
      diff = "Expected an S3 object, got an object that is not an S3 object",
      short = 'attr',
      info = info
    ))
  }
  # Test that `current` is of class `class`
  cc <- base::class(x = current)
  res <- if (exact) {
    identical(x = cc, y = class)
  } else {
    inherits(x = current, what = class)
  }
  clsfmt <- paste0(sQuote(x = class, q = FALSE), collapse = '/')
  ccfmt <- paste0(sQuote(x = cc, q = FALSE), collapse = '/')
  return(tinytest(
    result = res,
    call = call,
    diff = if (exact) {
      sprintf(
        fmt = "Expected S3 object of class %s, got %s",
        clsfmt,
        ccfmt
      )
    } else {
      sprintf(
        fmt = "Expected S3 object that inherits from %s, got %s",
        clsfmt,
        ccfmt
      )
    },
    short = 'attr',
    info = info
  ))
}
