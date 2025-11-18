#' @importFrom tinytest tinytest
#'
NULL

#' @docType package
#'
#' @section \CRANpkg{testthat} Equivalents:
#' \pkg{tinyexpect} aims to port as many \CRANpkg{testthat} expectation
#' predicates to the \CRANpkg{tinytest} framework as makes sense.
#' \Sexpr[stage=render,results=rd]{
#' eq <- Filter(
#'   f = function(x) startsWith(x, "expect_"),
#'   x = tinyexpect:::.equivs$testthat$equivalents
#' )
#' if (length(eq)) {
#'   msg <- paste(
#'     "The following",
#'     ngettext(length(eq), "predicate has", "predicates have"),
#'     "been ported:",
#'     sprintf(
#'       "\\\itemize{\n\%s\n}",
#'       paste0(
#'         sprintf("\\\item \\\code{\\\\link{\%s}()}", eq),
#'         collapse = "\n"
#'       )
#'     )
#'   )
#'   missing <- tinyexpect:::.equivs$testthat$missing
#'   if (length(missing)) {
#'     mm <- sprintf(
#'       paste(
#'         "\\\subsection{Missing Predicates}{The following \\\pkg{\%s}",
#'         "\%s not been ported:\n\%s}"
#'       ),
#'       "testthat",
#'       ngettext(length(missing), "predicate has", "predicates have"),
#'       sprintf(
#'         "\\\itemize{\n\%s\n}",
#'         paste0(
#'           sprintf("\\\item \\\code{\\\\link[testthat]{\%s}()}", missing),
#'           collapse = "\n"
#'         )
#'       )
#'     )
#'     msg <- paste(msg, mm)
#'   }
#'   utilities <- Filter(
#'     f = function(x) !startsWith(x, "expect_"),
#'     x = tinyexpect:::.equivs$testthat$equivalents
#'   )
#'   if (length(utilities)) {
#'     mu <- sprintf(
#'       paste(
#'         "\\\subsection{Utility Functions}{The following \\\pkg{\%s}",
#'         "utility \%s been ported:\n\%s}"
#'       ),
#'       "testthat",
#'       ngettext(length(utilities), "function has", "functions have"),
#'       sprintf(
#'         "\\\itemize{\n\%s\n}",
#'         paste0(
#'           sprintf("\\\item \\\code{\\\\link{\%s}()}", utilities),
#'           collapse = "\n"
#'         )
#'       )
#'     )
#'     msg <- paste(msg, mu)
#'   }
#'   msg
#' } else {
#'   paste(
#'     "For a complete list of equivalent predicates,",
#'     "please install and load \\\pkg{testthat}"
#'   )
#' }
#' }
#'
#' @keywords package internal
#'
"_PACKAGE"

.pkg <- new.env()
.equivs <- new.env()

#' Dots Error Condition
#'
#' Generate an error condition for incorrect dots (\dots) usage
#'
#' @param ... Arguments passed on to \code{\link[base]{simpleError}()}
#'
#' @return An error condition with the message
#' \dQuote{\code{'...' must be empty}}
#'
#' @keywords internal utilities error
#'
#' @export
#'
#' @family utils
#'
#' @examples
#' .dots_err()
#'
.dots_err <- function(...) {
  return(simpleError(message = "'...' must be empty", ...))
}

#' Info Error Message
#'
#' Generate an error condition for incorrect \sQuote{\code{info}} usage
#'
#' @param ... Arguments passed on to \code{\link[base]{simpleError}()}
#'
#' @return An error condition with the message
#' \dQuote{\code{'info' must be a single character value}}
#'
#' @keywords internal utilities error
#'
#' @export
#'
#' @family utils
#'
#' @examples
#' .info_err()
#'
.info_err <- function(...) {
  return(simpleError(
    message = "'info' must be a single character value",
    ...
  ))
}

#' Is an Object \code{NA}
#'
#' Test that an object is \code{NA}; an object is \code{NA} if
#' \itemize{
#'  \item it has a length of \eqn{1}
#'  \item \code{is.na(x)} is \code{TRUE}
#' }
#'
#' @param x An \R object
#'
#' @return \code{TRUE} if \code{x} is \code{NA}, otherwise \code{FALSE}
#'
#' @keywords internal utilities NA
#'
#' @export
#'
#' @family utils
#'
#' @examples
#' .is_na(NA) # TRUE
#' .is_na(c(NA, NA)) # FALSE
#'
.is_na <- function(x) {
  return(length(x = x) == 1L && is.na(x = x))
}

#' Is an Object a String
#'
#' Test that an object is a string; an object is a string if
#' \itemize{
#'  \item it is a \link[base]{character}
#'  \item it has a length of \eqn{1}
#'  \item is is not \code{NA}
#'  \item it is not \link[base:nzchar]{empty}
#' }
#'
#' @param x An \R object
#'
#' @return \code{TRUE} if \code{x} is a string, otherwise \code{FALSE}
#'
#' @keywords internal utilities character
#'
#' @export
#'
#' @family utils
#'
#' @seealso \code{\link[base]{nzchar}()}
#'
#' @examples
#' .is_string("a") # TRUE
#' .is_string("hello") # TRUE
#' .is_string("hello world") # FALSE
#'
#' .is_string("") # FALSE
#' .is_string(character(length = 0L)) # FALSE
#' .is_string(c("hello", "world")) # FALSE
#'
.is_string <- function(x) {
  return(
    is.character(x = x) &&
      length(x = x) == 1L &&
      !is.na(x = x) &&
      nzchar(x = x)
  )
}

#' Is an Environment Variable Trueish
#'
#' @param var An environment variable
#'
#' @return \code{TRUE} if \code{var} is trueish, otherwise \code{FALSE}
#'
#' @keywords internal utilities
#'
#' @export
#'
#' @family utils
#'
#' @examplesIf requireNamespace("withr", quietly = TRUE)
#' withr::with_envvar(c(R_TINYEXPECT_ENVVAR = "true"), {
#'   .is_var("R_TINYEXPECT_ENVVAR") # TRUE
#' })
#'
#' withr::with_envvar(c(R_TINYEXPECT_ENVVAR = "false"), {
#'   .is_var("R_TINYEXPECT_ENVVAR") # FALSE
#' })
#'
.is_var <- function(var) {
  env <- Sys.getenv(x = var, unset = "false")
  if (grepl(pattern = "^[[:digit:]]+$", x = env)) {
    env <- as.numeric(x = env)
  }
  env <- as.logical(x = env)
  if (isTRUE(x = getOption(x = "tinyexpect.envvar.strict", default = FALSE))) {
    return(isTRUE(x = env))
  }
  return(!isFALSE(x = env))
}

#' \CRANpkg{rlang}-Style Vector Type Predicate
#'
#' An \R-based version of \code{\link[rlang:is_vector]{rlang::is_vector}()};
#' compared to \code{\link[base]{is.vector}()}, \code{.is_vector()} considers
#' the following to be vectors:
#' \itemize{
#'  \item vectors
#'  \item \link[base:factor]{factors}, including
#'   \link[base:ordered]{ordered factors}
#'  \item \link[base:list]{lists}, including \link[base:data.frame]{data frames}
#'   and other \link[base:structure]{S3 objects}
#'  \item \link[base:array]{arrays}, including \link[base:matrix]{matrices}
#' }
#'
#' @param x An \R object
#'
#' @return \code{TRUE} if \code{x} is a vector under \CRANpkg{rlang}-rules,
#' otherwise \code{FALSE}
#'
#' @keywords internal utilities classes
#'
#' @export
#'
#' @family utils
#'
#' @seealso \code{\link[rlang:is_vector]{rlang::is_vector}()},
#' \code{\link[base]{is.vector}()}
#'
#' @noRd
#'
#' @examples
#' # Factors
#' is.vector(factor()) # FALSE
#' rlang::is_vector(factor()) # TRUE
#' .is_vector(factor()) # TRUE
#'
#' # Lists
#' is.vector(list()) # TRUE
#' rlang::is_vector(list()) # TRUE
#' .is_vector(list()) # TRUE
#'
#' # Data frames
#' is.vector(data.frame()) # FALSE
#' rlang::is_vector(data.frame()) # TRUE
#' .is_vector(data.frame()) # TRUE
#'
#' # Matrices and Arrays
#' is.vector(matrix()) # FALSE
#' rlang::is_vector(matrix()) # TRUE
#' .is_vector(matrix()) # TRUE
#'
#' is.vector(array()) # FALSE
#' rlang::is_vector(array()) # TRUE
#' .is_vector(array()) # TRUE
#'
.is_vector <- function(x) {
  return(
    is.vector(x = x) ||
      is.factor(x = x) ||
      is.list(x = x) ||
      is.array(x = x)
  )
}

# nocov start
.onLoad <- function(libname, pkgname) {
  # Cache package name and path
  .pkg$pkgname <- pkgname
  .pkg$libname <- libname
  .pkg$equivalencies <- list()
  lockEnvironment(env = .pkg, bindings = TRUE)
  # Register `expect_*` functions as tinytest extensions
  expects <- Filter(
    f = function(x) startsWith(x = x, prefix = "expect_"),
    x = parseNamespaceFile(package = pkgname, package.lib = libname)$exports
  )
  if (length(x = expects)) {
    tinytest::register_tinytest_extension(pkg = pkgname, functions = expects)
  }
  #' Identify Equivalent Functions in Another Package
  #'
  #' Generate a list of equivalent and missing functions between this package
  #' and another one. Any functions in the other package marked as deprecated
  #' with either \code{testthat:::edition_deprecate()} or a
  #' \code{\link[base]{warning}()} starting with \dQuote{\code{Deprecated:}}
  #' will be ignored
  #'
  #' @note This function expects that the current package name is stored in
  #' an \link[base]{environment} \dQuote{\code{.pkg}} under the value
  #' \dQuote{\code{pkgname}}; it also expects an unlocked environment
  #' \dQuote{\code{.equivs}} to be available in order to store the return
  #' value in
  #'
  #' @param pkgname Name of package to search for equivalents in
  #' @param pkgpath Optional path to `pkgname` on-disk
  #'
  #' @return A list with equivalent function names in the
  #' \dQuote{\code{equivalents}} key, and missing function
  #' names in the \dQuote{\code{missing}} key
  #'
  #' @keywords internal
  #'
  #' @noRd
  #'
  #' @examples
  #' # If `pkgname` is loaded, generate equivalencies
  #' if (isNamespaceLoaded("testthat")) {
  #'   .equivalencies("testthat")
  #' } else {
  #'   # Otherwise, add a load hook for `pkgname`
  #'   setHook(packageEvent("testthat"), value = .equivalencies)
  #' }
  #'
  .equivalencies <- function(pkgname, pkgpath = NULL) {
    exports <- getNamespaceExports(ns = getNamespace(name = .pkg$pkgname))
    if (!length(x = exports)) {
      exports <- parseNamespaceFile(
        package = .pkg$pkgname,
        package.lib = .pkg$libname
      )$exports
    }
    ns <- getNamespace(name = pkgname)
    nse <- getNamespaceExports(ns = ns)
    equivalents <- Filter(
      f = function(f, pkg = pkgname, lib_loc = pkgpath) {
        man <- utils::help.search(
          pattern = sprintf(fmt = "^%s$", f),
          package = pkg,
          lib.loc = if (is.null(x = lib_loc)) {
            .libPaths()
          } else {
            dirname(path = pkgpath)
          },
          types = "help"
        )
        return(as.logical(x = nrow(x = man$matches)))
      },
      x = intersect(x = exports, y = nse)
    )
    predicates <- nse[startsWith(x = nse, prefix = "expect_")]
    deprecated <- Filter(
      f = function(f, envir = ns) {
        fxn <- get(x = f, envir = envir, mode = "function")
        body <- as.character(x = body(fun = fxn))
        for (prefix in c("edition_deprecate", 'warning("Deprecated:')) {
          if (any(startsWith(x = body, prefix = prefix))) {
            return(TRUE)
          }
        }
        return(FALSE)
      },
      x = predicates
    )
    missing <- setdiff(
      x = setdiff(x = predicates, y = deprecated),
      y = union(
        x = equivalents,
        y = getNamespaceExports(ns = getNamespace(name = "tinytest"))
      )
    )
    .equivs[[pkgname]] <- list(
      equivalents = equivalents,
      missing = missing
    )
    lockBinding(sym = pkgname, env = .equivs)
    return(.equivs[[pkgname]])
  }
  # Add testthat equivalencies
  if (isNamespaceLoaded(name = "testthat")) {
    .equivalencies(pkgname = "testthat")
  } else {
    setHook(
      hookName = packageEvent(pkgname = "testthat"),
      value = .equivalencies
    )
  }
  return(invisible(x = NULL))
}

.onAttach <- function(libname, pkgname) {
  extensions <- getOption(x = "tt.extensions")[[pkgname]]
  if (n <- length(x = extensions)) {
    if (any(int <- !extensions %in% getNamespaceExports(ns = pkgname))) {
      msg <- sprintf(
        fmt = "Found %i internal expectation %s registered",
        sum(int),
        ngettext(n = sum(int), msg1 = "predicate", msg2 = "predicates")
      )
      warning(paste(strwrap(x = msg), collapse = "\n"), call. = FALSE)
    }
    packageStartupMessage(sprintf(
      fmt = "Registering %i expectation %s",
      n,
      ngettext(n = n, msg1 = "predicate", msg2 = "predicates")
    ))
  }
  return(invisible(x = NULL))
}
# nocov end
