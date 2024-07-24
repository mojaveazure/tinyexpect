
#' Stop Testing on Specific Operating Systems and Architectures
#'
#' @param os Operating system to not test on; choose one or more from:
#' \itemize{
#'  \item \dQuote{\code{windows}}
#'  \item \dQuote{\code{mac}}
#'  \item \dQuote{\code{linux}}
#'  \item \dQuote{\code{sunos}}
#'  \item \dQuote{\code{solaris}}
#' }
#' Pass \code{TRUE} to set \code{os} to all of the above; this is useful
#' for limiting tests by architechture rather than OS/architecture combos
#' @param arch Optional system architectures to not test on; note that this
#' only applies to operating systems present in \code{os}
#'
#' @return If called within a \CRANpkg{tinytest} test running under \code{os}
#' and potentially on an \code{arch} system, triggers an exit condition;
#' otherwise, returns one of
#' \itemize{
#'  \item A string saying that the code is running under \code{os}
#'  \item A string saying that the code is running under \code{os} on an
#'   \code{arch} system
#'  \item \code{NULL} invisibly
#' }
#'
#' @export
#'
#' @template section-skip-exit
#'
#' @family skip
#'
#' @templateVar fxn skip_on_os
#' @template link-testthat
#'
#' @seealso \code{\link[base]{Sys.info}()},
#' \code{\link[base]{R.version}[["arch"]]}
#'
#' @examples
#' (system <- tolower(Sys.info()[["sysname"]]))
#' skip_on_os(system)
#'
#' # Nothing happens if on a different OS
#' (other <- sample(setdiff(c("windows", "mac", "linux", "sunos", "solaris"), system), size = 1L))
#' skip_on_os(other)
#'
#' # System architechtures can be used to fine-tune skips
#' (sysarch <- R.version$arch)
#' skip_on_os(system, arch = sysarch)
#'
skip_on_os <- function(os, arch = NULL) {
  choices <- c('windows', 'mac', 'linux', 'sunos', 'solaris')
  if (isTRUE(x = os)) {
    os <- choices
  }
  os <- match.arg(arg = tolower(x = os), choices = choices, several.ok = TRUE)
  stopifnot(
    "'arch' must be a character vector" = is.null(x = arch) ||
      is.character(x = arch)
  )
  system <- tolower(x = Sys.info()[['sysname']])
  msg <- if (system %in% os) {
    switch(
      EXPR = system,
      sunos = ,
      solaris = 'On Solaris',
      paste0(
        "On ",
        toupper(x = substr(x = system, start = 1L, stop = 1L)),
        substr(x = system, start = 2L, stop = nchar(x = system))
      )
    )
  } else {
    NULL
  }
  sysarch <- R.version$arch
  if (is.character(x = msg) && is.character(x = arch)) {
    msg <- if (sysarch %in% arch) {
      paste(msg, sysarch)
    } else {
      NULL
    }
  }
  if (is.null(x = msg)) {
    return(invisible(x = NULL))
  }
  return(skip(message = msg))
}

#' @rdname skip_on_os
#' @export
#'
exit_on_os <- skip_on_os
