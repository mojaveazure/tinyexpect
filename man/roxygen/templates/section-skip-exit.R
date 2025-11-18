#' @section \dQuote{Skip} vs \dQuote{Exit}:
#' \pkg{tinyexpect} provides both \dQuote{\code{skip_}} and \dQuote{\code{exit_}}
#' versions of \dQuote{stop testing} functions due to the different philosophies
#' of \CRANpkg{tinytest} and \CRANpkg{testthat}; in \CRANpkg{testthat}, tests
#' are encapsulated by \code{\link[testthat]{test_that}()} to create smaller
#' testing units within a single test file. As such, if a series of tests need
#' to be passed over for some reason, it makes sense to \dQuote{skip} a
#' \code{\link[testthat]{test_that}()} block and move on to the next block
#'
#' \CRANpkg{tinytest}, however, treats each test file as a testing unit. Each
#' file in \code{inst/tinytest} is equivalent to a \CRANpkg{testthat}
#' \code{\link[testthat]{test_that}()} block; as such, if a series of tests need
#' to be passed over for some reason, it makes sense to \dQuote{exit} a test
#' file and move on to the next file in \code{inst/tinytest}
#'
#' In order to provide compatibility with users transitioning from
#' \CRANpkg{testthat} to \CRANpkg{tinytest}, and to provide continuity with the
#' \CRANpkg{tinytest} philosophy, \pkg{tinyexpect} provides both \code{skip_}-
#' and \code{exit_}- prefixed \dQuote{stop testing} functions that work
#' identically to one another
