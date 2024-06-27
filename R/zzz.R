#' @importFrom tinytest tinytest
#'
NULL

#' @docType package
#'
# @section \CRANpkg{testthat} Equivalency:
# The following \pkg{testthat} functions have an equivalent in \pkg{tinyexpect}
# \Sexpr[stage=build,results=rd]{tinyexpect:::.rd_equivalency_table()}
#'
#' @keywords package
#'
"_PACKAGE"

# nocov start
.pkg <- new.env()

.tinytest_expect <- \() sort(x = grep(
  pattern = '^expect_',
  x = getNamespaceExports('tinytest'),
  value = TRUE
))

.testthat_expect <- \() if (requireNamespace('testthat', quietly = TRUE)) {
  sort(x = grep(
    pattern = '^expect_',
    x = getNamespaceExports('testthat'),
    value = TRUE
  ))
} else {
  NULL
}

.rd_equivalency_table <- function() {
  if (!requireNamespace('tools', quietly = TRUE)) {
    return(invisible(x = character()))
  }
  rds <- tools::Rd_db(package = .pkg$pkgname, lib.loc = .pkg$libname)
  if (!length(x = rds)) {
    man <- system.file('man', package = .pkg$pkgname, lib.loc = .pkg$libname)
    rd.files <- list.files(path = man, pattern = '\\.Rd$')
    rds <- sapply(
      X = rd.files,
      FUN = \(x) tools::parse_Rd(file = file.path(man, x)),
      simplify = FALSE
    )
  }
  if (!length(x = rds)) {
    return(invisible(x = character()))
  }
  equiv <- lapply(
    X = rds,
    FUN = function(rd) {
      fxn <- Filter(f = \(x) attr(x = x, which = 'Rd_tag') == '\\usage', x = rd)
      if (!length(x = fxn)) {
        return(NULL)
      }
      fxn <- fxn[[1L]]
      for (i in fxn) {
        i <- tryCatch(expr = str2lang(s = i), error = \(...) NULL)
        if (is.call(x = i)) {
          fxn <- i
          break
        }
      }
      if (!is.call(x = fxn)) {
        return(NULL)
      }
      fxn <- as.character(x = fxn)[1L]
      links <- Filter(
        f = \(x) attr(x = x, which = 'Rd_tag') == '\\seealso',
        x = rd
      )
      if (!length(x = links)) {
        return(NULL)
      }
      links <- Filter(
        f = \(x) attr(x = x, which = 'Rd_tag') == '\\code',
        x = links[[1L]]
      )
      tt <- NULL
      for (i in seq_along(along.with = links)) {
        xr <- links[[i]][[1L]][[1L]]
        if (!grepl(pattern = '^testthat::', x = xr)) {
          next
        }
        tt <- gsub(pattern = '^testthat::', replacement = '', x = xr)
      }
      return(data.frame(expect = fxn, tt = tt))
    }
  )
  equiv <- do.call(what = 'rbind', args = equiv)
  tbl <- vector(mode = "character", length = nrow(x = equiv))
  for (i in seq_along(along.with = tbl)) {
    tbl[i] <- paste0(
      "\\code{\\link[testthat:",
      equiv$tt[i],
      "]{testthat::",
      equiv$tt[i],
      "}()} \\tab \\code{",
      .pkg$pkgname,
      "::\\link{",
      equiv$expect[i],
      "}()}\\cr"
    )
  }
  return(paste("\\tabular{cc}{", paste0(tbl, collapse = '\n'), "}", sep = '\n'))
}

.onLoad <- function(libname, pkgname) {
  # Cache package name and path
  .pkg$pkgname <- pkgname
  .pkg$libname <- libname
  lockEnvironment(env = .pkg, bindings = TRUE)
  # Register `expect_*` functions as tinytest extensions
  expects <- grep(
    pattern = '^expect_',
    x = utils::lsf.str(envir = getNamespace(name = pkgname)),
    value = TRUE
  )
  if (length(x = expects)) {
    # packageStartupMessage(
    #   "Resigering ",
    #   length(x = expects),
    #   " expectation ",
    #   ngettext(n = length(x = expects), msg1 = "function", "functions")
    # )
    tinytest::register_tinytest_extension(pkg = pkgname, functions = expects)
  }
}
# nocov end
