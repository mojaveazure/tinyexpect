#!/usr/bin/env Rscript

if (!requireNamespace("withr", quietly = TRUE)) {
  exit_file(msg = "Cannot find 'withr'")
}

op <- list(tinyexpect.skip = FALSE, tinyexpect.envvar.strict = FALSE)
trueish <- list(1L, "true", TRUE, "character")
falseish <- list(0L, "false", FALSE)

withr::with_options(
  new = op,
  code = expect_inherits(
    current = msg <- skip(),
    class = "character",
    info = "'skip()' returns a character"
  )
)
expect_length(
  current = msg,
  length = 1L,
  info = "'skip()' returns a single character value"
)
expect_identical(
  current = msg,
  target = "Skipping",
  info = "'skip()', by default, returns 'Skipping'"
)

withr::with_options(
  new = op,
  code = expect_identical(
    current = skip(message = "tomato"),
    target = "tomato",
    info = "'skip()' returns whatever 'message = ' is"
  )
)

withr::with_options(
  new = op,
  code = expect_error(
    current = skip(TRUE),
    info = "'skip()' errors out if passed a logical value"
  )
)

withr::with_options(
  new = op,
  code = expect_error(
    current = skip(1L),
    info = "'skip()' errors out if passed an integer value"
  )
)

withr::with_options(
  new = op,
  code = expect_error(
    current = skip(1.0),
    info = "'skip()' errors out if passed a numeric value"
  )
)

withr::with_options(
  new = op,
  code = expect_error(
    current = skip(c("msg1", "msg2")),
    info = "'skip()' errors out if passed multile character values value"
  )
)

# `skip_if_not_installed()`
i <- 10L
pkg <- paste(sample(letters, size = i, replace = TRUE), collapse = "")
while (nzchar(tryCatch(find.package(pkg), error = function(...) ""))) {
  pkg <- paste(sample(letters, size = 10L, replace = TRUE), collapse = "")
  i <- i + 1L
}

withr::with_options(
  new = op,
  code = expect_identical(
    current = skip_if_not_installed(pkg),
    target = sprintf(fmt = "%s cannot be loaded", pkg),
    info = "'skip_if_not_installed()' skips if 'pkg' cannot be loaded"
  )
)

v <- read.dcf(
  file = system.file("DESCRIPTION", package = "tinyexpect", mustWork = TRUE),
  fields = "Version"
)[1, "Version"]
nv <- as.numeric(unlist(strsplit(v, split = "\\.")))
nv[1L] <- nv[1L] + 1L
nv <- paste(nv, collapse = ".")

withr::with_options(
  new = op,
  code = expect_identical(
    current = skip_if_not_installed("tinyexpect", minimum_version = nv),
    target = sprintf(
      fmt = "Installed version of tinyexpect is %s, but %s is required",
      v,
      nv
    ),
    info = paste(
      "'skip_if_not_installed()' skips if 'pkg' can be loaded,",
      "but is not at least 'minimum_version'"
    )
  )
)

withr::with_options(
  new = op,
  code = expect_null(
    current = skip_if_not_installed("tinyexpect"),
    info = "'skip_if_not_installed()' does not skip if 'pkg' is installed"
  )
)

withr::with_options(
  new = op,
  code = expect_null(
    current = skip_if_not_installed("tinyexpect", minimum_version = v),
    info = paste(
      "'skip_if_not_installed()' does not skip if 'pkg'",
      "is installed and at least 'minimum_version'"
    )
  )
)

# `skip_on_bioc()`
for (i in seq_along(trueish)) {
  var <- trueish[[i]]
  withr::with_options(
    new = op,
    code = withr::with_envvar(
      new = list(IS_BIOC_BUILD_MACHINE = var),
      code = expect_identical(
        current = skip_on_bioc(),
        target = "On Bioconductor",
        info = sprintf(
          fmt = "'skip_on_bioc()' skips if 'IS_BIOC_BUILD_MACHINE' is trueish ('%s')",
          var
        )
      )
    )
  )
}

for (i in seq_along(falseish)) {
  var <- falseish[[i]]
  withr::with_options(
    new = op,
    code = withr::with_envvar(
      new = list(IS_BIOC_BUILD_MACHINE = var),
      code = expect_null(
        current = skip_on_bioc(),
        info = sprintf(
          fmt = "'skip_on_bioc()' does not skip if 'IS_BIOC_BUILD_MACHINE' is falseish ('%s')",
          var
        )
      )
    )
  )
}

# `skip_on_ci()`
for (i in seq_along(trueish)) {
  var <- trueish[[i]]
  withr::with_options(
    new = op,
    code = withr::with_envvar(
      new = list(CI = var),
      code = expect_identical(
        current = skip_on_ci(),
        target = "On CI",
        info = sprintf(
          fmt = "'skip_on_ci()' skips if 'CI' is trueish ('%s')",
          var
        )
      )
    )
  )
}

for (i in seq_along(falseish)) {
  var <- falseish[[i]]
  withr::with_options(
    new = op,
    code = withr::with_envvar(
      new = list(CI = var),
      code = expect_null(
        current = skip_on_ci(),
        info = sprintf(
          fmt = "'skip_on_ci()' does not skip if 'CI' is falseish ('%s')",
          var
        )
      )
    )
  )
}

# `skip_on_covr()`
for (i in seq_along(trueish)) {
  var <- trueish[[i]]
  withr::with_options(
    new = op,
    code = withr::with_envvar(
      new = list(R_COVR = var),
      code = expect_identical(
        current = skip_on_covr(),
        target = "On covr",
        info = sprintf(
          fmt = "'skip_on_covr()' skips if 'R_COVR' is trueish ('%s')",
          var
        )
      )
    )
  )
}

for (i in seq_along(falseish)) {
  var <- falseish[[i]]
  withr::with_options(
    new = op,
    code = withr::with_envvar(
      new = list(R_COVR = var),
      code = expect_null(
        current = skip_on_covr(),
        info = sprintf(
          fmt = "'skip_on_covr()' does not skip if 'R_COVR' is falseish ('%s')",
          var
        )
      )
    )
  )
}

# `skip_on_cran()`
if (!interactive()) {
  for (i in seq_along(falseish)) {
    var <- falseish[[i]]
    withr::with_options(
      new = op,
      code = withr::with_envvar(
        new = list(NOT_CRAN = var),
        code = expect_identical(
          current = skip_on_cran(),
          target = "On CRAN",
          info = sprintf(
            fmt = "'skip_on_cran()' skips if 'NOT_CRAN' is falseish ('%s')",
            var
          )
        )
      )
    )
  }
  for (i in seq_along(trueish)) {
    var <- trueish[[i]]
    withr::with_options(
      new = op,
      code = withr::with_envvar(
        new = list(NOT_CRAN = var),
        code = expect_null(
          current = skip_on_cran(),
          info = sprintf(
            fmt = "'skip_on_cran()' does not skip if 'NOT_CRAN' is trueish ('%s')",
            var
          )
        )
      )
    )
  }
}

# `skip_on_os()`
systems <- c("windows", "mac", "linux", "solaris")
sysos <- tolower(Sys.info()[["sysname"]])
systems <- switch(
  EXPR = sysos,
  darwin = sub(pattern = "^mac$", replacement = sysos, x = systems),
  sunos = sub(pattern = "^solaris$", replacement = sysos, x = systems),
  systems
)
msgos <- switch(
  EXPR = sysos,
  sunos = ,
  solaris = "Solaris",
  darwin = ,
  mac = "macOS",
  paste0(
    toupper(substr(sysos, start = 1L, stop = 1L)),
    substr(sysos, start = 2L, stop = nchar(sysos))
  )
)
sysarch <- R.version[["arch"]]
i <- 10L
nonarch <- paste(sample(letters, size = i, replace = TRUE), collapse = "")
while (nonarch == sysarch) {
  nonarch <- paste(sample(letters, size = i, replace = TRUE), collapse = "")
  i <- i + 1L
}

withr::with_options(
  new = op,
  code = expect_identical(
    current = skip_on_os(sysos),
    target = sprintf(fmt = "On %s", msgos),
    info = sprintf(fmt = "'skip_on_os()' skips when under 'os' ('%s')", sysos)
  )
)

withr::with_options(
  new = op,
  code = expect_identical(
    current = skip_on_os(os = union(x = sysos, y = systems)),
    target = sprintf(fmt = "On %s", msgos),
    info = "'skip_on_os()' skips when under one of many 'os'"
  )
)

for (os in setdiff(x = systems, y = sysos)) {
  withr::with_options(
    new = op,
    code = expect_null(
      current = skip_on_os(os),
      info = sprintf(
        fmt = "'skip_on_os()' does not skip when not under 'os' ('%s')",
        os
      )
    )
  )
}

withr::with_options(
  new = op,
  code = expect_identical(
    current = skip_on_os(sysos, arch = sysarch),
    target = sprintf(fmt = "On %s %s", msgos, sysarch),
    info = sprintf(
      fmt = "'skip_on_os()' skips when under 'os' ('%s') on 'arch' ('%s')",
      sysos,
      sysarch
    )
  )
)

withr::with_options(
  new = op,
  code = expect_null(
    current = skip_on_os(sysos, arch = nonarch),
    info = sprintf(
      fmt = paste(
        "'skip_on_os()' does not skip when under 'os' ('%s'),",
        "but on different 'arch' (system '%s' vs non-arch '%s')"
      ),
      sysos,
      sysarch,
      nonarch
    )
  )
)

for (os in setdiff(systems, sysos)) {
  withr::with_options(
    new = op,
    code = expect_null(
      current = skip_on_os(os, arch = sysarch),
      info = sprintf(
        fmt = paste(
          "'skip_on_os()' does not skip when not under 'os' ('%s'),",
          "even when on 'arch' ('%s')"
        ),
        os,
        sysarch
      )
    )
  )
}

withr::with_options(
  new = op,
  code = expect_identical(
    current = skip_on_os(TRUE, arch = sysarch),
    target = sprintf(fmt = "On %s %s", msgos, sysarch),
    info = "'skip_on_os(os = TRUE)' will skip on all os"
  )
)

types <- c(
  "complex",
  "expression",
  "integer",
  "list",
  "logical",
  "numeric",
  "raw"
)
for (tt in types) {
  withr::with_options(
    new = op,
    code = expect_error(
      current = skip_on_os(TRUE, arch = vector(mode = tt, length = 1L)),
      pattern = "^'arch' must be a character vector$",
      info = sprintf(
        fmt = "'skip_on_os()' throws an error when 'arch' is %s",
        tt
      )
    )
  )
}
