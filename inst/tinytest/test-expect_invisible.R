#!/usr/bin/env Rscript

# Test basic mechanics
expect_inherits(
  current = tt <- expect_invisible(""),
  class = "tinytest",
  info = "'expect_invisible()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected invisible result, got a visible object"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_invisible('')")
  )
}

# Test invisibility with assignment
expect_true(
  current = as.logical(expect_invisible(x <- 10L)),
  info = "assignment is invisible"
)
expect_false(
  current = as.logical(expect_invisible(x)),
  info = "calling an assigned object is visible"
)
expect_equal(
  current = x,
  target = 10L,
  info = "assignment within 'expect_invisible()' works"
)

# Test dots
expect_error(
  current = expect_invisible("", tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_invisible()' throws an error when '...' are not empty"
)
err <- tryCatch(expr = expect_invisible("", tomato = 1L), error = force)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_invisible()",
  info = "'expect_invisible(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_invisible("", info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_invisible()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(expr = expect_invisible("", info = infos[[i]]), error = force)
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_invisible()",
    info = sprintf(
      fmt = "'expect_invisible(info = )' sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}
