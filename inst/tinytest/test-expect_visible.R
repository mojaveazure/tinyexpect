#!/usr/bin/env Rscript

# Test basic mechanics
expect_inherits(
  tt <- expect_visible(""),
  class = "tinytest",
  info = "'expect_visible()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected visible result, got an invisible object"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_visible('')")
  )
}

# Test visibility with assignment
expect_false(
  current = as.logical(expect_visible(x <- 2L + 2L)),
  info = "assignment is invisible"
)
expect_true(
  current = as.logical(expect_visible(x)),
  info = "calling an assigned object is visible"
)
expect_equal(
  current = x,
  target = 4L,
  info = "assignment within 'expect_visible()' works"
)

# Test dots
expect_error(
  current = expect_visible("", tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_visible()' throws an error when '...' are not empty"
)
err <- tryCatch(expr = expect_visible("", tomato = 1L), error = force)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_visible()",
  info = "'expect_visible(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_visible("", info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_visible()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(expr = expect_visible("", info = infos[[i]]), error = force)
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_visible()",
    info = sprintf(
      fmt = "'expect_visible(info = )' sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}
