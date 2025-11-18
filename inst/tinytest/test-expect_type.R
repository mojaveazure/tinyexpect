#!/usr/bin/env Rscript

# Basic mechanics
expect_inherits(
  current = tt <- expect_type("", "character"),
  class = "tinytest",
  info = "'expect_type()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected object of type 'character', got 'character'"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_type('', 'character')")
  )
}

# Test types
types <- c(
  "logical",
  "integer",
  "double",
  "complex",
  "character",
  "raw",
  "list",
  "NULL",
  "closure",
  "environment",
  "S4"
)
info <- "'expect_type() returns %s when obj is of class '%s' and type is '%s'"
for (t in types) {
  obj <- switch(
    EXPR = t,
    double = numeric(),
    'NULL' = NULL,
    closure = function() {},
    S4 = methods::getClass(Class = "classRepresentation"),
    do.call(what = t, args = list())
  )
  expect_true(
    current = as.logical(x = expect_type(current = obj, type = t)),
    info = sprintf(info, "TRUE", class(obj)[1L], t)
  )
  for (xt in setdiff(x = types, y = t)) {
    expect_false(
      current = as.logical(x = expect_type(current = obj, type = xt)),
      info = sprintf(fmt = info, "FALSE", class(x = obj)[1L], xt)
    )
  }
}

# Test dots
expect_error(
  current = expect_type("", "character", tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_type()' throws an error when '...' are not empty"
)
err <- tryCatch(expr = expect_type("", "character", tomato = 1L), error = force)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_type()",
  info = "'expect_type(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_type("", "character", info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_type()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(
    expr = expect_type("", "character", info = infos[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_type()",
    info = sprintf(
      fmt = "'expect_type(info = )' sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}

# Test `type`
types <- list(
  "a logical" = TRUE,
  "a numeric" = 1.1,
  "a vector" = c("hello", "world"),
  "empty" = "",
  "an NA" = NA_character_
)
for (i in seq_along(along.with = types)) {
  expect_error(
    current = expect_type("", types[[i]]),
    pattern = "^'type' must be a single non-empty string",
    info = sprintf(
      fmt = "'expect_type()' throws an error when 'type' is a %s",
      names(x = types)[i]
    )
  )
  err <- tryCatch(expr = expect_type("", types[[i]]), error = force)
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_type()",
    info = sprintf(
      fmt = "'expect_type(type = )' sets the correct condition call (%s)",
      names(x = types)[i]
    )
  )
}
