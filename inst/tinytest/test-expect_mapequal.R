#!/usr/bin/env Rscript

# Test basic mechanics
x <- c(a = 1L, b = 2L)
y <- c(b = 2L, a = 1L)
expect_inherits(
  current = tt <- expect_mapequal(x, y),
  class = "tinytest",
  info = "'expect_mapequal()' returns a 'tinytest' object"
)
expect_identical(
  current = attr(x = tt, which = "short"),
  target = 'xcpt',
  info = "'expect_mapequal()' tests expectations"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_mapequal(x, y)"),
    info = "'expect_mapequal()' captures the call correctly"
  )
}

expect_true(
  current = expect_mapequal(current = c(a = 1L, b = 2L), target = c(b = 2L, a = 1L))
)
expect_false(
  current = expect_mapequal(current = c(a = 1L, b = 2L), target = c(a = 2L, b = 1L))
)

# Test dots
x <- c(a = 1L, b = 2L)
y <- c(b = 2L, a = 1L)
expect_error(
  current = expect_mapequal(current = x, target = y, tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_mapequal()' throws an error when '...' are not empty"
)
err <- tryCatch(
  expr = expect_mapequal(current = x, target = y, tomato = 1L),
  error = force
)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_mapequal()",
  info = "'expect_mapequal(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_mapequal(current = x, target = y, info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_mapequal()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(
    expr = expect_mapequal(current = x, target = y, info = infos[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_mapequal()",
    info = sprintf(
      fmt = "'expect_mapequal(info = ) sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}

# Test `current` and `target`
cases <- list(
  function(x) x,
  NULL
)
if (requireNamespace("Matrix", quietly = TRUE)) {
  cases <- c(cases, list("an S4 object" = Matrix::Matrix()))
}
pattern <- "^Both 'current' and 'target' must be vectors$"
for (i in seq_along(along.with = cases)) {
  tt <- typeof(x = cases[[i]])
  msg <- switch(
    EXPR = tt,
    closure = "a function",
    "NULL" = "NULL",
    sprintf(fmt = "of type '%s'", tt)
  )
  expect_error(
    current = expect_mapequal(current = cases[[i]], target = 1L),
    pattern = pattern,
    info = sprintf(
      fmt = "'expect_mapequal()' throws an error when 'current' is %s",
      msg
    )
  )
  expect_error(
    current = expect_mapequal(current = 1L, target = cases[[i]]),
    pattern = pattern,
    info = sprintf(
      fmt = "'expect_mapequal()' throws an error when 'target' is %s",
      msg
    )
  )
  expect_error(
    current = expect_mapequal(current = cases[[i]], target = cases[[i]]),
    pattern = pattern,
    info = sprintf(
      fmt = "'expect_mapequal()' throws an error when both 'current' and target are %s",
      switch(
        EXPR = tt,
        closure = "functions",
        "NULL" = "NULL",
        sprintf(fmt = "of type '%s'", tt)
      )
    )
  )
  err <- tryCatch(
    expr = expect_mapequal(current = cases[[i]], target = 1L),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_mapequal()",
    info = sprintf(
      fmt = "'expect_mapequal()' vector check sets the correct condition call (%s)",
      tt
    )
  )
}

# Test `length(current) && length(target)`
expect_warning(
  tt <- expect_mapequal(current = vector(), target = vector()),
  pattern = "^Both 'current' and 'target' are empty$",
  info = "'expect_mapequal()' throws a warning when 'current' and 'target' are empty"
)
wrn <- tryCatch(
  expr = expect_mapequal(current = vector(), target = vector()),
  warning = force
)
expect_identical(
  current = conditionCall(c = wrn),
  target = "expect_mapequal()",
  info = "'expect_mapequal()' length warning sets the correct condition call"
)
expect_true(
  current = tt,
  info = "'expect_mapequal()' returns TRUE when 'current' and 'target' are empty"
)

# Test `names(current)` and `names(target)`
for (i in c("current", "target")) {
  # Duplicate names
  current <- switch(EXPR = i, current = c(x, x), x)
  target <- switch(EXPR = i, target = c(y, y), y)
  pattern <- sprintf(fmt = "^Names of '%s' must be unique and non-empty$", i)
  expect_error(
    current = expect_mapequal(current = current, target = target),
    pattern = pattern,
    info = sprintf(
      fmt = "'expect_mapequal()' throws an error when '%s' has duplicate names",
      i
    )
  )
  err <- tryCatch(
    expr = expect_mapequal(current = current, target = target),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_mapequal()",
    info = sprintf(
      fmt = "'expect_mapequal()' duplicate names error sets the correct condition call (%s)",
      i
    )
  )
  # Empty names
  current <- switch(EXPR = i, current = c(x, x[[1L]]), x)
  target <- switch(EXPR = i, target = c(y, y[[1L]]), y)
  expect_error(
    current = expect_mapequal(current = current, target = target),
    pattern = pattern,
    info = sprintf(
      fmt = "'expect_mapequal()' throws an error when '%s' has unnamed values",
      i
    )
  )
  err <- tryCatch(
    expr = expect_mapequal(current = current, target = target),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_mapequal()",
    info = sprintf(
      fmt = "'expect_mapequal()' unnamed names error sets the correct condition call (%s)",
      i
    )
  )
}

# Test fully unnamed
for (i in c("current", "target")) {
  current <- switch(EXPR = i, current = unname(obj = x), x)
  target <- switch(EXPR = i, target = unname(obj = y), y)
  expect_false(
    tt <- expect_mapequal(current = current, target = target),
    info = sprintf(fmt = "'expect_mapequal()' returns FALSE when '%s' is unnamed", i)
  )
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = "Both 'current' and 'target' must be named",
    info = sprintf(fmt = "'expect_mapequal()' sets the correct diff when '%s' is unnamed", i)
  )
}

# Test `!setequal(names(current), names(target))`
cases <- c("current", "target")
nletters <- `names<-`(letters, letters)
for (i in cases) {
  current <- switch(EXPR = i, current = nletters, x)
  target <- switch(EXPR = i, target = nletters, y)
  fmt <- sprintf(
    fmt = "'expect_mapequal()' %s when '%s' has names not present in '%s'",
    "%s",
    i,
    setdiff(x = cases, y = i)
  )
  expect_false(
    tt <- expect_mapequal(current = current, target = target),
    info = sprintf(fmt = fmt, "returns FALSE")
  )
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf(
      fmt = paste(
        "Expected names to be identical between 'current' and 'target',",
        "observed names in '%s' not present in '%s'"
      ),
      i,
      setdiff(x = cases, y = i)
    ),
    info = sprintf(fmt = fmt, "sets the correct diff")
  )
}
