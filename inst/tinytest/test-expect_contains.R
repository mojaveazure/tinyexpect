#!/usr/bin/env Rscript

# Test basic mechanics
x <- letters
y <- rev(x = letters)
expect_inherits(
  current = tt <- expect_contains(x, y),
  class = "tinytest",
  info = "'expect_contains()' returns a 'tinytest' object"
)
expect_identical(
  current = attr(x = tt, which = "short"),
  target = 'xcpt',
  info = "'expect_contains()' tests expectations"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_contains(x, y)"),
    info = "'expect_contains()' captures the call correctly"
  )
}

expect_true(
  current = expect_contains(current = letters, target = rev(letters)),
  info = paste(
    "'expect_contains()' returns TRUE when 'current'",
    "is wholly contained in 'target'"
  )
)
expect_true(
  current = expect_contains(
    current = c("a", "a", "b"),
    target = c("b", "b", "a")
  ),
  info = "'expect_contains()' ignores duplicates"
)
expect_true(
  current = expect_contains(current = letters, target = letters[-1L]),
  info = paste(
    "'expect_contains()' returns TRUE when 'current'",
    "is a superset of 'target"
  )
)
expect_false(
  current = expect_contains(current = letters[-1L], target = letters),
  info = paste(
    "'expect_contains()' returns TRUE when 'current'",
    "is not a superset of 'target"
  )
)

# Test dots
x <- letters
y <- rev(x = letters)
expect_error(
  current = expect_contains(current = x, target = y, tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_contains()' throws an error when '...' are not empty"
)
err <- tryCatch(
  expr = expect_contains(current = x, target = y, tomato = 1L),
  error = force
)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_contains()",
  info = "'expect_contains(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_contains(current = x, target = y, info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_contains()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(
    expr = expect_contains(current = x, target = y, info = infos[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_contains()",
    info = sprintf(
      fmt = "'expect_contains(info = ) sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}

# Test `current` and `target`
rlang <- list(
  data.frame(x = 1L:10L, y = 10L:1L),
  matrix(data = c(1L:10L, 10L:1L), ncol = 2L),
  array(data = c(1L:10L, 10L:1L, 1L:10L), dim = c(3L, 5L, 2L)),
  factor(x = rep_len(x = c("g1", "g2"), length.out = 15L)),
  ordered(x = rep_len(x = c("g1", "g2"), length.out = 15L)),
  list(x = 1L:10L, y = 10L:1L)
)
for (i in seq_along(along.with = rlang)) {
  expect_true(
    current = expect_contains(
      current = unname(obj = rlang[[i]]),
      target = unname(obj = rlang[[i]])
    ),
    info = sprintf(
      fmt = "'expect_contains()' treats objects of class '%s' as vectors",
      class(x = rlang[[i]])[1L]
    )
  )
}

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
    current = expect_contains(current = cases[[i]], target = 1L),
    pattern = pattern,
    info = sprintf(
      fmt = "'expect_contains()' throws an error when 'current' is %s",
      msg
    )
  )
  expect_error(
    current = expect_contains(current = 1L, target = cases[[i]]),
    pattern = pattern,
    info = sprintf(
      fmt = "'expect_contains()' throws an error when 'target' is %s",
      msg
    )
  )
  expect_error(
    current = expect_contains(current = cases[[i]], target = cases[[i]]),
    pattern = pattern,
    info = sprintf(
      fmt = paste(
        "'expect_contains()' throws an error when",
        "both 'current' and target are %s"
      ),
      switch(
        EXPR = tt,
        closure = "functions",
        "NULL" = "NULL",
        sprintf(fmt = "of type '%s'", tt)
      )
    )
  )
  err <- tryCatch(
    expr = expect_contains(current = cases[[i]], target = 1L),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_contains()",
    info = sprintf(
      fmt = "'expect_contains()' vector check sets the correct condition call (%s)",
      tt
    )
  )
}
