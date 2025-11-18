#!/usr/bin/env Rscript

# Basic mechanics
expect_inherits(
  current = tt <- expect_compare(1L, 1L, operator = "=="),
  class = "tinytest",
  info = "'expect_compare()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "xcpt")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected 1 to be equal to 1"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_compare(1L, 1L, operator = '==')")
  )
}

# Test operators
x <- 1L
y <- 10L
comparisons <- c(
  "<" = "strictly less than",
  "<=" = "less than or equal to",
  ">" = "strictly greater than",
  ">=" = "greater than or equal to",
  "==" = "equal to",
  "!=" = "not equal to"
)

for (i in seq_along(along.with = comparisons)) {
  op <- names(x = comparisons)[i]
  tt <- expect_compare(current = x, target = y, operator = op)
  switch(
    EXPR = op,
    "<" = ,
    "<=" = ,
    "!=" = expect_true(
      current = as.logical(x = tt),
      info = sprintf(
        fmt = "'expect_compare()' returns TRUE when %s %s %s holds",
        x,
        op,
        y
      )
    ),
    expect_false(
      current = as.logical(x = tt),
      info = sprintf(
        fmt = "'expect_compare()' returns FALSE when %s %s %s does not hold",
        x,
        op,
        y
      )
    )
  )
  expect_true(
    current = grepl(pattern = comparisons[i], x = attr(x = tt, which = "diff")),
    info = sprintf(
      fmt = "'expect_compare()' includes proper comparison message when operator is '%s'",
      op
    )
  )
}

# Test vectorized comparisons
expect_error(
  current = expect_compare(current = 1:2, target = 1:2, operator = "=="),
  info = "'expect_compare()' throws an error when the comparison yields a vector"
)

# Test dots
expect_error(
  current = expect_compare(1L, 1L, operator = "==", tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_compare()' throws an error when '...' are not empty"
)
err <- tryCatch(
  expr = expect_compare(1L, 1L, operator = "==", tomato = 1L),
  error = force
)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_compare()",
  info = "'expect_compare(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_compare(1L, 1L, operator = "==", info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_compare()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(
    expr = expect_compare(1L, 1L, operator = "==", info = infos[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_compare()",
    info = sprintf(
      fmt = "'expect_compare(info = )' sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}

# Test `call`
types <- c("character", "complex", "integer", "list", "logical", "numeric")
for (tt in types) {
  expect_error(
    current = expect_compare(1L, 1L, operator = "==", call = vector(mode = tt)),
    pattern = "^'call' must be a call$",
    info = sprintf(
      fmt = "'expect_compare()' throws an error when 'call' is a %s",
      tt
    )
  )
  err <- tryCatch(
    expr = expect_compare(1L, 1L, operator = "==", call = vector(mode = tt)),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_compare()",
    info = "'expect_compare(call = )' sets the correct condition call"
  )
}
