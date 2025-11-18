#!/usr/bin/env Rscript

# Test basic mechanics
expect_inherits(
  current = tt <- expect_named(c(a = 1L)),
  class = "tinytest",
  info = "'expect_named()' returns a 'tinytest' object"
)
expect_identical(
  current = attr(x = tt, which = "short"),
  target = 'attr',
  info = "'expect_named()' tests expectations"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_named(c(a = 1L))"),
    info = "'expect_named()' captures the call correctly"
  )
}

x <- c(a = 1L, b = 2L, c = 3L)
y <- c(4L, 5L, 6L)
expect_true(
  current = expect_named(current = x),
  info = "'expect_named()' returns TRUE when 'current' is named"
)
expect_false(
  current = expect_named(current = y),
  info = "'expect_named()' returns FALSE when 'current' is unnamed"
)
expect_false(
  current = expect_named(current = x, target = NULL),
  info = "'expect_named(target = NULL)' returns FALSE when 'current' is named"
)
expect_true(
  current = expect_named(current = y, target = NULL),
  info = "'expect_named(target = NULL)' returns TRUE when 'current' is unnamed"
)

expect_true(
  current = expect_named(current = x, target = c("a", "b", "c")),
  info = "'expect_named()' returns TRUE when 'names(current)' and 'target' match"
)
grid <- Filter(
  f = Negate(f = is.null),
  x = apply(
    X = expand.grid(
      c("a", "b", "c"),
      c("a", "b", "c"),
      c("a", "b", "c"),
      stringsAsFactors = FALSE
    ),
    MARGIN = 1L,
    FUN = function(g, nm) {
      if (anyDuplicated(x = g)) {
        return(NULL)
      }
      g <- unname(obj = g)
      if (identical(x = g, y = nm)) {
        return(NULL)
      }
      return(g)
    },
    nm = names(x = x),
    simplify = FALSE
  )
)
fxn <- "expect_named"
for (i in seq_along(along.with = grid)) {
  nm <- paste0(dQuote(grid[[i]], q = FALSE), collapse = ", ")
  expect_false(
    current = expect_named(current = x, target = grid[[i]]),
    info = sprintf(
      fmt = paste(
        "'%s()' returns FALSE when 'names(current)'",
        "don't match 'target' (c(%s))"
      ),
      fxn,
      nm
    )
  )
  expect_true(
    current = expect_named(current = x, target = grid[[i]], ignore.order = TRUE),
    info = sprintf(
      fmt = paste(
        "'%s(ignore.order = TRUE)' returns TRUE when 'names(current)'",
        "and 'target' contain the same values (c(%s))"
      ),
      fxn,
      nm
    )
  )
}
for (i in names(x = x)) {
  expect_false(
    current = expect_named(current = x, target = i),
    info = sprintf(
      fmt = paste(
        "'expect_named()' returns FALSE when not all of",
        "'names(current)' are in 'target' (%s)"
      ),
      i
    )
  )
}
expect_false(
  current = expect_named(current = x, target = c(names(x = x), names(x = x))),
  info = paste(
    "'expect_named()' returns FALSE when 'names(current)'",
    "and 'target' are not identical"
  )
)

nm <- toupper(x = names(x = x))
expect_false(
  current = expect_named(current = x, target = nm),
  info = paste(
    "'expect_named()' returns FALSE when 'names(current)'",
    "and 'target' do not match case-sensitive"
  )
)
expect_true(
  current = expect_named(current = x, target = nm, ignore.case = TRUE),
  info = paste(
    "'expect_named(ignore.case = TRUE)' returns TRUE when",
    "'names(current)' and 'target' match case-insensitive"
  )
)
for (i in seq_along(along.with = grid)) {
  gi <- toupper(x = grid[[i]])
  nm <- paste0(dQuote(gi, q = FALSE), collapse = ", ")
  expect_false(
    current = expect_named(current = x, target = gi, ignore.order = TRUE),
    info = sprintf(
      fmt = "'%s()' returns FALSE when 'names(current)' don't match 'target' (c(%s))",
      fxn,
      nm
    )
  )
  expect_true(
    current = expect_named(
      current = x,
      target = gi,
      ignore.order = TRUE,
      ignore.case = TRUE
    ),
    info = sprintf(
      fmt = paste(
        "'%s(ignore.case = TRUE)' returns TRUE when 'names(current)'",
        "and 'target' contain the same values (c(%s))"
      ),
      fxn,
      nm
    )
  )
}

# Test dots
expect_error(
  current = expect_named(c(a = 1L), tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_named()' throws an error when '...' are not empty"
)
err <- tryCatch(
  expr = expect_named(c(a = 1L), tomato = 1L),
  error = force
)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_named()",
  info = "'expect_named(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_named(c(a = 1L), info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_named()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(
    expr = expect_named(c(a = 1L), info = infos[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_named()",
    info = sprintf(
      fmt = "'expect_named(info = ) sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}

# Test `target`
types <- c(
  "complex",
  "double",
  "expression",
  "integer",
  "list",
  "logical",
  "raw"
)
for (tt in types) {
  expect_error(
    current = expect_named(c(a = 1L), target = vector(mode = tt, length = 1L)),
    pattern = "^'target' must be a character vector$",
    info = sprintf(
      fmt = "'expect_named()' throws an error when 'target' is a %s",
      tt
    )
  )
  err <- tryCatch(
    expr = expect_named(c(a = 1L), target = vector(mode = tt, length = 1L)),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_named()",
    info = sprintf(
      fmt = "'expect_named(target = ) sets the correct condition call (%s)",
      tt
    )
  )
}

# Test `ignore.order`
types <- c(
  "character",
  "complex",
  "double",
  "expression",
  "integer",
  "list",
  "raw"
)
for (tt in types) {
  expect_error(
    current = expect_named(
      c(a = 1L),
      ignore.order = vector(mode = tt, length = 1L)
    ),
    pattern = "^'ignore.order' must be TRUE or FALSE$",
    info = sprintf(
      fmt = "'expect_named' throws an error when 'ignore.order' is a %s",
      tt
    )
  )
  err <- tryCatch(
    expr = expect_named(c(a = 1L), ignore.order = vector(mode = tt, length = 1L)),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_named()",
    info = sprintf(
      fmt = "'expect_named(ignore.order = ) sets the correct condition call (%s)",
      tt
    )
  )
}
expect_error(
  current = expect_named(c(a = 1L), ignore.order = NA),
  pattern = "^'ignore.order' must be TRUE or FALSE$",
  info = "'expect_named' throws an error when 'ignore.order' is NA"
)
expect_error(
  current = expect_named(c(a = 1L), ignore.order = logical(length = 2L)),
  pattern = "^'ignore.order' must be TRUE or FALSE$",
  info = "'expect_named' throws an error when 'ignore.order' is a vector"
)

# Test `ignore.case`
for (tt in types) {
  expect_error(
    current = expect_named(
      c(a = 1L),
      ignore.case = vector(mode = tt, length = 1L)
    ),
    pattern = "^'ignore.case' must be TRUE or FALSE$",
    info = sprintf(
      fmt = "'expect_named' throws an error when 'ignore.case' is a %s",
      tt
    )
  )
  err <- tryCatch(
    expr = expect_named(c(a = 1L), ignore.case = vector(mode = tt, length = 1L)),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_named()",
    info = sprintf(
      fmt = "'expect_named(ignore.case = ) sets the correct condition call (%s)",
      tt
    )
  )
}
expect_error(
  current = expect_named(c(a = 1L), ignore.case = NA),
  pattern = "^'ignore.case' must be TRUE or FALSE$",
  info = "'expect_named' throws an error when 'ignore.case' is NA"
)
expect_error(
  current = expect_named(c(a = 1L), ignore.case = logical(length = 2L)),
  pattern = "^'ignore.case' must be TRUE or FALSE$",
  info = "'expect_named' throws an error when 'ignore.case' is a vector"
)
