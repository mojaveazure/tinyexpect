expect_inherits(
  current = tt <- expect_type("", "character"),
  class = 'tinytest',
  info = "'expect_type()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected object of type 'character', got 'character'"
)
expect_identical(
  current = attr(x = tt, which = "call"),
  target = str2lang("expect_type('', 'character')")
)

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
    S4 = methods::getClass('matrix'),
    do.call(t, list())
  )
  expect_true(
    current = as.logical(x = expect_type(current = obj, type = t)),
    info = sprintf(info, "TRUE", class(obj)[1L], t)
  )
  for (xt in setdiff(x = types, y = t)) {
    expect_false(
      current = as.logical(x = expect_type(current = obj, type = xt)),
      info = sprintf(info, "FALSE", class(obj)[1L], xt)
    )
  }
}
