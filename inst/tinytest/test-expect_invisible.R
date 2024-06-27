expect_inherits(
  current = tt <- expect_invisible(""),
  class = 'tinytest',
  info = "'expect_invisible()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected invisible result, got a visible object"
)
(ttattr <- attr(x = tt, which = "call"))
(ttexpt <- str2lang("expect_invisible('')"))
(identical(x = ttattr, y = ttexpt))
expect_identical(
  current = ttattr,
  target = ttexpt
)

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
