
expect_inherits(
  tt <- expect_visible(''),
  class = 'tinytest',
  info = "'expect_visible()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected visible result, got an invisible object"
)
expect_identical(
  current = attr(x = tt, which = "call"),
  target = str2lang("expect_visible('')")
)


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
