cls <- methods::setClass("cls", contains = "list")

expect_inherits(
  current = tt <- expect_s4_class(cls(), "cls"),
  class = 'tinytest',
  info = "'expect_s4_class()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object of class 'cls', got 'cls'"
)
expect_identical(
  current = attr(x = tt, which = "call"),
  target = str2lang("expect_s4_class(cls(), 'cls')")
)

tt <- expect_s4_class(current = cls(), class = NA)
expect_false(
  current = as.logical(x = tt),
  info = "'expect_s4_class()' fails when obj is an S4 object and class is 'NA'"
)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected non-S4 object, got an S4 object",
  info = "'expect_s4_class()' fails when obj is an S4 object and class is 'NA'"
)

for (t in c("atomic", "S3")) {
  obj <- switch(
    EXPR = t,
    atomic = logical(),
    S3 = data.frame()
  )
  tt <- expect_s4_class(current = obj, class = class(obj)[1L])
  fmsg <- paste("'expect_s4_class()' fails when the object is", t)
  expect_false(current = as.logical(x = tt), info = fmsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = "Expected an S4 object, got a non-S4 object",
    info = fmsg
  )

  smsg <- paste("'expect_s4_class()' succeeds when object is", t, "class is 'NA'")
  tt <- expect_s4_class(current = obj, class = NA)
  expect_true(current = as.logical(x = tt), info = smsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = NA_character_,
    info = smsg
  )
}

smsg <- "'expect_s4_class() succeeds when obj is of the specified class"
xcls <- methods::setClass(Class = "xcls", contains = "cls")
tt <- expect_s4_class(current = xcls(), class = "xcls")
expect_true(current = as.logical(x = tt), info = smsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object of class 'xcls', got 'xcls'",
  info = smsg
)

imsg <- "'expect_s4_class() succeeds when obj inherits the specified class"
tt <- expect_s4_class(current = xcls(), class = "cls")
expect_true(current = as.logical(x = tt), info = imsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object that is of or inherits from 'cls', got 'xcls'",
  info = imsg
)

amsg <- "'expect_s4_class() succeeds when obj inherits one of the specified classes"
tt <- expect_s4_class(current = xcls(), class = c("xcls", "cls"))
expect_true(current = as.logical(x = tt), info = amsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object of class 'xcls'/'cls', got 'xcls'",
  info = amsg
)

tt <- expect_s4_class(current = xcls(), class = c("classRepresentation", "cls"), info = "")
expect_true(current = as.logical(x = tt), info = amsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object that is of or inherits from 'classRepresentation'/'cls', got 'xcls'",
  info = amsg
)

fmsg <- "'expect_s4_class()' fails when obj inherits from none of the specified classes"
tt <- expect_s4_class(current = xcls(), class = "classRepresentation")
expect_false(current = as.logical(x = tt), info = fmsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object that is of or inherits from 'classRepresentation', got 'xcls'",
  info = fmsg
)
