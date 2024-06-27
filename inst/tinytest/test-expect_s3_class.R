expect_inherits(
  current = tt <- expect_s3_class(data.frame(), "data.frame"),
  class = 'tinytest',
  info = "'expect_s3_class()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S3 object that inherits from 'data.frame', got 'data.frame'"
)
expect_identical(
  current = attr(x = tt, which = "call"),
  target = str2lang("expect_s3_class(data.frame(), 'data.frame')")
)

tt <- expect_s3_class(current = data.frame(), class = NA)
expect_false(
  current = as.logical(x = tt),
  info = "'expect_s3_class()' fails when obj is an S3 object and class is 'NA'"
)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected object that is not an S3 object, got an S3 object",
  info = "'expect_s3_class()' fails when obj is an S3 object and class is 'NA'"
)

cls <- methods::setClass("cls", contains = "list")
for (t in c("atomic", "S4")) {
  obj <- switch(
    EXPR = t,
    atomic = logical(),
    S4 = cls()
  )
  tt <- expect_s3_class(current = obj, class = class(obj)[1L])
  fmsg <- paste("'expect_s3_class()' fails when the object is", t)
  expect_false(current = as.logical(x = tt), info = fmsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = "Expected an S3 object, got an object that is not an S3 object",
    info = fmsg
  )

  smsg <- paste("'expect_s3_class()' succeeds when object is", t, "class is 'NA'")
  tt <- expect_s3_class(current = obj, class = NA)
  expect_true(current = as.logical(x = tt), info = smsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = "Expected object that is not an S3 object, got an S3 object",
    info = smsg
  )
}

xclass <- c("a", "b")
x <- structure(.Data = list(), class = xclass)
for (cls in xclass) {
  smsg <- "'expect_s3_class()' succeeds when obj is an S3 object that inherits from the specified class"
  tt <- expect_s3_class(current = x, class = cls)
  expect_true(
    current = as.logical(x = tt),
    info = smsg
  )
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf("Expected S3 object that inherits from '%s', got 'a'/'b'", cls),
    info = smsg
  )

  imsg <- "'expect_s3_class()' fails when obj is an S3 object that does not inherit from the specified class"
  tt <- expect_s3_class(current = x, class = "data.frame")
  expect_false(current = as.logical(x = tt), info = imsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = "Expected S3 object that inherits from 'data.frame', got 'a'/'b'",
    info = imsg
  )

  jmsg <- "'expect_s3_class() succeeds when obj is an S3 object that inherits from one of the specified classes"
  tt <- expect_s3_class(current = x, class = c("data.frame", cls))
  expect_true(current = as.logical(x = tt), info = jmsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf("Expected S3 object that inherits from 'data.frame'/'%s', got 'a'/'b'", cls),
    info = jmsg
  )

  xmsg <- "'expect_s3_class()' fails when the class of obj doesn't match the class when exact is TRUE"
  tt <- expect_s3_class(current = x, class = cls, exact = TRUE)
  expect_false(current = as.logical(x = tt), info = xmsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf("Expected S3 object of class '%s', got 'a'/'b'", cls),
    info = xmsg
  )
}

xmsg <- "'expect_s3_class()' succeeds when the class of the object exactly matches the specified class and exact is TRUE"
tt <- expect_s3_class(current = x, class = xclass, exact = TRUE)
expect_true(current = as.logical(x = tt), info = xmsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S3 object of class 'a'/'b', got 'a'/'b'",
  info = xmsg
)
