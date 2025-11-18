#!/usr/bin/env Rscript

# Test basic mechanics
expect_inherits(
  current = tt <- expect_s3_class(data.frame(), "data.frame"),
  class = "tinytest",
  info = "'expect_s3_class()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S3 object that inherits from 'data.frame', got 'data.frame'"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang("expect_s3_class(data.frame(), 'data.frame')")
  )
}

# Test `class = NA`
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

# Test non-S3 classes
cls <- methods::setClass(Class = "cls", contains = "list")
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

# Test S3 inheritance
xclass <- c("a", "b")
x <- structure(.Data = list(), class = xclass)
for (cls in xclass) {
  # Simple inheritance
  smsg <- paste(
    "'expect_s3_class()' succeeds when obj is an S3 object",
    "that inherits from the specified class"
  )
  tt <- expect_s3_class(current = x, class = cls)
  expect_true(
    current = as.logical(x = tt),
    info = smsg
  )
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf(
      fmt = "Expected S3 object that inherits from '%s', got 'a'/'b'",
      cls
    ),
    info = smsg
  )

  # Simple inheritance, failure
  imsg <- paste(
    "'expect_s3_class()' fails when obj is an S3 object",
    "that does not inherit from the specified class"
  )
  tt <- expect_s3_class(current = x, class = "data.frame")
  expect_false(current = as.logical(x = tt), info = imsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = "Expected S3 object that inherits from 'data.frame', got 'a'/'b'",
    info = imsg
  )

  # Simple inheritance, one of many
  jmsg <- paste(
    "'expect_s3_class() succeeds when obj is an S3 object",
    "that inherits from one of the specified classes"
  )
  tt <- expect_s3_class(current = x, class = c("data.frame", cls))
  expect_true(current = as.logical(x = tt), info = jmsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf(
      fmt = "Expected S3 object that inherits from 'data.frame'/'%s', got 'a'/'b'",
      cls
    ),
    info = jmsg
  )

  # Exact inheritance, failure
  xmsg <- paste(
    "'expect_s3_class()' fails when the class of obj",
    "doesn't match the class when exact is TRUE"
  )
  tt <- expect_s3_class(current = x, class = cls, exact = TRUE)
  expect_false(current = as.logical(x = tt), info = xmsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf(
      fmt = "Expected S3 object of class '%s', got 'a'/'b'",
      cls
    ),
    info = xmsg
  )
}

# Test exact S3 inheritance
xmsg <- paste(
  "'expect_s3_class()' succeeds when the class of the object",
  "exactly matches the specified class and exact is TRUE"
)
tt <- expect_s3_class(current = x, class = xclass, exact = TRUE)
expect_true(current = as.logical(x = tt), info = xmsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S3 object of class 'a'/'b', got 'a'/'b'",
  info = xmsg
)

# Test dots
expect_error(
  current = expect_s3_class(data.frame(), "data.frame", tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_s3_class()' throws an error when '...' are not empty"
)
err <- tryCatch(
  expr = expect_s3_class(data.frame(), "data.frame", tomato = 1L),
  error = force
)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_s3_class()",
  info = "'expect_s3_class(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_s3_class(data.frame(), "data.frame", info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_s3_class()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(
    expr = expect_s3_class(data.frame(), "data.frame", info = infos[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_s3_class()",
    info = sprintf(
      fmt = "'expect_s3_class(info = ) sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}

# Test `class`
classes <- list(
  "a logical" = TRUE,
  "a numeric" = 1.1,
  empty = ""
)
for (i in seq_along(along.with = classes)) {
  expect_error(
    current = expect_s3_class(data.frame(), classes[[i]]),
    pattern = "^'class' must be a non-empty character vector$",
    info = sprintf(
      fmt = "'expect_s3_class()' throws an error when 'class' is %s",
      names(x = classes)[i]
    )
  )
  err <- tryCatch(
    expr = expect_s3_class(data.frame(), classes[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_s3_class()",
    info = sprintf(
      fmt = "'expect_s3_class(class = )' sets the correct condition call (%s)",
      names(x = classes)[i]
    )
  )
}

# Test `exact`
exacts <- list(
  character = "true",
  numeric = 1.1,
  vector = c(TRUE, TRUE),
  "NA" = NA
)
for (i in seq_along(along.with = exacts)) {
  expect_error(
    current = expect_s3_class(data.frame(), "data.frame", exact = exacts[[i]]),
    pattern = "^'exact' must be TRUE or FALSE$",
    info = sprintf(
      fmt = "'expect_s3_class()' throws an error when 'exact' is %s %s",
      ifelse(test = is.na(x = exacts)[[i]], yes = "an", no = "a"),
      names(x = exacts)[i]
    )
  )
  err <- tryCatch(
    expr = expect_s3_class(data.frame(), "data.frame", exact = exacts[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_s3_class()",
    info = sprintf(
      fmt = "'expect_s3_class(exact = )' sets the correct condition call (%s)",
      names(x = exacts)[i]
    )
  )
}
