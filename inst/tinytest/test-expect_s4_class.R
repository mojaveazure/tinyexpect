#!/usr/bin/env Rscript

if (!requireNamespace("methods", quietly = TRUE)) {
  exit_file(msg = "Cannot find 'methods'")
}

# Test basic mechanics
cls <- methods::setClass(Class = "cls", contains = "list")
expect_inherits(
  current = tt <- expect_s4_class(cls(), "cls"),
  class = "tinytest",
  info = "'expect_s4_class()' returns a 'tinytest' object"
)
expect_identical(current = attr(x = tt, which = "short"), target = "attr")
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object of class 'cls', got 'cls'"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang("expect_s4_class(cls(), 'cls')")
  )
}

# Test `class = NA`
expect_false(
  tt <- expect_s4_class(current = cls(), class = NA),
  info = "'expect_s4_class()' fails when obj is an S4 object and class is 'NA'"
)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected non-S4 object, got an S4 object",
  info = "'expect_s4_class()' fails when obj is an S4 object and class is 'NA'"
)

# Test non-S4 objects
for (t in c("atomic", "S3")) {
  obj <- switch(
    EXPR = t,
    atomic = logical(),
    S3 = data.frame()
  )

  fmsg <- sprintf(fmt = "'expect_s4_class()' fails when the object is %s", t)
  expect_false(
    current = tt <- expect_s4_class(current = obj, class = class(obj)[1L]),
    info = fmsg
  )
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = "Expected an S4 object, got a non-S4 object",
    info = fmsg
  )

  smsg <- sprintf(
    fmt = "'expect_s4_class()' succeeds when object is %s class is 'NA'",
    t
  )
  expect_true(tt <- expect_s4_class(current = obj, class = NA), info = smsg)
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = NA_character_,
    info = smsg
  )
}

# Test simple inheritance
smsg <- "'expect_s4_class() succeeds when obj is of the specified class"
xcls <- methods::setClass(Class = "xcls", contains = "cls")
tt <- expect_s4_class(current = xcls(), class = "xcls")
expect_true(current = as.logical(x = tt), info = smsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object of class 'xcls', got 'xcls'",
  info = smsg
)

# Test simple failure
imsg <- "'expect_s4_class() succeeds when obj inherits the specified class"
expect_true(tt <- expect_s4_class(current = xcls(), class = "cls"), info = imsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object that is of or inherits from 'cls', got 'xcls'",
  info = imsg
)

# Test one-of-many
amsg <- "'expect_s4_class() succeeds when obj inherits one of the specified classes"
expect_true(
  tt <- expect_s4_class(current = xcls(), class = c("xcls", "cls")),
  info = amsg
)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = "Expected S4 object of class 'xcls'/'cls', got 'xcls'",
  info = amsg
)

expect_true(
  tt <- expect_s4_class(
    current = xcls(),
    class = c("classRepresentation", "cls")
  ),
  info = amsg
)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = paste(
    "Expected S4 object that is of or inherits",
    "from 'classRepresentation'/'cls', got 'xcls'"
  ),
  info = amsg
)

# Test none-of-many
fmsg <- paste(
  "'expect_s4_class()' fails when obj inherits",
  "from none of the specified classes"
)
tt <- expect_s4_class(current = xcls(), class = "classRepresentation")
expect_false(current = as.logical(x = tt), info = fmsg)
expect_identical(
  current = attr(x = tt, which = "diff"),
  target = paste(
    "Expected S4 object that is of or inherits",
    "from 'classRepresentation', got 'xcls'"
  ),
  info = fmsg
)

# Test dots
expect_error(
  current = expect_s4_class(cls(), "cls", tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_s4_class()' throws an error when '...' are not empty"
)
err <- tryCatch(
  expr = expect_s4_class(cls(), "cls", tomato = 1L),
  error = force
)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_s4_class()",
  info = "'expect_s4_class(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_s4_class(cls(), "cls", info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_s4_class()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(
    expr = expect_s4_class(cls(), "cls", info = infos[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_s4_class()",
    info = sprintf(
      fmt = "'expect_s4_class(info = ) sets the correct condition call (%s)",
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
    current = expect_s4_class(cls(), classes[[i]]),
    pattern = "^'class' must be a non-empty character vector$",
    info = sprintf(
      fmt = "'expect_s4_class()' throws an error when 'class' is %s",
      names(x = classes)[i]
    )
  )
  err <- tryCatch(
    expr = expect_s4_class(cls(), classes[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_s4_class()",
    info = sprintf(
      fmt = "'expect_s4_class(class = )' sets the correct condition call (%s)",
      names(x = classes)[i]
    )
  )
}
