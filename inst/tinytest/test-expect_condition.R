#!/usr/bin/env Rscript

# Test basic mechanics
expect_inherits(
  current = tt <- expect_condition(""),
  class = "tinytest",
  info = "'expect_condition()' returns a 'tinytest' object"
)
expect_identical(
  current = attr(x = tt, which = "short"),
  target = 'xcpt',
  info = "'expect_condition()' tests expectations"
)
# R-devel is weird with this
if (!grepl(pattern = "devel", x = R.version$status, ignore.case = TRUE)) {
  expect_identical(
    current = attr(x = tt, which = "call"),
    target = str2lang(s = "expect_condition('')"),
    info = "'expect_condition()' captures the call correctly"
  )
}

conditions <- list(
  nocond = function(x = "test") x,
  condition = function(x = "test") signalCondition(cond = x),
  customCondition = function(x = "test") {
    cond <- simpleCondition(message = x)
    class(x = cond) <- c("customCondition", class(x = cond))
    signalCondition(cond = cond)
  },
  message = function(x = "test") message(x),
  customMessage = function(x = "test") {
    cond <- simpleMessage(message = x)
    class(x = cond) <- c("customMessage", class = class(x = cond))
    message(cond)
  },
  warning = function(x = "test") warning(x),
  customWarning = function(x = "test") warning(warningCondition(
    x,
    class = "customWarning"
  )),
  error = function(x = "test") stop(x),
  customError = function(x = "test") stop(errorCondition(
    x,
    class = "customError"
  ))
)

pass <- "'expect_condition()' passes when 'current' throws a condition"
fail <- "'expect_condition()' fails when 'current' does not throw a condition"
wfail <- "'expect_condition()' fails when 'current' throws a condition"

for (i in seq_along(along.with = conditions)) {
  f <- conditions[[i]]
  cls <- names(x = conditions)[i]
  switch(
    EXPR = cls,
    nocond = expect_false(
      current = expect_condition(current = f()),
      info = sprintf(fmt = "%s (`%s()`)", fail, cls)
    ),
    expect_true(
      current = expect_condition(current = f()),
      info = sprintf(fmt = "%s (`%s()`)", pass, cls)
    )
  )
  # Test patterns
  expect_false(
    current = expect_condition(current = f(), pattern = "tomato"),
    info = sprintf(fmt = "%s matching 'message' (`%s()`)", fail, cls)
  )
  switch(
    EXPR = cls,
    nocond = {},
    expect_true(
      current = expect_condition(current = f("tomato"), pattern = "tomato"),
      info = sprintf(fmt = "%s matching 'pattern' (`%s()`)", pass, cls)
    )
  )
  # Test classes
  expect_false(
    current = expect_condition(current = f(), class = "notRealCondition"),
    info = sprintf(fmt = "%s inheriting from 'class' (`%s()`)", fail, cls)
  )
  switch(
    EXPR = cls,
    nocond = {},
    expect_true(
      current = expect_condition(current = f(), class = "condition"),
      info = sprintf(fmt = "%s inheriting from 'class' (`%s()`)", pass, cls)
    )
  )
  switch(
    EXPR = cls,
    nocond = ,
    condition = ,
    customCondition = {},
    expect_true(
      expect_condition(current = f(), class = c("message", "warning", "error")),
      info = sprintf(
        fmt = "%s inheriting from one of 'class' (`%s()`)",
        pass,
        cls
      )
    )
  )
  switch(
    EXPR = cls,
    nocond = ,
    condition = ,
    customCondition = {},
    message = ,
    customMessage = {
      expect_true(
        current = expect_condition(current = f(), class = "message"),
        info = sprintf(
          fmt = "'expect_condition()' passes when 'current' throws a message (`%s()`)",
          cls
        )
      )
      for (j in c("warning", "error")) {
        expect_false(
          current = expect_condition(current = f(), class = j),
          info = sprintf(
            "'expect_condition()' passes when 'current' does not throw %s %s (`%s()`)",
            switch(EXPR = j, error = "an", "a"),
            j,
            cls
          )
        )
      }
    },
    warning = ,
    customWarning = {
      expect_true(
        current = expect_condition(current = f(), class = 'warning'),
        info = sprintf(
          fmt = "'expect_condition()' passes when 'current' throws a warning (`%s()`)",
          cls
        )
      )
      for (j in c("message", "error")) {
        expect_false(
          current = expect_condition(current = f(), class = j),
          info = sprintf(
            "'expect_condition()' passes when 'current' does not throw %s %s (`%s()`)",
            switch(EXPR = j, error = "an", "a"),
            j,
            cls
          )
        )
      }
    },
    error = ,
    customError = {
      expect_true(
        current = expect_condition(current = f(), class = 'error'),
        info = sprintf(
          fmt = "'expect_condition()' passes when 'current' throws an error (`%s()`)",
          cls
        )
      )
      for (j in c("warning", "message")) {
        expect_false(
          current = expect_condition(current = f(), class = j),
          info = sprintf(
            "'expect_condition()' passes when 'current' does not throw a %s (`%s()`)",
            j,
            cls
          )
        )
      }
    }
  )
  switch(
    EXPR = cls,
    nocond = {},
    expect_true(
      current = expect_condition(current = f(), class = cls),
      info = sprintf(
        fmt = "%s inheriting from custom 'class' (`%s()`)",
        pass,
        cls
      )
    )
  )
  # Test patterns and classes
  expect_false(
    current = expect_condition(
      current = f(),
      pattern = "tomato",
      class = "notRealCondition"
    ),
    info = sprintf(
      fmt = "%s matching 'pattern' and inheriting from 'class' (`%s()`)",
      fail,
      cls
    )
  )
  expect_false(
    current = expect_condition(
      current = f("tomato"),
      pattern = "tomato",
      class = "notRealCondition"
    ),
    info = sprintf(
      fmt = "%s matching 'pattern' but not inheriting from 'class' (`%s()`)",
      wfail,
      cls
    )
  )
  switch(
    EXPR = cls,
    nocond = {},
    expect_false(
      current = expect_condition(
        current = f(),
        pattern = "tomato",
        class = "condition"
      ),
      info = sprintf(
        fmt = "%s inheriting from 'class' but not matching 'pattern' (`%s()`)",
        wfail,
        cls
      )
    )
  )
  switch(
    EXPR = cls,
    nocond = {},
    expect_true(
      current = expect_condition(
        current = f("tomato"),
        pattern = "tomato",
        class = "condition"
      ),
      info = sprintf(
        fmt = "%s matching 'pattern' and inheriting from 'class' (`%s()`)",
        pass,
        cls
      )
    )
  )
}

# Test dots
f <- conditions$condition
expect_error(
  current = expect_condition(f(), tomato = 1L),
  pattern = "^'\\.\\.\\.' must be empty$",
  info = "'expect_condition()' throws an error when '...' are not empty"
)
err <- tryCatch(expr = expect_condition(f(), tomato = 1L), error = force)
expect_identical(
  current = conditionCall(c = err),
  target = "expect_condition()",
  info = "'expect_condition(...)' sets the correct condition call"
)

# Test `info`
infos <- list(
  logical = TRUE,
  numeric = 1.1,
  vector = c("hello", "world")
)
for (i in seq_along(along.with = infos)) {
  expect_error(
    current = expect_condition(f(), info = infos[[i]]),
    pattern = "^'info' must be a single character value$",
    info = sprintf(
      fmt = "'expect_condition()' throws an error when 'info' is a %s",
      names(x = infos)[i]
    )
  )
  err <- tryCatch(expr = expect_condition(f(), info = infos[[i]]), error = force)
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_condition()",
    info = sprintf(
      fmt = "'expect_condition(info = )' sets the correct condition call (%s)",
      names(x = infos)[i]
    )
  )
}

# Test `pattern`
patterns <- list(
  "a logical" = TRUE,
  "a numeric" = 1.1,
  "a vector" = c("hello", "world"),
  "empty" = ""
)
for (i in seq_along(along.with = patterns)) {
  expect_error(
    current = expect_condition(f(), patterns[[i]]),
    pattern = "^'pattern' must be a single, non-empty string$",
    info = sprintf(
      fmt = "'expect_condition()' throws an error when 'pattern' is %s",
      names(x = patterns)[i]
    )
  )
  err <- tryCatch(expr = expect_condition(f(), patterns[[i]]), error = force)
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_condition()",
    info = sprintf(
      fmt = "'expect_condition(pattern = )' sets the correct condition call (%s)",
      names(x = patterns)[i]
    )
  )
}

# Test `class`
classes <- list(
  "logical" = TRUE,
  "numeric" = 1.1
)
for (i in seq_along(along.with = classes)) {
  expect_error(
    current = expect_condition(f(), class = classes[[i]]),
    pattern = "^'class' must be a character vector$",
    info = sprintf(
      fmt = "'expect_condition()' throws an error when 'class' is a %s",
      names(x = classes)[i]
    )
  )
  err <- tryCatch(
    expr = expect_condition(f(), class = classes[[i]]),
    error = force
  )
  expect_identical(
    current = conditionCall(c = err),
    target = "expect_condition()",
    info = sprintf(
      fmt = "'expect_condition(class = )' sets the correct condition call (%s)",
      names(x = classes)[i]
    )
  )
}
