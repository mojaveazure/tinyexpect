#!/usr/bin/env Rscript

# `.dots_err()`
expect_inherits(
  de <- .dots_err(),
  class = "simpleError"
)
expect_identical(
  current = conditionMessage(c = de),
  target = "'...' must be empty"
)

# `.info_err()`
expect_inherits(
  de <- .info_err(),
  class = "simpleError"
)
expect_identical(
  current = conditionMessage(c = de),
  target = "'info' must be a single character value"
)

# `.is_na()`
nas <- list(NA, NA_character_, NA_complex_, NA_integer_, NA_real_)
for (i in seq_along(along.with = nas)) {
  tt <- typeof(x = nas[[i]])
  expect_true(
    current = .is_na(x = nas[[i]]),
    info = sprintf(fmt = "'.is_na()' returns TRUE for NAs of type %s", tt)
  )
  expect_false(
    current = .is_na(x = c(nas[[i]], nas[[i]])),
    info = sprintf(fmt = "'.is_na()' returns FALSE for vectors of type %s", tt)
  )
}
for (i in c(TRUE, FALSE)) {
  expect_false(
    current = .is_na(x = i),
    info = sprintf(fmt = ".is_na()' returns FALSE for %s", i)
  )
}

# `.is_string()`
expect_true(current = .is_string(x = "a"))
expect_true(current = .is_string(x = "hello"))
expect_true(current = .is_string(x = "hello world"))
types <- c(
  "complex",
  "double",
  "integer",
  "list",
  "logical",
  "numeric",
  "raw"
)
for (tt in types) {
  expect_false(
    current = .is_string(x = vector(mode = tt, length = 1L)),
    info = sprintf(fmt = "'.is_string() returns FALSE when 'x' is of type %s", tt)
  )
}
for (i in c(0L, 2L)) {
  expect_false(
    current = .is_string(x = vector(mode = "character", length = i)),
    info = sprintf(
      fmt = "'.is_string()' returns FALSE when 'x' is a character of length %i",
      i
    )
  )
}
expect_false(
  current = .is_string(x = NA_character_),
  info = "'.is_string()' returns FALSE for character NAs"
)
expect_false(
  current = .is_string(x = ""),
  info = "'.is_string()' returns FALSE for empty characters ('')"
)

# `.is_var()`
if (requireNamespace("withr", quietly = TRUE)) {
  fmt <- "'.is_var()' returns %s when 'var' is '%s' and strict is %s"
  trueish <- list(1L, "true", TRUE, "character")
  falseish <- list(0L, "false", FALSE)
  for (op in c(TRUE, FALSE)) {
    withr::with_options(
      new = c(tinyexpect.envvar.strict = op),
      code = {
        for (i in seq_along(along.with = trueish)) {
          var <- trueish[[i]]
          withr::with_envvar(
            new = c(R_TINYEXPECT_ENVVAR = var),
            code = if (!op || isTRUE(x = as.logical(x = var))) {
              expect_true(
                current = .is_var(var = "R_TINYEXPECT_ENVVAR"),
                info = sprintf(fmt = fmt, "TRUE", var, op)
              )
            } else {
              expect_false(
                current = .is_var(var = "R_TINYEXPECT_ENVVAR"),
                info = sprintf(fmt = fmt, "FALSE", var, op)
              )
            }
          )
        }
        for (i in seq_along(along.with = falseish)) {
          var <- falseish[[i]]
          withr::with_envvar(
            new = c(R_TINYEXPECT_ENVVAR = var),
            code = expect_false(
              current = .is_var(var = "R_TINYEXPECT_ENVVAR"),
              info = sprintf(fmt = fmt, "FALSE", var, op)
            )
          )
        }
      }
    )
  }
}

# `.is_vector()`
cases <- list(
  "a vector" = vector(),
  "a factor" = factor(),
  "a list" = list(),
  "data frame" = data.frame(),
  "a matrix" = matrix(),
  "an array" = array()
)
for (i in seq_along(along.with = cases)) {
  expect_true(
    current = .is_vector(x = cases[[i]]),
    info = sprintf(
      fmt = "'.is_vector()' returns TRUE when 'x' is %s",
      names(x = cases)[i]
    )
  )
}
