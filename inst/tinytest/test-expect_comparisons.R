#!/usr/bin/env Rscript

# Basic mechanics
fxns <- sprintf(fmt = "expect_%s", c("lt", "lte", "gt", "gte"))
for (f in fxns) {
  fn <- match.fun(FUN = f)
  expect_inherits(
    current = tt <- fn(1L, 1L),
    class = "tinytest",
    info = sprintf(fmt = "'%s()' returns a 'tinytest' object", f)
  )
  expect_identical(
    current = attr(x = tt, which = "short"),
    target = "xcpt",
    info = sprintf(fmt = "'%s()' tests expectations", f)
  )
  expect_identical(
    current = attr(x = tt, which = "diff"),
    target = sprintf(
      fmt = "Expected %s to be %s %s",
      1L,
      switch(
        EXPR = f,
        expect_lt = "strictly less than",
        expect_lte = "less than or equal to",
        expect_gt = "strictly greater than",
        expect_gte = "greater than or equal to"
      ),
      1L
    ),
    info = sprintf(fmt = "'%s()' sets the correct diff", f)
  )
  for (x in 1:10) {
    for (y in 1:10) {
      fmt <- paste(sprintf(fmt = "'%s(%i, %i)", f, x, y), "returns %s")
      current <- fn(current = x, target = y)
      if (x == y) {
        switch(
          EXPR = f,
          expect_lte = ,
          expect_gte = expect_true(
            current = current,
            info = sprintf(fmt = fmt, "TRUE")
          ),
          expect_false(current = current, info = sprintf(fmt = fmt, "FALSE"))
        )
      }
      if (x < y) {
        switch(
          EXPR = f,
          expect_lt = ,
          expect_lte = expect_true(
            current = current,
            info = sprintf(fmt = fmt, "TRUE")
          ),
          expect_false(current = current, info = sprintf(fmt = fmt, "FALSE"))
        )
      }
      if (x > y) {
        switch(
          EXPR = f,
          expect_gt = ,
          expect_gte = expect_true(
            current = current,
            info = sprintf(fmt = fmt, "TRUE")
          ),
          expect_false(current = current, info = sprintf(fmt = fmt, "FALSE"))
        )
      }
    }
  }
}
