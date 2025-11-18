#!/usr/bin/env Rscript

pkg <- "tinyexpect"

if (!requireNamespace(pkg, quietly = TRUE)) {
  exit_file(msg = sprintf(fmt = "Cannot find '%s'", pkg))
}

opt <- getOption(x = "tt.extensions")
expect_inherits(current = opt, class = "list")
expect_true(current = pkg %in% names(x = opt))

registered <- opt[[pkg]]
expect_inherits(current = registered, class = "character")
exported <- Filter(
  f = function(x) startsWith(x = x, prefix = "expect_"),
  x = getNamespaceExports(ns = getNamespace(name = pkg))
)
expect_identical(current = sort(x = registered), target = sort(x = exported))
