#!/usr/bin/env Rscript

if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_package("tinyexpect")
}
