pkg <- "tinyexpect"

opt <- getOption(x = "tt.extensions")
expect_inherits(current = opt, class = "list")
expect_true(current = pkg %in% names(x = opt))

ext <- opt[[pkg]]
expect_inherits(current = ext, class = "character")
expt <- grep(
  pattern = '^expect_',
  x = lsf.str(envir = getNamespace(name = pkg)),
  value = TRUE
)
expect_identical(current = sort(x = ext), target = sort(x = expt))
