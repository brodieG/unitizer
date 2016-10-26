library(unitizer)
context("Options")

old.opts <- options()
new.opts <- unitizer:::options_zero()
options(old.opts)

# not great tests...

test_that("options", {
  expect_true(all(names(new.opts) %in% names(old.opts)))
  expect_true(length(new.opts) <= length(old.opts))
})
