library(unitizer)

test_that("copy fastlm dir works", {
  x <- copy_fastlm_to_tmpdir()
  expect_identical(
    list.files(x),
    c("DESCRIPTION", "man", "NAMESPACE", "R", "tests")
  )
  expect_identical(
    readLines(file.path(x, "DESCRIPTION"))[[5L]],
    "Version: 0.1.0"
  )
  update_fastlm(x, version="0.1.1")
  expect_identical(
    readLines(file.path(x, "DESCRIPTION"))[[5L]],
    "Version: 0.1.1"
  )
  update_fastlm(x, version="0.1.2")
  expect_identical(
    readLines(file.path(x, "DESCRIPTION"))[[5L]],
    "Version: 0.1.2"
  )
  unlink(x, recursive=TRUE)
})

test_that("show_file", {
  f <- tempfile()
  cat("this is a\ntest code\nfile\n", file=f)
  file.show <- capture.output(show_file(f))
  expect_equal(file.show[[1L]], "+---------------+")
  start.file <- grep("+---+-----------+", file.show, fixed=TRUE)
  expect_equal(length(start.file), 2L)
  expect_equal(
    file.show[start.file[[1L]]:start.file[[2L]]],
    c("+---+-----------+", "| 1 | this is a |", "| 2 | test code |", "| 3 | file      |", "+---+-----------+")
  )
  unlink(f)
})
# Run actual demo bits; note we want to force `interactive.mode=TRUE` so that
# `read_line_vals` values are used as user input

(.unitizer.fastlm <- copy_fastlm_to_tmpdir())    # package directory
devtools::install(.unitizer.fastlm, quiet=TRUE)  # install first version
.unitizer.test.file <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm.R")
.unitizer.test.store <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm.unitizer")

unitizer:::read_line_set_vals(c("Y", "Y", "Y", "Y", "Y"))
cat(unitizer:::.global$prompt.vals, sep="\n")
untz <- unitize(.unitizer.test.file, interactive.mode=TRUE)

test_that("demo create worked", {
  expect_is(untz, "unitizer")
  expect_equal(
    untz@items.ref.calls.deparse,
    c("library(unitizer.fastlm)", "dat <- data.frame(x = 1:100, y = (1:100)^2)", "res <- fastlm(dat$x, dat$y)", "res", "get_slope(res)", "get_rsq(res)", "fastlm(1:100, 1:10)")
  )
  expect_equal(
    lapply(unitizer:::as.list(untz@items.ref[4:7]), function(x) x@data@value),
    list(structure(c(-1717, 101, 0.938678984853783), .Names = c("intercept", "slope", "rsq"), class = "fastlm"), 101, 0.938678984853783, NULL)
  )
})
unitizer:::read_line_set_vals(c("5", "Q"))
txt <- capture.output(review(.unitizer.test.store)) # review is always in interactive mode

test_that("demo review", {
  expect_true(
    grepl(
      "5\\. get_slope\\(res\\).*> get_slope\\(res\\)\\[1\\] 101.*Q",
      paste0(txt, collapse="")
    )
  )
})

unitizer:::read_line_set_vals(NULL)

