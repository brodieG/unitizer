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
