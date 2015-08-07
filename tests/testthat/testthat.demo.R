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
