library(unitizer)

test_that("copy fastlm dir works", {
  x <- copy_fastlm_to_tmpdir()
  expect_identical(
    list.files(x),
    c("DESCRIPTION", "man", "NAMESPACE", "R", "tests")
  )
  unlink(x, recursive=TRUE)
})
