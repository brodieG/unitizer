library(testthat)
library(unitizer)
context("Change")

local( {
  test_that("Construction works", {
    expect_error(new("unitizerChanges", removed=1:3), "invalid.*slot.*removed.*length 2")
    expect_error(new("unitizerChanges", failed=letters[1:2]), "invalid.*failed.*got.*character")
  } )
  my.changes <- new(
    "unitizerChanges", failed=c(1L,10L), new=c(1L,5L), removed=c(2L,4L),
    corrupted=c(3L,8L)
  )
  test_that("Output as expected", {
    expect_equal(
      capture.output(show(my.changes)),
      c("- Replacing 1 out of 10 failed tests", "- Adding 1 out of 5 new tests",
        "- Removing 2 out of 4 removed tests", "- Replacing 3 out of 8 tests with errors")
    )
  } )
  test_that("Length Works", {
    expect_equal(length(my.changes), 7)
  } )
} )
