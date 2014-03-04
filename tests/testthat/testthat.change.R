library(testthat)
library(testor)

local( {
  test_that("Construction works", {
    expect_error(new("testorChanges", error=1:3), "invalid.*slot.*error.*length 2")
    expect_error(new("testorChanges", failed=letters[1:2]), "invalid.*failed.*got.*character")
  } )
  my.changes <- new("testorChanges", failed=c(1L,10L), new=c(1L,5L), removed=c(2L,4L), error=c(3L,8L))
  test_that("Output as expected", {
    expect_equal(
      capture.output(show(my.changes)), 
      c("- Replace 1 out of 10 failed tests", "- Add 1 out of 5 new tests", 
        "- Remove 2 out of 4 removed tests", "- Replace 3 out of 8 tests with errors") 
    )
  } )
  test_that("Length Works", {
    expect_equal(length(my.changes), 7)
  } )
} )
