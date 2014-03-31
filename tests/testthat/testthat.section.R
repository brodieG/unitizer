library(testor)
library(testthat)

expr.1 <- expression(1 + 1, b <- 5, matrix(integer(), nrow=b, ncol=b))
expr.2 <- {1 + 1; b <- 5; matrix(integer(), nrow=b, ncol=b)}
expr.3 <- quote(expression(1 + 1, b <- 5, matrix(integer(), nrow=b, ncol=b)))
expr.4 <- quote({1 + 1; b <- 5; matrix(integer(), nrow=b, ncol=b)})

expect_error(testor_sect(1:3), "must be a 1 length character vector")
expect_error(testor_sect(letters), , "must be a 1 length character vector")
expect_error(testor_sect("mytest", expr.1, 1:3), "`details` must be character")
expect_error(testor_sect("mytest", expr.1, letters, letters), "must be \"testorItemTestsFuns\" or a function")
# note the following produces a message, but it's not actually an error, it's just that there are multiple errors
expect_error(testor_sect("mytest", expr.1, letters, runif), "Problem with provided function")
expect_error(testor_sect("mytest", expr.2), "Argument `expr` must be an expression", "`expr.2` is already evaluated")
expect_error(testor_sect("mytest", matrix(1:9, nrow=3)), "Argument `expr` must be an expression")

expect_is(sect.1 <- testor_sect("mytest", expr.1), "testorSectionExpression")
expect_identical(testor:::as.expression(sect.1), expr.1)
expect_is(sect.2 <- testor_sect("mytest", {1 + 1; b <- 5; matrix(integer(), nrow=b, ncol=b)}), "testorSectionExpression")
expect_identical(sect.1, sect.2)
expect_is(sect.3 <- testor_sect("mytest", expr.3), "testorSectionExpression")
expect_identical(sect.1, sect.3)
expect_is(sect.4 <- testor_sect("mytest", expr.4), "testorSectionExpression")
expect_identical(sect.1, sect.4)
expect_is(sect.5 <- testor_sect("mytest", expression(1 + 1, b <- 5, matrix(integer(), nrow=b, ncol=b))), "testorSectionExpression")
expect_identical(sect.1, sect.5)
expect_is(sect.1 <- testor_sect("mytest", expr.1, compare=identical), "testorSectionExpression")

message("All tests completed")