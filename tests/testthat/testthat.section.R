library(unitizer)
library(testthat)
context("Section")

local({
  expr.1 <- expression(1 + 1, b <- 5, matrix(integer(), nrow=b, ncol=b))
  expr.2 <- {1 + 1; b <- 5; matrix(integer(), nrow=b, ncol=b)}
  expr.3 <- quote(expression(1 + 1, b <- 5, matrix(integer(), nrow=b, ncol=b)))
  expr.4 <- quote({1 + 1; b <- 5; matrix(integer(), nrow=b, ncol=b)})

  test_that("simple tests", {
    expect_error(unitizer_sect(1:3), "must be a 1 length character vector")
    expect_error(unitizer_sect(letters), "must be a 1 length character vector")
    expect_error(unitizer_sect("mytest", expr.1, 1:3), "`details` must be character")
    # note the following two produce error messages, but it's not actually an error,
    # it's just that there are multiple errors and `expect_error` only suppresses
    # the last one, not the preceding ones.
    expect_error(
      unitizer_sect("mytest", expr.1, letters, letters),
      "Argument `compare` must be \"testFuns\" or a function"
    )
    expect_error(
      unitizer_sect("mytest", expr.1, letters, identity),
      "does not have at least two arguments"
    )
    expect_error(
      unitizer_sect("mytest", expr.2),
      "Argument `expr` must be an expression"
    )
    expect_error(
      unitizer_sect("mytest", matrix(1:9, nrow=3)),
      "Argument `expr` must be an expression"
    )
    expect_is(sect.1 <- unitizer_sect("mytest", expr.1), "unitizerSectionExpression")
    expect_identical(unitizer:::as.expression(sect.1), expr.1)
    expect_is(
      sect.2 <- unitizer_sect(
        "mytest", {1 + 1; b <- 5; matrix(integer(), nrow=b, ncol=b)}
      ), "unitizerSectionExpression"
    )
    expect_identical(sect.1, sect.2)
    expect_is(sect.3 <- unitizer_sect("mytest", expr.3), "unitizerSectionExpression")
    expect_identical(sect.1, sect.3)
    expect_is(sect.4 <- unitizer_sect("mytest", expr.4), "unitizerSectionExpression")
    expect_identical(sect.1, sect.4)
    expect_is(
      sect.5 <- unitizer_sect(
        "mytest", expression(1 + 1, b <- 5, matrix(integer(), nrow=b, ncol=b))
      ),
      "unitizerSectionExpression"
    )
    expect_identical(sect.1, sect.5)
    expect_is(
      sect.1 <- unitizer_sect("mytest", expr.1, compare=identical),
      "unitizerSectionExpression"
    )
    expect_warning(unitizer_sect('hello'), 'is empty')
  })
  # Run expressions with different comparison functions

  expr.1 <- expression(50 + runif(1) / 10 ^ 10, message("Hello There", runif(1)), cat("Hello there", runif(1)), stop("Yo", runif(1)))
  expr.2 <- expression(50 + runif(1) / 10 ^ 10, message("Hello There", runif(1)), cat("Hello there", runif(1)), stop("Yo", runif(1)))
  expr.3 <- expression(
    unitizer_sect(
      "change comp funs", compare=identical,
      {
        50 + runif(1) / 10 ^ 10
        message("Hello There", runif(1))
        cat("Hello there", runif(1))
        stop("Yo", runif(1))
  } ) )
  expr.4 <- expression(
    unitizer_sect(
      "change comp funs",
      compare=testFuns(
        value=identical, output=all.equal, message=all.equal,
        conditions=function(x, y) TRUE
      ),
      {
        50 + runif(1) / 10 ^ 10
        message("Hello There", runif(1))
        cat("Hello there", runif(1))
        stop("Yo", runif(1))
  } ) )
  my.unitizer <- new("unitizer", id=1, zero.env=new.env())
  capture.output(my.unitizer <- my.unitizer + expr.1)
  my.unitizer2 <- new("unitizer", id=2, zero.env=new.env())
  # make previous items into reference items
  capture.output(my.unitizer2 <- my.unitizer2 + my.unitizer@items.new)
  # now add back items to compare
  capture.output(my.unitizer2 <- my.unitizer2 + expr.2)
  my.unitizer3 <- new("unitizer", id=3, zero.env=new.env())
  # make previous items into reference items
  capture.output(my.unitizer3 <- my.unitizer3 + my.unitizer@items.new)
  # now add back items to compare
  capture.output(my.unitizer3 <- my.unitizer3 + expr.3)
  my.unitizer4 <- new("unitizer", id=4, zero.env=new.env())
  # make previous items into reference items
  capture.output(my.unitizer4 <- my.unitizer4 + my.unitizer@items.new)
  # now add back items to compare
  capture.output(my.unitizer4 <- my.unitizer4 + expr.4)

  test_that("Custom Comparison Functions",
    {
      expect_equal(
        structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = 4:5, .Dimnames = list(NULL, c("value", "conditions", "output", "message", "aborted"))),
        my.unitizer2@tests.result
      )
      expect_equal(
        structure(c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = 4:5, .Dimnames = list(NULL, c("value", "conditions", "output", "message", "aborted"))),
        my.unitizer3@tests.result
      )
      expect_equal(
        structure(c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE), .Dim = 4:5, .Dimnames = list(NULL, c("value", "conditions", "output", "message", "aborted"))),
        my.unitizer4@tests.result
      )
  } )
})
