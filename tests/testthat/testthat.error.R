context("Error")

local({
  test_that("Test Error Diffs", {
    diffs <- new(
      'unitizerItemTestsErrorsDiffs',
      value=new(
        'unitizerItemTestsErrorsDiff', txt='value', err=TRUE,
        diff=diffobj::diffChr(1, 2)
    ) )
    expect_equal(diffs$value@diff@target, 1)
    expect_equal(diffs$value@diff@current, 2)
    expect_error(diffs$values, "must be one of")
    expect_error(diffs[[NA]], "must be character")
  })
  err <- new(
    "unitizerItemTestsErrors",
    value=new(
      "unitizerItemTestError", compare.err=TRUE, value=c('compare', 'error')
    )
  )
  err.capt <- unitizer:::capture_output(show(err))

  test_that("Show Test Error", {
    # Create an error that triggers non-standard error diff display (comparison
    # error with more than one line of output)

    expect_equal(
      err.capt$message,
      c("| Unable to compare value: ", "| - compare", "| - error",  "")
    )
    expect_is(unitizer:::as.Diffs(err)@value, "unitizerItemTestsErrorsDiff")
  })

})
