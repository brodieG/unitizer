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
  test_that("Show Test Error", {
    expect_is(unitizer:::as.Diffs(err)@value, "unitizerItemTestsErrorsDiff")
  })

})
