### unitizeR - Turn Arbitrary Code into Unit Tests

One of the more important purposes of unit tests is to ensure that previously tested functionality does not break as a result of changes to the code base.  `unitizer` seeks to simplify this aspect of unit testing by taking advantage of the following:

* code functionality is demonstrated by evaluating expressions and observing results
* unit tests verify that specific expressions continue to produce the same results
* in R, expressions are objects, results are objects, and both can be stored

`unitizer` evaluates arbitrary expressions and stores them along with the result of their evaluation.  Re-running the same `unitizer` then compares the new results for each expression to the previously stored results.  If there are any differences `unitizer` breaks out into an interactive mode to allow comparison of the old evaluation to the new one, and if necessary, update the tests.

### But Why?

There are some noteworthy benefits to this approach, in particular:

 1. tests for expressions that produce complex objects (e.g. `lm(y ~ x + z), DF)`) are trivial to write because the result of the expression becomes the unit test
 2. there is no coding overhead to writing the tests; what you typed in the console when you were developing functionality can be used as the unit tests (taken to the extreme, you could just use the history file from when you were informally testing functionality)
 3. (almost) all aspects of the evaluation can be captured and used as part of the tests; for example, any conditions signalled during evaluation will be captured for comparison to future evaluations

I first embarked on this project to address point 1.  In the absence of `unitizer`, testing that complex objects are created as expected requires running the test expression, deparsing the result, and then copy/pasting the ungainly string into a file with an `all.equal` or equivalent such statement.  While this isn't back-breaking, it does start taking on sisyphean overtones when you set out to build comprehensive tests, or god forbid, realize you need to slightly modify the output of your functions.

Points 2. and 3. are gravy.  For example, for 3., you can easily test that your code both fails, and that when it does, it produces the desired error message.

One aspect of evaluation that isn't currently captured but hopefully will be in the future are side effects such as plots.

### How Do I Use It?

There are several ways to use `unitizer`, but the simplest one is to create a file with your test expressions.  For example, if we were the author of the `lm` function, we might have:

    ### file: tests/unitizer/lm.R

    set.seed(1)
    df <- data.frame(x=1:100, y=1:100 + rnorm(100, 0, 20))

    lm(y ~ x, df)
    lm(y ~ x - 1, df)
    summary(lm(y ~ I(x ^ 2), df))

    # Cause errors

    lm(df, y ~ x)
    lm("garbage", NULL)

Then, we just run:

    unitize("tests/unitizer/lm.R")

The expressions will be evaluated, and results, errors, warnings, screen ouput, etc. will be stored.  If this is the first time you've run `unitize`, you will be prompted to review each test in an interactive environment to verify the expressions actually produce the desired outcome.  Once that's done, everything is stored in an `RDS` and voila, unit tests! 

Later on, you can just re-run:

    unitize("tests/unitizer/lm.R")

If everything is working as before then all the tests will pass and you can keep coding merrily.  If you introduced regressions, then `unitizer` will highlight the failing tests and allow you to review them interactively.  If you added more tests to the file since the last time you ran `unitize`, those tests will be added to the RDS subject to your review.