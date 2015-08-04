if(!require(devtools)) stop("Package `devtools` is required for this demo.")
library(unitizer)

# In this script we will demonstrate the `unitize` workflow by running `unitize`
# against three different versions of our pseudo package `unitizer.fastlm`.

# Each version of the package represents an incremental step in the development
# process.  There is a companion vignette to this demo that you can refer to for
# more details.

# Note: `unitizer.fastlm` implements fast computations for slope, intercept, and
# R^2 of a linear single variable regression.

prompt_to_proceed()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ SETUP START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The setup portion of this demo will execute steps would happen organically in
# the course of your package development, but must be automated here.  The
# steps are:
#
# - Create a file with R expressions to use as tests
# - Install the package that we are testing
#
# The test file would normally reside in "<packagedir>/tests/unitizer", but we
# will write it to a temporary file:

prompt_to_proceed()

# Write tests to temp file by `cat`ing a string to a `tempfile()`

cat(
  file=(.unitizer.test.file <- tempfile(fileext=".R")),
  "# Calls to `library` and assignments are not normally considered tests, so
  # you will not be prompted to review them

  library(unitizer.fastlm)
  x <- 1:100
  y <- x ^ 2
  res <- fastlm(x, y)

  res                     # first reviewable expression
  get_slope(res)
  get_rsq(res)

  fastlm(x, head(y))      # This should cause an error; press Y to add to store\n"
)
# Now `.unitizer.test.file` contains the address of an R file with the above
# expressions

prompt_to_proceed()

# Now install initial version of `unitizer.fastlm`

prompt_to_proceed()

install(fastlm_dir(version=0), quiet=TRUE)

# Note: `fastlm_dir` returns the location of the `unitizer.fastlm` sources
# embedded within `unitizer`. Normally you would just `install` your package.
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~ SETUP COMPLETE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prompt_to_proceed()

# We can now `unitize`.  After you press ENTER at the next prompt, `unitize`
# will launch an interactive test review environment.  Add the tests to the
# `unitizer` store by typing Y at the prompts.  Normally you would carefully
# review results to ensure they are as expected.
#
# You can always type H at the `unitizer` prompt to get contextual help.

prompt_to_proceed()

unitize(.unitizer.test.file)

# If all went well you successfully added four tests to `unitizer`.

prompt_to_proceed()

# Let's update our `unitizer.fastlm` package to use the real computations
# instead of piggybacking off of `stats::lm` as our first version did.

prompt_to_proceed()

install(fastlm_dir(version=1), quiet=TRUE)

# We can now re-run `unitize` to check for regressions.  In this case, two of
# the four tests will fail.  Since we know the original implementation was
# correct these new values should be rejected by typing 'N' at the prompt and
# exiting `unitizer`.

prompt_to_proceed()

unitize(.unitizer.test.file)

# We will now install the final fixed implementation of `unitizer.fastlm`.

prompt_to_proceed()

install(fastlm_dir(version=2), quiet=TRUE)

# And re-run our tests; in theory they should all pass.

prompt_to_proceed()

unitize(.unitizer.test.file)

# If you followed instructions all tests should have passed, which tells us that
# `unitizer.fastlm` is now producing the same values as it originally was when
# it was just a wrapper around `base::lm`.

# We can now comfortably make further modifications to `unitizer.fastlm` knowing
# that any regressions we introduce to existing functionality will be detected
# by `unitize`.

prompt_to_proceed()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEMO OVER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show `unitizer.fastlm` is actually faster for this simple task:

local({
  DF <- data.frame(x=1:1e5, y=(1:1e5) ^ 2)
  print(system.time(flm.val <- with(DF, unitizer.fastlm::fastlm(x, y))))
  print(system.time(lm.val <- c((lm.res <- lm(y ~ x, DF))$coefficients, summary(lm.res)$r.squared)))
  all.equal(lm.val, unclass(flm.val), check.attributes=FALSE)
})
# clean-up / remove temp files, etc
{
  remove.packages("unitizer.fastlm")
  unlink(.unitizer.test.file)
  unlink(sub(".R", ".unitizer", .unitizer.test.file), recursive=TRUE)
  rm(.unitizer.test.file)
}
