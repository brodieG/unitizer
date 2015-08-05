if(!require(devtools)) stop("Package `devtools` is required for this demo.")
library(unitizer)

# In this script we will demonstrate the `unitize` workflow by running `unitize`
# against three different versions of our pseudo package `unitizer.fastlm`.

# Each version of the package represents an incremental step in the development
# process.  There is a companion vignette to this demo that you can refer to for
# more details.

# Note: `unitizer.fastlm` implements fast computations for slope, intercept, and
# R^2 of a linear single variable regression.

`[Press ENTER to Continue]`()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ SETUP START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We start by placing our package sources in a temporary directory and use it as
# our working directory (NOTE: if you interrupt demo you will need to reset the
# working directory yourself).

`[Press ENTER to Continue]`()

.unitizer.dir.pkg <- copy_fastlm_to_tmpdir()
.unitizer.old.dir <- setwd(.unitizer.dir.pkg)

# The sources contain our test file which we will `unitize`.

show_file("tests/unitizer/fastlm.R")  # this is our test file

# Install the package to get started

devtools::install(quiet=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ SETUP COMPLETE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We can now `unitize`.  After you press ENTER at the next prompt, `unitize`
# will launch.  After `unitizer` launches, accept tests by typing Y at the
# prompts.  Normally you would carefully review results to ensure they are as
# expected, but in this case we know the initial implementation is correct and
# just needs to be optimized.
#
# You can always type H at the `unitizer` prompt to get contextual help.

`[Press ENTER to Continue]`()

unitize("tests/unitizer/fastlm.R")

# If all went well you successfully added four tests to `unitizer`.

`[Press ENTER to Continue]`()

# Let's update our `unitizer.fastlm` package to use the real computations
# instead of piggybacking off of `stats::lm` as our first version did.  We
# do this with `update_fastlm`; in real life you would be updating your source
# code at this point.

`[Press ENTER to Continue]`()

update_fastlm(.unitizer.dir.pkg, version="0.1.1")
devtools::install(quiet=TRUE)

# We can now re-run `unitize` to check for regressions.  In this case, two of
# the four tests will fail.  Since we know the original implementation was
# correct these new values should be rejected by typing 'N' at the prompt and
# exiting `unitizer`.

`[Press ENTER to Continue]`()

unitize("tests/unitizer/fastlm.R")

# We will now install the final fixed implementation of `unitizer.fastlm`.

`[Press ENTER to Continue]`()

update_fastlm(.unitizer.dir.pkg, version="0.1.2")
devtools::install(quiet=TRUE)

# And re-run our tests; in theory they should all pass.

`[Press ENTER to Continue]`()

unitize("tests/unitizer/fastlm.R")

# If you followed instructions all tests should have passed, which tells us that
# `unitizer.fastlm` is now producing the same values as it originally was when
# it was just a wrapper around `base::lm`.

# We can now comfortably make further modifications to `unitizer.fastlm` knowing
# that any regressions we introduce to existing functionality will be detected
# by `unitize`.

`[Press ENTER to Continue]`()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEMO OVER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show `unitizer.fastlm` is actually faster for this simple task:

local({
  DF <- data.frame(x=1:1e5, y=(1:1e5) ^ 2)
  print(system.time(flm.val <- with(DF, unitizer.fastlm::fastlm(x, y))))
  print(system.time(lm.val <- c((lm.res <- lm(y ~ x, DF))$coefficients, summary(lm.res)$r.squared)))
  all.equal(lm.val, unclass(flm.val), check.attributes=FALSE)
})
# Clean-up / remove temp files, etc
{
  remove.packages("unitizer.fastlm")
  unlink(.unitizer.dir.pkg, recursive=TRUE)
  setwd(.unitizer.old.dir)
  rm(.unitizer.old.dir, .unitizer.dir.pkg)
}
