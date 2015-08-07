library(unitizer)
unitizer_check_demo_state()

# In this script we will demonstrate the `unitizer` workflow by running
# `unitize` against three different versions of our pseudo package
# `unitizer.fastlm`:
#
# 1. v0.1.0: a known working copy based on `base::lm`
# 2. v0.1.1: initial (flawed) attempt at optimization of existing functionality
# 2. v0.1.2: fixes to regressions introduced in v0.1.1
#
# `unitizer.fastlm` implements faster computations of slope, intercept and R^2
# for a single variable linear regression.
#
# See Also: `?unitizer::demo`, `?unitize`, `?vignette("unitizer")`

`[Press ENTER to Continue]`()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Simulate package development by creating a temporary "package directory" and
# copying the `unitizer.fastlm` sources there-in:

(.unitizer.fastlm <- copy_fastlm_to_tmpdir())  # package directory
list.files(.unitizer.fastlm)                   # contains our sources

# And in our sources is the test file, which we will `unitize`:

(.unitizer.test.file <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm.R"))
show_file(.unitizer.test.file)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[Press ENTER to Continue]`()

# Install the package to get started

devtools::install(.unitizer.fastlm, quiet=TRUE)

# After you press ENTER at the next prompt, `unitize` will launch.  Type H at
# the `unitizer>` prompt for contextual help, or enter any valid R expression
# to evaluate it in the environment of the test.
#
# Once you are done exploring, press 'Y' at each prompt to accept the tests
# since we know they are correct

`[Press ENTER to Continue]`()

unitize(.unitizer.test.file)

# If all went well you added four tests to `unitizer`.
#
# Let's update our `unitizer.fastlm` package to use the real computations
# instead of piggybacking off of `stats::lm` as our first version did.  We
# do this with `update_fastlm`; in real life you would be updating your source
# code at this point.

`[Press ENTER to Continue]`()

update_fastlm(.unitizer.fastlm, version="0.1.1")
devtools::install(.unitizer.fastlm, quiet=TRUE)

# We can now re-run `unitize` to check for regressions.  In this case, two of
# the four tests will fail.  Try typing `str(.new)` or `str(.ref)` at
# `unitizer>` prompt.  Once you are done reviewing, type 'N' to reject the
# failing tests since we know they are incorrect.

`[Press ENTER to Continue]`()

unitize(.unitizer.test.file)

# We will now install and run the final implementation of `unitizer.fastlm`

`[Press ENTER to Continue]`()

update_fastlm(.unitizer.fastlm, version="0.1.2")
devtools::install(.unitizer.fastlm, quiet=TRUE)

unitize(.unitizer.test.file)

# If you followed instructions all tests should have passed, which tells us that
# `unitizer.fastlm` is now producing the same values as it originally was when
# it was just a wrapper around `base::lm`.

`[Press ENTER to Continue]`()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEMO OVER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show `unitizer.fastlm` is actually faster for this simple task:

local({
  DF <- data.frame(x=1:1e5, y=(1:1e5) ^ 2)
  rbind(
    fastlm=system.time(with(DF, unitizer.fastlm::fastlm(x, y))),
    lm=system.time(c((lm.res <- lm(y ~ x, DF))$coefficients, summary(lm.res)$r.squared))
  )[, 1:3]
})
unitizer_cleanup_demo()
