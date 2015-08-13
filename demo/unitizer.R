library(unitizer)
unitizer_check_demo_state()

# In this script we will demonstrate the `unitizer` workflow by simulating the
# development process of the `unitizer.fastlm` package, a pseudo package that
# implements faster computations of slope, intercept and R^2 for a single
# variable linear regression.
#
# We will install three different versions of the package, and run our tests
# againt each:
#
# 1. v0.1.0: a known working copy based on `base::lm`
# 2. v0.1.1: initial (flawed) attempt at optimization of existing functionality
# 2. v0.1.2: fixes to regressions introduced in v0.1.1
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

# Install first version of packate

devtools::install(.unitizer.fastlm, quiet=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[Press ENTER to Continue]`()

# After you press ENTER at the next prompt, `unitize` will launch.

`[Press ENTER to Continue]`()

unitize(.unitizer.test.file)

# If all went well you added four tests to `unitizer`.
#
# We will now update `unitizer.fastlm` package to use the fast computations
# instead of piggybacking off of `stats::lm` as our first version did.  We
# do this with `update_fastlm`; in real life you would be updating your source
# code at this point.  After the update/re-install, we re-run `unitize`:

`[Press ENTER to Continue]`()

update_fastlm(.unitizer.fastlm, version="0.1.1")
devtools::install(.unitizer.fastlm, quiet=TRUE)

unitize(.unitizer.test.file)

# Let's fix the regressions we introduced and re-run `unitize`:

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
