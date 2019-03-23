library(unitizer)
unitizer_check_demo_state()

# In this script we demonstrate the `unitizer` workflow by installing and
# testing three versions of `unitizer.fastlm`, a package that implements faster
# computations of slope, intercept and R^2 for a single variable regression.
#
# The package versions are:
#
# 1. v0.1.0: a slow version that is known to produce the correct results
# 2. v0.1.1: initial (flawed) attempt at optimizing our functions
# 2. v0.1.2: fixes to regressions introduced in v0.1.1
#
# See Also: `?unitizer::demo`, `?unitize`, `vignette("unitizer")`

`[Press ENTER to Continue]`()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(.unitizer.fastlm <- copy_fastlm_to_tmpdir())    # package directory
list.files(.unitizer.fastlm)                     # contains our sources
dir.create((lib.dir <- tempfile()))
options(unitizer.tmp.lib.loc=lib.dir)

install.packages(
  .unitizer.fastlm, repos=NULL, type="src", quiet=TRUE, lib=lib.dir
)

# And in our sources is the test file, which we will `unitize`:

.unitizer.test.file <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm1.R")
show_file(.unitizer.test.file)

# Here we copied `untizer.fastlm` sources to a temporary "package directory"
# and installed it. The test file contained therein is shown for reference.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# After you press ENTER at the prompt, `unitize` will launch.  You should accept
# the tests since we know the first package version is correct, though slow.

`[Press ENTER to Continue]`()

unitize(.unitizer.test.file)

# If all went well you added four tests to `unitizer`.
#
# We will now update `unitizer.fastlm` package to use the fast computations. We
# do this with `update_fastlm`; in real life you would be updating your source
# code at this point.  After the update/re-install, we re-run `unitize`:

`[Press ENTER to Continue]`()

update_fastlm(.unitizer.fastlm, version="0.1.1")
install.packages(
  .unitizer.fastlm, repos=NULL, type="src", quiet=TRUE, lib=lib.dir
)

unitize(.unitizer.test.file)

# Let's fix the regressions we introduced and re-run `unitize`:

`[Press ENTER to Continue]`()

update_fastlm(.unitizer.fastlm, version="0.1.2")
install.packages(
  .unitizer.fastlm, repos=NULL, type="src", quiet=TRUE, lib=lib.dir
)

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
    fastlm=system.time(with(DF, utzflm::fastlm(x, y))),
    lm=system.time(c((lm.res <- lm(y ~ x, DF))$coefficients, summary(lm.res)$r.squared))
  )[, 1:3]
})
unitizer_cleanup_demo()
