if(!require(devtools)) stop("Package `devtools` is required for this demo.")
library(unitizer)

# In this script we will demonstrate the `unitize` workflow by running `unitize`
# against three different versions of our pseudo package `unitizer.fastlm`.
# Each version is intended to represent an incremental step in the development
# process.  There is a companion vignette to this demo that you can refer to for
# more details.

# We will start by creating a test file (normally this would go in
# "<packagedir>/tests/unitizer" but we will use a temporary file for the demo)

prompt_to_proceed()  # stop execution until you hit ENTER

cat(
  file=(test.file <- tempfile(fileext=".R")),
  "# Calls to `library` and assignments are not normally considered tests, so
  # you will not be prompted to review them

  library(unitizer.fastlm)
  x <- 1:100
  y <- x ^ 2
  res <- fastlm(x, y)

  res
  get_slope(res)
  get_rsq(res)

  fastlm(x, head(y))      # This should cause an error; press Y to add to store\n"
)

# Install `unitizer.fastlm` (`fastlm_dir` returns the location of the
# `unitizer.fastlm` sources embedded within `unitizer`)

prompt_to_proceed()

install(fastlm_dir(version=0))

# We are now ready to `unitize`.  Once `unitize` runs, press Y at the prompts to
# add the tests to the `unitizer` store (in this case, we know our functions are
# correct because they just wrap `base::lm`).
#
# Note you can always type H at the `unitizer` prompt to get contextual help.

prompt_to_proceed()

unitize(test.file)

# If all went well you added four tests to `unitizer`.

# We will now update our `fastlm` package to use the real computations instead
# of piggybacking of `lm` as our first version did.

prompt_to_proceed()

install(fastlm_dir(version=1))

# We can now re-run `unitize` to see if all went well.  In this case, two of
# our four tests will fail because we messed up the slope and intercept
# formulas.  This means we will have to re-implement our functions.
#
# In the meantime, type N at the `unitizer` prompt to reject the new values
# since they are wrong.  You can compare the correct reference value with the
# newly computed one by inspecting the `.ref` and `.new` objects

prompt_to_proceed()

unitize(test.file)

# We will now install the final fixed implementation of `unitizer.fastlm`:

prompt_to_proceed()

install(fastlm_dir(version=2))

# And re-run our tests; in theory they should all pass

prompt_to_proceed()

unitize(test.file)

# Clean-up
{
  remove.packages("unitizer.fastlm")
  unlink(test.file)
  unlink(sub(".R", ".unitizer", test.file), recursive=TRUE)
}
