# Assignments and calls to `library` are not considered tests by
# `unitizer` so you will not need to review them

library(unitizer.fastlm)
dat <- data.frame(x=1:100, y=(1:100) ^ 2)
res <- fastlm(dat$x, dat$y)

# Our first attempt at fast slope and intercept calculations is
# wrong and the values no longer match what we recorded in the
# first run.  This causes the following two tests to fail.
#
# `unitizer` provides a simple diff view to show what went wrong,
# but you may review the new and reference objects with `.new` and
# `.ref` respectively. You should reject these tests by typing 'N'
# at the prompt since they are incorrect.

res

# This one is also incorrect; reject with 'N'

get_slope(res)

# Still correct

get_rsq(res)

# Still causes the same error (hence test passes)

fastlm(1:100, 1:10)
