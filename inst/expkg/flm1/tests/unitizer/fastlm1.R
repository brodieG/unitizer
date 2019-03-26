library(utzflm, lib.loc=getOption('unitizer.tmp.lib.loc'))
dat <- data.frame(x=1:100, y=(1:100) ^ 2)
res <- fastlm(dat$x, dat$y)

# Our fast computations do not produce the same results as our
# original tests so they fail.  If you need more detail than the
# provided diff you may use `.new`/`.NEW` or `.ref`/`.REF`.
#
# You should reject these tests by typing 'N' at the prompt since
# they are incorrect.

res

# This one is also incorrect; reject with 'N'

get_slope(res)

# Still correct

get_rsq(res)

# Still causes the same error (hence test passes)

fastlm(1:100, 1:10)
