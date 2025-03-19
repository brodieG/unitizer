# Assignments and calls to `library` are not considered tests by
# `unitizer` so you will not need to review them

library(utzflm, lib.loc=getOption('unitizer.tmp.lib.loc'))
dat <- data.frame(x=1:100, y=(1:100) ^ 2)
res <- fastlm(dat$x, dat$y)

# The `unitizer>` prompt is like the standard R prompt. You may
# enter expressions such as `lm(y ~ x, dat)$coefficients`, or
# `str(res)`.
#
# Once you are done reviewing, you need to tell `unitizer` you
# accept the test by typing 'Y' at the prompt.  Enter 'H' for help.

res
