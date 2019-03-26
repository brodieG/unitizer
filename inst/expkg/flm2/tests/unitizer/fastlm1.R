# Assignments and calls to `library` are not considered tests by
# `unitizer` so you will not need to review them

library(utzflm, lib.loc=getOption('unitizer.tmp.lib.loc'))
res <- fastlm(1:100, (1:100) ^ 2)

# Our new implementation of slope and intercept calculations is not correct,
# which is why we are seeing these tests as failed.  Type 'N' at the prompts
# since we do not want to overwrite our previously correct tests with these
# incorrect ones

res
get_slope(res)

# Still correct

get_rsq(res)

# Still causes the same error (hence test passes)

fastlm(1:100, 1:10)
