# Assignments and calls to `library` are not considered tests by
# `unitizer` so you will not need to review them

library(unitizer.fastlm)
x <- 1:100
y <- x ^ 2
res <- fastlm(x, y)

# Tests start here

res
get_slope(res)
get_rsq(res)

# The following test is expected to cause an error; press 'Y' to
# accept it so future checks can confirm the same error persists

fastlm(x, head(y))
