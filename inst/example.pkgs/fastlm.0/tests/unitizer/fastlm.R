# Assignments and calls to `library` are not considered tests by
# `unitizer` so you will not need to review them

library(unitizer.fastlm)
x <- 1:100
y <- x ^ 2
res <- fastlm(x, y)

# The following four expressions are the tests

res
get_slope(res)
get_rsq(res)

fastlm(x, head(y))  # Causes an error; press Y to add to store
