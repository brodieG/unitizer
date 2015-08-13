# Assignments and calls to `library` are not considered tests by
# `unitizer` so you will not need to review them

library(unitizer.fastlm)
res <- fastlm(1:100, (1:100) ^ 2)

# You will need to review the following four tests. Add them to the
# `unitizer` store by typing 'Y' at the prompt.  Type 'H' if you need
# help.

res
get_slope(res)
get_rsq(res)

# The following test is expected to cause an error; press 'Y' to
# accept it so future checks can confirm the same error persists

fastlm(1:100, 1:10)
