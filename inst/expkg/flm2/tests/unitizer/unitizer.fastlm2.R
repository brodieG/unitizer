# Extra test file for internal tests; not for DEMO
# This one gives us the opportunity of removing a couple of tests and using
# sections

unitizer_sect("Basic Tests", {
  library(utzflm, lib.loc=getOption('unitizer.tmp.lib.loc'))
  x <- 1:10
  y <- x ^ 2  # modified
  res <- fastlm(x, y)

  get_slope(res)
})

unitizer_sect("Advanced Tests", {
  get_rsq(res)
})

