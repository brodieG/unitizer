# Extra test file for internal tests; not for DEMO
# This one gives us the opportunity of removing a couple of tests and using
# sections

unitizer_sect("Basic Tests", {
  library(utzflm, lib.loc=getOption('unitizer.tmp.lib.loc'))
  x <- 1:10
  y <- x ^ 3
  res <- fastlm(x, y)

  get_slope(res)
})

unitizer_sect("Advanced Tests", {
  2 * get_slope(res) + get_intercept(res)
  get_rsq(res)
})

