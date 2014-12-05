library(unitizer.fastlm)

unitizer_sect("Create Fastlm Object", {
  x <- 1:100
  y <- x ^ 2
  res <- fastlm(x, y)
  res
})
unitizer_sect("Accessor Functions", {
  get_slope(res)
  get_rsq(res)
  get_intercept(res)
})
unitizer_sect("Error Handling", {
  fastlm(x, head(y))
  get_rsq("cabbage")
})
