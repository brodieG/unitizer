library(fastlm)

unitizer_sect(
  "Base Tests", 
  {
    x <- 1:100
    y <- ((1:100) / 10) ^ 2

    (res <- fastlm(x, y))   # parens to force treatment as test
    get_slope(res)
    get_intercept(res)
    get_rsq(res)
} )
unitizer_sect(
  "Test Error Handling", 
  {
    fastlm(x, head(y))
    fastlm(x, NULL)

    get_slope("hello")
    get_rsq(FALSE)
} )
