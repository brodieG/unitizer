library(microbenchmark)

unitizer_sect("Simple Section", {
  mx <- matrix(1:20, nrow=4)
  t(mx)
  mx %*% t(mx)
} )
print("off section")
unitizer_sect("Nesting", {
  set.seed(20)
  df <- data.frame(a=1:20, b=(1:20) / 2 + rnorm(20, 0, 2))
  lm(b ~ a, df)
  5 + runif(1, 0, 1/10^32)
  unitizer_sect("Inner", compare=identical, {
    5 + runif(1, 0, 1/10^32)
    t(mx) %*% mx
    stop("wow")
  } )
  paste0(letters, 1:26)
  end.on.assign <- "yoyoyo"
} )
unitizer_sect("Nesting.2", {
  arr <- array(sample(1:12), dim=c(2, 2, 3))
  apply(arr, 1:2, rev)
  set.seed(NULL)
  unitizer_sect(compare=sample, expr={
#    20
    warning("yo")
#    apply(arr, 1:2, sum)
#    stop("Random Error Message 1: ", sample(1:10000, 1))
    # unitizer_sect("double.inner",
    #   {
    #     20
    #     warning("yo")
    #     apply(arr, 1:2, sum)
    #     stop("Random Error Message 2: ", sample(1:10000, 1))
    #   },
    #   compare=unitizerItemTestsFuns(output=all.equal, message=all.equal)
    # )
#    5 + 3 * 1:10
  } )
  paste0(letters, 1:26)
  end.on.assign <- "yoyoyo"
} )
end.assign.two <- "blergh"

