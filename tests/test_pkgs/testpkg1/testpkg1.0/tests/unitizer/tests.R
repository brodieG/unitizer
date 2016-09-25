library(testpkg1)

unitizer_sect("fun1", {
  # Fun 1 simple usage
  fun1(1:10)
  fun1(letters)
  fun1(letters, TRUE, TRUE)
  fun1(1:10 + .1)
  fun1(1:10 + .1, warn=TRUE)
})
unitizer_sect("fun2 & 3", {
  # fun 2 simple usage
  fun2(1:10)
  fun3(1:10)  # and fun 3
})
