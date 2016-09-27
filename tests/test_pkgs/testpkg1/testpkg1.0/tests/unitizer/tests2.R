library(testpkg1)

unitizer_sect("fun1-2", {
  # Add some numbers
  fun1(1:10) + 2
  fun1(1:10 + .1, warn=TRUE) + 3
})
unitizer_sect("fun2-2", {
  fun2(1:10) + fun2(1:10)
  fun3(1:10) + 4          # Add Four
})
