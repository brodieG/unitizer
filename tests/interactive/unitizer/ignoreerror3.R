# Testing case where an ignored call does not cause an error to show up even
# though it is the first time it is called

library(idontexist)  # this should show up!

unitizer_sect(
  "Base Tests",
  {
    x <- 1:100
    y <- ((1:100) / 10) ^ 2

    (res <- idontexist_fun(x, y))   # parens to force treatment as test
} )
