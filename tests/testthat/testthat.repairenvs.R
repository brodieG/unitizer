library(unitizer)
library(testthat)

local({
  exps <- expression(
    1 + 1,
    a <- 54,
    b <- 38,
    a + b,
    e <- 5 * a,
    a ^ 2,
    f <- e * a,
    matrix(rep(f, 20))
  )
  my.unitizer <- new("unitizer", id=1, zero.env=new.env())
  my.unitizer <- my.unitizer + exps   # add ref.exps as new items
  my.unitizer2 <- new("unitizer", id=2, zero.env=new.env())
  my.unitizer2 <- my.unitizer2 + my.unitizer@items.new    # now convert them to reference items

  # Purposefully mess up the environments

  parent.env(my.unitizer2@items.ref[[2]]@env) <- baseenv()
  expect_warning(
    x <- unitizer:::healEnvs(my.unitizer2@items.ref, my.unitizer2),
    "Detected corrupted environment history"
  )
  expect_true(
    is(unitizer:::healEnvs(my.unitizer2@items.ref, my.unitizer2),
    "unitizerItems")
  )
})
