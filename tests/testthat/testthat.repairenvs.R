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

  test_that(
    "messed up env ancestry repair works", {
    # Purposefully mess up the environments

    parent.env(my.unitizer2@items.ref[[2]]@env) <- baseenv()
    expect_warning(
      x <- unitizer:::healEnvs(my.unitizer2@items.ref, my.unitizer2),
      "Detected corrupted environment history"
    )
    expect_true(
      is(
        unitizer:::healEnvs(my.unitizer2@items.ref, my.unitizer2),
        "unitizerItems"
    ) )
    ref.anc <- unitizer:::env_ancestry(x@base.env)
    itm.anc <- unitizer:::env_ancestry(x[[1L]]@env)

    # Items should belong to base env for reference

    expect_identical(rev(ref.anc), head(rev(itm.anc), length(ref.anc)))
  } )

  test_that(
    "re-assigning to ignored environments handled properly", {
    items.picked <- my.unitizer@items.new[-3L]  # now `a + b` could try to re-assign to `a <- 54`, but that is same env as `a + b` b/c it is ignored
    items.heal <- unitizer:::healEnvs(items.picked, my.unitizer)
  } )




})
