library(unitizer)
library(testthat)
context("Item")
local({
  old.opt <- options(unitizer.color=FALSE)
  on.exit(options(old.opt))

  if(!identical(basename(getwd()), "testthat"))
    stop("Working dir does not appear to be /testthat, is ", getwd())

  rdsf <- function(x)
    file.path(getwd(), "helper", "item", sprintf("%s.rds", x))

  # These tests are intended to cover all the functions/classes/methods in:
  # - item.R
  # - item.sub.R
  # - test_eval.R     # indirectly
  # - heal.R
  # - unitizer.R
  # Basically everything that can be tested non-interactively

  local( {

    # Helper funs

    callDep <- function(x) paste0(deparse(x@call, width=500), collapse="")
    lsObjs <- function(x) paste0(x@ls$name, x@ls$status, collapse=", ")
    lsStat <- function(x) x@ls$status
    lsInv <- function(x) isTRUE(attr(x@ls, "invalid"))

    # Get started

    new.exps <- expression(
      1 + 1,
      a <- 54,     # keep
      b <- 38,     # keep
      a + b,
      e <- 5 * a,  # keep
      a ^ 2,       # Keep
      f <- e * a,
      matrix(rep(f, 20))  # keep
    )
    ref.exps <- expression(
      1 + 1,
      a <- 54,
      b <- 38,
      a + b,
      e <- 5 * a,
      e ^ 3
    )
    Sys.sleep(.2)
    my.unitizer <- new("unitizer", id=1, zero.env=new.env())
    # add ref.exps as new items
    capture.output(my.unitizer <- my.unitizer + ref.exps)
    my.unitizer2 <- new("unitizer", id=2, zero.env=new.env())
    # now convert them to reference items
    capture.output(my.unitizer2 <- my.unitizer2 + my.unitizer@items.new)
    # now test against new.exps
    capture.output(my.unitizer2 <- my.unitizer2 + new.exps)

    test_that("item funs", {
      item <- my.unitizer@items.new[[1L]]
      expect_equal(unitizer:::itemType(item), "new")
      expect_error(unitizer:::itemType(item) <- "asdfasd", "must be in")
      unitizer:::itemType(item) <- "reference"
      expect_equal(unitizer:::itemType(item), "reference")
      expect_error(
        unitizer:::itemsType(my.unitizer@items.new) <- as.character(1:1000),
        "have same length"
      )
      expect_error(item$booboo, "must be in")
    })
    test_that("unitizer creation worked as expected", {
      expect_true(validObject(my.unitizer, complete=TRUE))
      expect_equal_to_reference(
        capture.output(show(my.unitizer@items.new[[1L]])),
        rdsf(100)
      )
      expect_equal(length(my.unitizer2), length(new.exps))
      expect_equal(length(my.unitizer2@items.new), length(new.exps))
      expect_equal(length(my.unitizer2@items.ref), length(ref.exps))
      expect_equal(
        as.expression(
          lapply(
            unitizer:::as.list(my.unitizer2@items.new), slot, "call"
        ) ),
        new.exps
      )
      expect_equal(
        as.expression(
          lapply(
            unitizer:::as.list(my.unitizer2@items.ref), slot, "call"
        ) ),
        ref.exps
      )
      vals <- lapply(
        unitizer:::as.list(my.unitizer2@items.new), function(x) x@data@value[[1L]]
      )
      vals.ign <- unitizer:::ignored(my.unitizer2@items.new)
      expect_equal(vals[!vals.ign], lapply(new.exps, eval)[!vals.ign])
      expect_true(all(vapply(vals[vals.ign], is, logical(1L), "unitizerDummy")))

      vals <- lapply(
        unitizer:::as.list(my.unitizer2@items.ref), function(x) x@data@value[[1L]]
      )
      vals.ign <- unitizer:::ignored(my.unitizer2@items.ref)
      expect_equal(vals[!vals.ign], lapply(ref.exps, eval)[!vals.ign])
      expect_true(all(vapply(vals[vals.ign], is, logical(1L), "unitizerDummy")))

      expect_equal(my.unitizer2@items.new.map, c(1L, 2L, 3L, 4L, 5L, NA, NA, NA))
      expect_equal(my.unitizer2@items.ref.map, c(1L, 2L, 3L, 4L, 5L, NA))
      expect_equal(
        my.unitizer2@tests.fail,
        c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
      )
      expect_equal(
        my.unitizer2@tests.status,
        structure(
          c(1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L),
          .Label = c("Pass", "Fail", "New", "Deleted", "Error"), class = "factor"
        )
      )
      expect_equal(my.unitizer2@section.map, c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L))
      expect_equal(unitizer:::ignored(my.unitizer2@items.new), c(F, T, T, F, T, F, T, F))
      expect_equal(unitizer:::ignored(my.unitizer2@items.ref), c(F, T, T, F, T, F))
    } )
    test_that("Size Measurement works", {
      # Used to produce warnings because the same base.env was used for every
      # unitizer because it was created on package load as part of the S4 class
      # definition instead of in "initialize", so any time we instantiated more
      # than one object they all shared the same environment, causing issues with
      # saveRDS

      x <- unitizer:::sizeUntz(my.unitizer2)
      expect_true(is.matrix(x) && is.numeric(x))
      expect_identical(colnames(x), c("size", "rds"))
    })
    test_that("Environment healing works", {
      items.mixed <- my.unitizer2@items.new[4:5] +
        my.unitizer2@items.ref[[1]] + my.unitizer2@items.new[c(2, 6, 8)]
      items.sorted <- unitizer:::healEnvs(items.mixed, my.unitizer2)
      env.anc <- lapply(
        unitizer:::as.list(items.sorted),
        function(x) rev(unitizer:::env_ancestry(x@env, my.unitizer2@base.env))
      )
      max.len <- max(vapply(env.anc, length, 1L))
      env.anc.2 <- lapply(env.anc, function(x) {length(x) <- max.len; x})
      env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors=FALSE)
      # Here only the first item is reference, all others

      expect_equal(length(unique(unlist(env.anc.df[2, ]))), 1L)
      expect_true(
        all(
          apply(env.anc.df[-(1:2), -1], 1,
          function(x) length(unique(Filter(Negate(is.na), x)))) == 1L
      ) )
      expect_equal(  # First item is reference, all others are new
        unitizer:::itemsType(items.sorted), c("reference", rep("new", 7L))
      )
      expect_equal(  # Expected order of ids
        vapply(unitizer:::as.list(items.sorted), function(x) x@id, integer(1L)),
        1:8
      )
      expect_equal(
        lapply(unitizer:::as.list(items.sorted), function(x) x@ls$names),
        list(character(0), character(0), character(0), c("a", "b"), character(0), c("a", "b", "e"), character(0), c("a", "b", "e", "f"))
      )
      expect_equal(unique(unlist(lapply(unitizer:::as.list(items.sorted), function(x) x@ls$status))), "")
    } )
    # Tests with conditions

    my_fun <- function() {warning("hello"); 25}
    ref.exps1a <- expression(stop("boom"), my_fun())
    my.unitizer1a <- new("unitizer", id=100, zero.env=new.env())
    # add ref.exps as new items
    capture.output(my.unitizer1a <- my.unitizer1a + ref.exps1a)

    test_that("Items with conditions", {
      expect_equal_to_reference(
        capture.output(show(my.unitizer1a@items.new[[1L]])), rdsf(200)
      )
      expect_equal_to_reference(
        capture.output(show(my.unitizer1a@items.new[[2L]])), rdsf(300)
      )
      expect_equal_to_reference(
        capture.output(show(my.unitizer1a@items.new[[1L]]@data@conditions)),
        rdsf(400)
      )
    })
    new.exps2 <- expression(
      1 + 1,                #  1 *    Stars highlight items we are selecting, but keep in mind that
      a <- 54,              #  2      unitizer only cares about non ignored tests, and that the selection
      b <- runif(5),        #  3      status of ignored test has nothing to do with what we end up
      howdy <- "yowser",    #  4 *    with wrt to ignored tests
      a + b,                #  5 *
      e <- 5 * a,           #  6
      a ^ 2,                #  7
      f <- e * a,           #  8
      matrix(rep(f, 20))    #  9 *
    )
    ref.exps2 <- expression(
      1 + 1,                   #  1
      a <- 54,                 #  2
      b <- runif(5),           #  3 *
      25 + 3,                  #  4
      q <- b ^ 2 / a,          #  5 *
      a + b,                   #  6
      z <- w <- list(1, 2, 3), #  7
      Reduce(`+`, z),          #  8 *                # Doesn't exist, should connect back to `a + b`
      e <- 5 * a,              #  9
      e ^ 3,                   # 10 *
      e * a                    # 11 *
    )
    # Note that healEnvs modifies objects that contain environments, and as such
    # you won't get the same result if you run this function twice, so don't be
    # surprised if tests fail in those circumstances

    my.unitizer3 <- new("unitizer", id=1, zero.env=new.env())
    # add ref.exps as new items
    capture.output(my.unitizer3 <- my.unitizer3 + ref.exps2)
    my.unitizer4 <- new("unitizer", id=2, zero.env=new.env())
    # now convert them to reference items
    capture.output(my.unitizer4 <- my.unitizer4 + my.unitizer3@items.new)
    # now test against new.exps
    capture.output(my.unitizer4 <- my.unitizer4 + new.exps2)
    capture.output(
      items.mixed2 <- my.unitizer4@items.ref[c(8, 10, 3, 5, 11)] +
        my.unitizer4@items.new[c(1, 4, 5, 9)]
    )
    items.sorted2 <- unitizer:::healEnvs(items.mixed2, my.unitizer4)

    test_that("Environment healing works 2", {
      env.anc <- lapply(
        unitizer:::as.list(items.sorted2),
        function(x) rev(unitizer:::env_ancestry(x@env, my.unitizer4@base.env))
      )
      max.len <- max(vapply(env.anc, length, 1L))
      env.anc.2 <- lapply(env.anc, function(x) {length(x) <- max.len; x})
      unname(env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors=FALSE))

      expect_equal(length(unique(unlist(env.anc.df[1, ]))), 1L, info="all tests should have same base.env")
      expect_identical(env.anc.df[1, 1], unitizer:::env_name(my.unitizer4@base.env), info="base.env should be unitizer env")
      expect_equal(length(unique(unlist(env.anc.df[2, ]))), 1L, info="all tests should also have another sub base.env")
      expect_identical(env.anc.df[2, 1], unitizer:::env_name(my.unitizer4@items.ref@base.env), info="and it should be the items.ref here")

      items <- items.sorted2
      items.lst <- unitizer:::as.list(items)

      heal.info <- cbind(
        type=unitizer:::itemsType(items),
        ignored=unitizer:::ignored(items),
        id=vapply(items.lst, slot, 1L, "id"),
        call=vapply(items.lst, callDep, ""),
        ls=vapply(items.lst, lsObjs, ""),
        ls.invalid=vapply(items.lst, lsInv, TRUE)
      )
      expect_equal(
        info="new items should all have normal status",
        unique(unlist(lapply(items.lst[unitizer:::itemsType(items) == "new"], lsStat))), ""
      )
      expect_equal(
        info="Reference tests should have no ls data",
        unique(vapply(items.lst[unitizer:::ignored(items)], lsObjs, "")), ""
      )
      expect_true(
        info="Reference tests should have invalid ls",
        all(vapply(items.lst[unitizer:::ignored(items)], lsInv, logical(1L)))
      )
    } )
    my.unitizer5 <- new("unitizer", id=2, zero.env=new.env())
    # now add back our composite elements as references
    capture.output(my.unitizer5 <- my.unitizer5 + items.sorted2)
    capture.output(my.unitizer5 <- my.unitizer5 + new.exps2)      # and new items

    test_that("ls works", {
      # This is an ignored test, so there will be some problems

      env.val <- new.env(parent=my.unitizer5@items.new[[3]]@env)
      env.eval <- new.env(parent=env.val)
      assign(".NEW", my.unitizer5@items.new[[3]], env.val)
      assign(".new", my.unitizer5@items.new[[3]]@data@value[[1L]], env.val)
      assign(".REF", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[3]]]], env.val)
      assign(
        ".ref",
        my.unitizer5@items.ref[[
          my.unitizer5@items.new.map[[3]]
        ]]@data@value[[1L]],
        env.val
      )
      expect_warning(
        ls.res <- evalq(unitizer:::unitizer_ls(), env.eval),
        "The ls output for `.ref` is invalid"
      )
      # Reference tests won't show up since they were nuked by `healEnvs`

      expect_equal_to_reference(ls.res, rdsf(500))

      # These are normal tests so should work

      env.val <- new.env(parent=my.unitizer5@items.new[[9]]@env)
      env.eval <- new.env(parent=env.val)
      assign(".NEW", my.unitizer5@items.new[[9]], env.val)
      assign(".new", my.unitizer5@items.new[[9]]@data@value[[1L]], env.val)
      assign(
        ".REF", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[9]]]], env.val
      )
      assign(
        ".ref",
        my.unitizer5@items.ref[[my.unitizer5@items.new.map[[9]]]]@data@value[[1L]],
        env.val
      )
      expect_equal_to_reference(
        evalq(unitizer:::unitizer_ls(), env.eval), rdsf(600)
      )
      expect_equal_to_reference(
        capture.output(print(evalq(unitizer:::unitizer_ls(), env.eval))),
        rdsf(700)
      )
    } )
    # Test that reference tests moving around doesn't cause major issues

    new.exps6 <- expression(
      1 + 1,                #  1   *
      a <- 54,              #  2
      b <- runif(5),        #  3
      howdy <- "yowser",    #  4
      a + b,                #  5
      e <- 5 * a,           #  6
      a ^ 2,                #  7   *
      f <- 25,              #  8   *
      matrix(rep(f, 20))    #  9
    )
    ref.exps6 <- expression(
      1 + 1,                #  1
      a <- 54,              #  2
      f <- 25,              #  3
      matrix(rep(f, 20)),   #  4   *
      b <- runif(5),        #  5
      boomboom <- "boo",    #  6
      a + b,                #  7   *
      a + b + f,            #  8
      e <- 5 * a,           #  9
      a ^ 2                 # 10
    )
    my.unitizer10 <- new("unitizer", id=1, zero.env=new.env())
    # add ref.exps as new items
    capture.output(my.unitizer10 <- my.unitizer10 + ref.exps6)
    my.unitizer11 <- new("unitizer", id=2, zero.env=new.env())
    # now convert them to reference items
    capture.output(my.unitizer11 <- my.unitizer11 + my.unitizer10@items.new)
    # now test against new.exps
    capture.output(my.unitizer11 <- my.unitizer11 + new.exps6)
    items.mixed3 <- my.unitizer11@items.ref[c(4, 7)] +
      my.unitizer11@items.new[c(1, 7, 8)]
    items.sorted3 <- unitizer:::healEnvs(items.mixed3, my.unitizer11)

    # Main difference to previous versions is that we're testing that moving the
    # order of tests around between ref and new still works

    test_that("Environment Healing Works #3", {

      # Both reference tests get appended to item #1, which means among other things
      # that for the second refernce test, the `a` object is absent (but `b` is
      # present because it gets sucked in by virtue of being an ignored test just
      # ahead of it)

      items <- items.sorted3
      items.lst <- unitizer:::as.list(items)
      expect_equal(
        cbind(
          type=unitizer:::itemsType(items),
          ignored=unitizer:::ignored(items),
          id=vapply(items.lst, slot, 1L, "id"),
          call=vapply(items.lst, callDep, ""),
          ls=vapply(items.lst, lsObjs, ""),
          ls.invalid=vapply(items.lst, lsInv, TRUE)
        ),
        structure(c("new", "reference", "reference", "reference", "reference", "reference", "reference", "new", "new", "FALSE", "TRUE", "TRUE", "FALSE", "TRUE", "TRUE", "FALSE", "TRUE", "FALSE", "1", "2", "3", "4", "5", "6", "7", "6", "7", "1 + 1", "a <- 54", "f <- 25", "matrix(rep(f, 20))", "b <- runif(5)", "boomboom <- \"boo\"", "a + b", "e <- 5 * a", "a^2", "", "", "", "a, f", "", "", "a*, b, boomboom, f*", "", "a, b, e, howdy", "FALSE", "TRUE", "TRUE", "FALSE", "TRUE", "TRUE", "FALSE", "TRUE", "FALSE"), .Dim = c(9L, 6L), .Dimnames = list(NULL, c("type", "ignored", "id", "call", "ls", "ls.invalid")))
      )
    } )
    # This is to test for issue #2, which resulted in a self referential environment
    # in the stored items.  The following code used to fail:

    new.exps3 <- expression(1 + 1,  a <- 54, b <- 5, 2 + 2, runif(1))
    ref.exps3 <- expression(1 + 1,  a <- 54, 2 + 2, runif(1))
    my.unitizer6 <- new("unitizer", id=1, zero.env=new.env())
    # add ref.exps as new items
    capture.output(my.unitizer6 <- my.unitizer6 + ref.exps3)
    my.unitizer7 <- new("unitizer", id=2, zero.env=new.env())
    # now convert them to reference items
    capture.output(my.unitizer7 <- my.unitizer7 + my.unitizer6@items.new)
    # now test against new.exps
    capture.output(my.unitizer7 <- my.unitizer7 + new.exps3)

    # Note this doesn't test that there are no circular references, only that what
    # used to fail no longer fails.

    test_that("No circular environment references", {
      expect_equal(
        structure(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), .Dim = 5:6, .Dimnames = list(NULL, c("", "value", "conditions", "output", "message", "aborted"))),
        cbind(my.unitizer7@tests.new, my.unitizer7@tests.result)
      )
    } )
    # Error objects

    test_that("Error Display", {
      # # Superseded by diffobj
      # expect_match(
      #   paste0(capture.output(show(my.unitizer7@tests.errorDetails[[5L]])), collapse=";"),
      #   "^@@ \\.ref @@;-  \\[1\\] [0-9.]+;@@ \\.new @@", "+  \\[1\\] [0-9.]$"
      # )
      expect_match(
        capture.output(show(my.unitizer7@tests.errorDetails[[5L]])),
        "Value mismatch:", all=FALSE
      )
    })
    test_that("testFuns", {
      # these two should just work fine
      new("testFuns", output=all.equal, value=function(x, y) TRUE)
      new("testFuns")
      expect_error(
        new(
          "testFuns", output=all.equal, value=function(x, y, z) TRUE
        ),
        "invalid class .* object"
      )
      # this should work too now, since technically has two args
      expect_is(
        new("testFuns", output=all.equal, value=function(x, y=1, z=1) TRUE),
        "testFuns"
      )
      expect_error( new("testFuns", cabbage=all.equal),
        "Can't initialize invalid slots .*cabbage"
      )
    } )
    new.exps4 <- expression(a <- function() b(), b <- function() TRUE, a())
    my.unitizer8 <- new("unitizer", id=3, zero.env=new.env())
    new.exps5 <- expression(a <- function() b(), NULL, b <- function() TRUE, a())
    my.unitizer9 <- new("unitizer", id=4, zero.env=new.env())
    capture.output(x <- my.unitizer9 + new.exps5)

    test_that("Misc", {
      fun <- function() quote(stop("This error should not be thrown"))
      expect_true(info="Make sure tests that return calls work",
        is(
          new(
            "unitizerItem", value=fun(), call=quote(fun()),
            env=sys.frame(sys.parent() + 1L)
          ),
          "unitizerItem"
        )
      )
      # Nested environment hand waving can break down under certain circumstances
      # this first one should work because there are no tests until after all
      # the pieces necessary to run `a()` are defined:

      capture.output(res <- my.unitizer8 + new.exps4)
      expect_true(info="This is where `unitizer` nested environments fail",
        is(res, "unitizer")
      )
      # this should break because the NULL forces `b` to be stored in a different
      # environment to `a`; note: funky error message matching because in
      # at least some versions of rdevel reported fun name seems to change
      # (possibly related to level 3 bytecode)

      expect_match(
        x@items.new[[4]]@data@message[[1]],
        "Error in [ab]\\(\\) : could not find function "
      )
    } )

    test_that("Comparison Function Errors", {

      exps <- expression(
        fun <- function(x, y) warning("not gonna work"),
        unitizer_sect(compare=fun, expr={
          1 + 1
        })
      )
      my.unitizer <- new("unitizer", id=25, zero.env=new.env())
      # add ref.exps as new items
      capture.output(my.unitizer <- my.unitizer + exps)

      capture.output(
        my.unitizer2 <-
          new("unitizer", id=26, zero.env=new.env()) + my.unitizer@items.new
      )
      expect_warning(
        capture.output(my.unitizer2 <- my.unitizer2 + exps),
        "not gonna work"
      )
      expect_identical(as.character(my.unitizer2@tests.status), c("Pass", "Error"))
      expect_identical(
        my.unitizer2@tests.errorDetails[[2]]@value@value,
        "comparison function `fun` signaled a condition of class `c(\"simpleWarning\", \"warning\", \"condition\")`, with message \"not gonna work\" and call `fun(2, 2)`."
      )
    } )
    test_that("Language Objects Tested Properly", {
      exps <- expression(
        quote(x),
        quote(x + y),
        quote(identity(x)),
        expression(1 + y),
        quote(expression(1 + y))
      )
      my.unitizer <- new("unitizer", id=27, zero.env=new.env())
      # add ref.exps as new items
      capture.output(my.unitizer <- my.unitizer + exps)

      capture.output(
        my.unitizer2 <-
          new("unitizer", id=28, zero.env=new.env()) + my.unitizer@items.new
      )
      capture.output(my.unitizer2 <- my.unitizer2 + exps)

      # This used to error b/c expressions returning unevaluated calls/symbols were
      # not compared as such (they were evaluated)

      expect_identical(as.character(my.unitizer2@tests.status), rep("Pass", 5))
    } )
    test_that(
      "Test Fun Captured Properly", {
      expect_equal(
        new("unitizerItemTestFun", fun=identical)@fun.name, "identical"
      )
    })
  } )
})
