library(unitizer)
library(testthat)

# These tests are intended to cover all the functions/classes/methods in:
# - item.R
# - item.sub.R
# - test_eval.R     # indirectly
# - heal.R
# - unitizer.R
# Basically everything that can be tested non-interactively 

local( {
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
  my.unitizer <- new("unitizer", id=1, zero.env=new.env())
  my.unitizer <- my.unitizer + ref.exps   # add ref.exps as new items
  my.unitizer2 <- new("unitizer", id=2, zero.env=new.env())  
  my.unitizer2 <- my.unitizer2 + my.unitizer@items.new    # now convert them to reference items
  my.unitizer2 <- my.unitizer2 + new.exps   # now test against new.exps

  test_that("unitizer creation worked as expected", {
    expect_equal(length(my.unitizer2), length(new.exps))
    expect_equal(length(my.unitizer2@items.new), length(new.exps))
    expect_equal(length(my.unitizer2@items.ref), length(ref.exps))
    expect_equal(as.expression(lapply(unitizer:::as.list(my.unitizer2@items.new), slot, "call")), new.exps)
    expect_equal(as.expression(lapply(unitizer:::as.list(my.unitizer2@items.ref), slot, "call")), ref.exps)
    expect_equal(lapply(unitizer:::as.list(my.unitizer2@items.new), function(x) x@data@value), lapply(new.exps, eval))
    expect_equal(lapply(unitizer:::as.list(my.unitizer2@items.ref), function(x) x@data@value), lapply(ref.exps, eval))
    expect_equal(my.unitizer2@items.new.map, c(1L, 2L, 3L, 4L, 5L, NA, NA, NA))
    expect_equal(my.unitizer2@items.ref.map, c(1L, 2L, 3L, 4L, 5L, NA))
    expect_equal(my.unitizer2@tests.fail, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(my.unitizer2@tests.status, structure(c(1L, 1L, 1L, 1L, 1L, 4L, 4L, 4L), .Label = c("Pass", "Fail", "Error", "New", "Deleted"), class = "factor"))  
    expect_equal(my.unitizer2@section.map, c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L))
    expect_equal(unitizer:::ignored(my.unitizer2@items.new), c(F, T, T, F, T, F, T, F))
    expect_equal(unitizer:::ignored(my.unitizer2@items.ref), c(F, T, T, F, T, F))
  } )
  test_that("Environment healing works", {
    items.mixed <- my.unitizer2@items.new[4:5] + my.unitizer2@items.ref[[1]] + my.unitizer2@items.new[c(2, 6, 8)]
    expect_warning(
      items.sorted <- unitizer:::healEnvs(items.mixed, my.unitizer2),
      "Logic Problem: would have assigned circular environment reference"
    )
    env.anc <- lapply(
      unitizer:::as.list(items.sorted), 
      function(x) rev(unitizer:::env_ancestry(x@env, my.unitizer2@base.env))
    )
    max.len <- max(vapply(env.anc, length, 1L))
    env.anc.2 <- lapply(env.anc, function(x) {length(x) <- max.len; x})
    env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors=FALSE)
    # Here only the first item is reference, all others 

    expect_equal(length(unique(unlist(env.anc.df[2, ]))), 1L)
    expect_true(all(apply(env.anc.df[-(1:2), -1], 1, function(x) length(unique(Filter(Negate(is.na), x)))) == 1L))
    expect_equal(  # First item is reference, all others are new
      unitizer:::itemsType(items.sorted), c("reference", rep("new", 5))
    )
    expect_equal(  # Expected order of ids
      vapply(unitizer:::as.list(items.sorted), function(x) x@id, integer(1L)), 
      c(1, 2, 4, 5, 6, 8)
    )
    expect_equal(
      lapply(unitizer:::as.list(items.sorted), function(x) x@ls$names),
      list(character(0), "a", c("a", "b"), c("a", "b", "e"), c("a",  "b", "e"), c("a", "b", "e", "f"))
    )
    expect_equal(unique(unlist(lapply(unitizer:::as.list(items.sorted), function(x) x@ls$status))), "")
  } )
  new.exps2 <- expression(
    1 + 1,                #  1 *
    a <- 54,              #  2
    b <- runif(5),        #  3
    howdy <- "yowser",    #  4 *
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
  my.unitizer3 <- new("unitizer", id=1, zero.env=new.env())
  my.unitizer3 <- my.unitizer3 + ref.exps2   # add ref.exps as new items
  my.unitizer4 <- new("unitizer", id=2, zero.env=new.env())  
  my.unitizer4 <- my.unitizer4 + my.unitizer3@items.new    # now convert them to reference items
  my.unitizer4 <- my.unitizer4 + new.exps2   # now test against new.exps
  items.mixed2 <- my.unitizer4@items.ref[c(8, 10, 3, 5, 11)] + my.unitizer4@items.new[c(1, 4, 5, 9)]

  test_that("Environment healing works 2", {
    items.sorted2 <- unitizer:::healEnvs(items.mixed2, my.unitizer4)
    env.anc <- lapply(
      unitizer:::as.list(items.sorted2), 
      function(x) rev(unitizer:::env_ancestry(x@env, my.unitizer4@base.env))
    )
    max.len <- max(vapply(env.anc, length, 1L))
    env.anc.2 <- lapply(env.anc, function(x) {length(x) <- max.len; x})
    unname(env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors=FALSE))
    which.ref <- unitizer:::itemsType(items.sorted2) == "reference"

    expect_equal(length(unique(unlist(env.anc.df[1, ]))), 1L, info="all tests should have same base.env")
    expect_identical(env.anc.df[1, 1], unitizer:::env_name(my.unitizer4@base.env), info="base.env should be unitizer env")
    expect_equal(length(unique(unlist(env.anc.df[2, ]))), 1L, info="all tests should also have another sub base.env")
    expect_identical(env.anc.df[2, 1], unitizer:::env_name(my.unitizer4@items.ref@base.env), info="and it should be the items.ref here")
    expect_equal(info="Checking that new/reference items are in correct order", which.ref, c(F, T, T, F, F, T, F, T, T))
    expect_equal(info="Checking that items are in order",
      vapply(unitizer:::as.list(items.sorted2), function(x) x@id, integer(1L)), 
      c(1, 3, 5, 4, 5, 8, 9, 10 ,11)
    )
    i.calls <- lapply(unitizer:::as.list(items.sorted2), function(x) x@call)  # FOR DEBUGGING
    i.ls <- lapply(unitizer:::as.list(items.sorted2), function(x) x@ls)
    #names(i.ls) <- paste0(i.calls, ifelse(sapply(unitizer:::as.list(items.sorted2), function(x) x@reference), " REFERENCE", ""))
    i.ls.bind <- do.call(rbind, i.ls)
    expect_equal(info="these are all the objects in the environment",
      i.ls.bind$names,
      c("a", "b", "a", "b", "q", "a", "b", "howdy", "a", "b", "howdy",  "a", "b", "howdy", "q", "w", "z", "a", "b", "e", "f", "howdy",  "a", "b", "e", "f", "howdy", "q", "w", "z", "a", "b", "e", "f",  "howdy", "q", "w", "z")
    )
    expect_equal(info="environment object status",
      i.ls.bind$status,
      c("", "", "*", "*", "", "", "", "", "", "", "", "", "'", "**", "*", "", "", "", "", "", "", "", "", "'", "", "**", "**", "*", "*", "*", "", "'", "", "**", "**", "*", "*", "*")
    )
    expect_equal(unique(do.call(rbind, i.ls[!which.ref])$status), "", info="new items should all have normal status")
  } )
  my.unitizer5 <- new("unitizer", id=2, zero.env=new.env())  
  my.unitizer5 <- my.unitizer5 + items.sorted2   # now add back our composite elements as references
  my.unitizer5 <- my.unitizer5 + new.exps2       # and new items

  test_that("ls works", {
    env.obj <- new.env(parent=my.unitizer5@items.new[[3]]@env)
    env.val <- new.env(parent=env.obj)
    env.eval <- new.env(parent=env.val)
    assign(".new", my.unitizer5@items.new[[3]], env.obj)
    assign(".new", my.unitizer5@items.new[[3]]@data@value, env.val)
    assign(".ref", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[3]]]], env.obj)
    assign(".ref", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[3]]]]@data@value, env.val)
    expect_equal(
      evalq(unitizer:::unitizer_ls(), env.eval),
      structure(list(new = c("a", "b"), ref = c("a", "b"), tests = c(".new", ".ref")), .Names = c("new", "ref", "tests"), class = "unitizer_ls", mods = character(0))
    )
    env.obj <- new.env(parent=my.unitizer5@items.new[[9]]@env)
    env.val <- new.env(parent=env.obj)
    env.eval <- new.env(parent=env.val)
    assign(".new", my.unitizer5@items.new[[9]], env.obj)
    assign(".new", my.unitizer5@items.new[[9]]@data@value, env.val)
    assign(".ref", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[9]]]], env.obj)
    assign(".ref", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[9]]]]@data@value, env.val)
    expect_equal(
      evalq(unitizer:::unitizer_ls(), env.eval),
      structure(list(new = c("a", "b", "e", "f", "howdy"), ref = c("a",  "b", "e", "f", "howdy"), tests = c(".new", ".ref")), .Names = c("new",  "ref", "tests"), class = "unitizer_ls", mods = character(0))
    )
  } )
  # This is to test for issue #2, which resulted in a self referential environment
  # in the stored items.  The following code used to fail:

  new.exps3 <- expression(1 + 1,  a <- 54, b <- 5, 2 + 2, runif(1))
  ref.exps3 <- expression(1 + 1,  a <- 54, 2 + 2, runif(1))
  my.unitizer6 <- new("unitizer", id=1, zero.env=new.env())
  my.unitizer6 <- my.unitizer6 + ref.exps3   # add ref.exps as new items
  my.unitizer7 <- new("unitizer", id=2, zero.env=new.env())  
  my.unitizer7 <- my.unitizer7 + my.unitizer1@items.new    # now convert them to reference items
  my.unitizer7 <- my.unitizer7 + new.exps3   # now test against new.exps

  # Note this doesn't test that there are no circular references, only that what
  # used to fail no longer fails.

  test_that("No circular environment references", {
    expect_equal(
      structure(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), .Dim = 5:6, .Dimnames = list(c("test.result", "test.result", "", "test.result", "test.result"), c("", "value", "conditions", "output", "message", "aborted"))),
      cbind(my.unitizer7@tests.new, my.unitizer7@tests.result)
    )
  } )

  test_that("unitizerItemTestsFuns", {
    # these two should just work fine
    new("unitizerItemTestsFuns", output=all.equal, value=function(x, y) TRUE)
    new("unitizerItemTestsFuns")
    expect_error(new("unitizerItemTestsFuns", output=all.equal, value=function(x, y, z) TRUE), "invalid class .* object")
    expect_error(new("unitizerItemTestsFuns", output=all.equal, value=function(x, y=1, z=1) TRUE), "invalid class .* object")
    expect_error(new("unitizerItemTestsFuns", cabbage=all.equal), "Can't initialize invalid slots .*cabbage")
  } )
  new.exps4 <- expression(a <- function() b(), b <- function() TRUE, a())
  my.unitizer8 <- new("unitizer", id=3, zero.env=new.env())
  new.exps5 <- expression(a <- function() b(), NULL, b <- function() TRUE, a())
  my.unitizer9 <- new("unitizer", id=4, zero.env=new.env())
  x <- my.unitizer9 + new.exps5
  
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
    
    expect_true(info="This is where `unitizer` nested environments fail",
      is(my.unitizer8 + new.exps4, "unitizer")
    )
    # this should break because the NULL forces `b` to be stored in a different
    # environment to `a`. 

    expect_equal(x@items.new[[4]]@data@message[[1]], "Error in a() : could not find function \"b\"")
  } )
} )