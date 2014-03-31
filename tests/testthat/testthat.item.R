library(testor)
library(testthat)

# These tests are intended to cover all the functions/classes/methods in:
# - item.R
# - item.sub.R
# - test_eval.R     # indirectly
# - heal.R
# - testor.R
# Basically everything that can be tested non-interactively 

local( {
  new.exps <- expression(
    1 + 1,
    a <- 54,
    b <- 38,
    a + b,
    e <- 5 * a,
    a ^ 2,
    f <- e * a,
    matrix(rep(f, 20))
  )
  ref.exps <- expression(
    1 + 1,
    a <- 54,
    b <- 38,
    a + b,
    e <- 5 * a,
    e ^ 3
  )
  my.testor <- new("testor", id=1, zero.env=new.env())
  my.testor <- my.testor + ref.exps   # add ref.exps as new items
  my.testor2 <- new("testor", id=2, zero.env=new.env())  
  my.testor2 <- my.testor2 + my.testor@items.new    # now convert them to reference items
  my.testor2 <- my.testor2 + new.exps   # now test against new.exps

  test_that("testor creation worked as expected", {
    expect_equal(length(my.testor2), length(new.exps))
    expect_equal(length(my.testor2@items.new), length(new.exps))
    expect_equal(length(my.testor2@items.ref), length(ref.exps))
    expect_equal(as.expression(lapply(testor:::as.list(my.testor2@items.new), slot, "call")), new.exps)
    expect_equal(as.expression(lapply(testor:::as.list(my.testor2@items.ref), slot, "call")), ref.exps)
    expect_equal(lapply(testor:::as.list(my.testor2@items.new), function(x) x@data@value), lapply(new.exps, eval))
    expect_equal(lapply(testor:::as.list(my.testor2@items.ref), function(x) x@data@value), lapply(ref.exps, eval))
    expect_equal(my.testor2@items.new.map, c(1L, 2L, 3L, 4L, 5L, NA, NA, NA))
    expect_equal(my.testor2@items.ref.map, c(1L, 2L, 3L, 4L, 5L, NA))
    expect_equal(my.testor2@tests.fail, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(my.testor2@tests.status, structure(c(1L, 1L, 1L, 1L, 1L, 4L, 4L, 4L), .Label = c("Pass", "Fail", "Error", "New", "Deleted"), class = "factor"))  
    expect_equal(my.testor2@section.map, c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L))
    expect_equal(testor:::ignored(my.testor2@items.new), c(F, T, T, F, T, F, T, F))
    expect_equal(testor:::ignored(my.testor2@items.ref), c(F, T, T, F, T, F))
  } )
  test_that("Environment healing works", {
    items.mixed <- my.testor2@items.new[4:5] + my.testor2@items.ref[[1]] + my.testor2@items.new[c(2, 6, 8)]
    items.sorted <- testor:::healEnvs(items.mixed, my.testor2)
    env.anc <- lapply(
      testor:::as.list(items.sorted), 
      function(x) rev(testor:::env_ancestry(x@env, my.testor2@base.env))
    )
    max.len <- max(vapply(env.anc, length, 1L))
    env.anc.2 <- lapply(env.anc, function(x) {length(x) <- max.len; x})
    env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors=FALSE)
    # Here only the first item is reference, all others 

    expect_equal(length(unique(unlist(env.anc.df[2, ]))), 1L)
    expect_true(all(apply(env.anc.df[-(1:2), -1], 1, function(x) length(unique(Filter(Negate(is.na), x)))) == 1L))
    expect_equal(  # First item is reference, all others are new
      testor:::itemsType(items.sorted), c("reference", rep("new", 5))
    )
    expect_equal(  # Expected order of ids
      vapply(testor:::as.list(items.sorted), function(x) x@id, integer(1L)), 
      c(1, 2, 4, 5, 6, 8)
    )
    expect_equal(
      lapply(testor:::as.list(items.sorted), function(x) x@ls$names),
      list(character(0), "a", c("a", "b"), c("a", "b", "e"), c("a",  "b", "e"), c("a", "b", "e", "f"))
    )
    expect_equal(unique(unlist(lapply(testor:::as.list(items.sorted), function(x) x@ls$status))), "")
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
  my.testor3 <- new("testor", id=1, zero.env=new.env())
  my.testor3 <- my.testor3 + ref.exps2   # add ref.exps as new items
  my.testor4 <- new("testor", id=2, zero.env=new.env())  
  my.testor4 <- my.testor4 + my.testor3@items.new    # now convert them to reference items
  my.testor4 <- my.testor4 + new.exps2   # now test against new.exps
  items.mixed2 <- my.testor4@items.ref[c(8, 10, 3, 5, 11)] + my.testor4@items.new[c(1, 4, 5, 9)]
  items.sorted2 <- testor:::healEnvs(items.mixed2, my.testor4)

  test_that("Environment healing works 2", {
    env.anc <- lapply(
      testor:::as.list(items.sorted2), 
      function(x) rev(testor:::env_ancestry(x@env, my.testor4@base.env))
    )
    max.len <- max(vapply(env.anc, length, 1L))
    env.anc.2 <- lapply(env.anc, function(x) {length(x) <- max.len; x})
    unname(env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors=FALSE))
    which.ref <- testor:::itemsType(items.sorted2) == "reference"

    expect_equal(length(unique(unlist(env.anc.df[1, ]))), 1L, info="all tests should have same base.env")
    expect_identical(env.anc.df[1, 1], testor:::env_name(my.testor4@base.env), info="base.env should be testor env")
    expect_equal(length(unique(unlist(env.anc.df[2, ]))), 1L, info="all tests should also have another sub base.env")
    expect_identical(env.anc.df[2, 1], testor:::env_name(my.testor4@items.ref@base.env), info="and it should be the items.ref here")
    expect_equal(info="Checking that new/reference items are in correct order", which.ref, c(F, T, T, F, F, T, F, T, T))
    expect_equal(info="Checking that items are in order",
      vapply(testor:::as.list(items.sorted2), function(x) x@id, integer(1L)), 
      c(1, 3, 5, 4, 5, 8, 9, 10 ,11)
    )
    i.calls <- lapply(testor:::as.list(items.sorted2), function(x) x@call)  # FOR DEBUGGING
    i.ls <- lapply(testor:::as.list(items.sorted2), function(x) x@ls)
    #names(i.ls) <- paste0(i.calls, ifelse(sapply(testor:::as.list(items.sorted2), function(x) x@reference), " REFERENCE", ""))
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
  my.testor5 <- new("testor", id=2, zero.env=new.env())  
  my.testor5 <- my.testor5 + items.sorted2   # now add back our composite elements as references
  my.testor5 <- my.testor5 + new.exps2       # and new items

  test_that("ls works", {
    env.obj <- new.env(parent=my.testor5@items.new[[3]]@env)
    env.val <- new.env(parent=env.obj)
    env.eval <- new.env(parent=env.val)
    assign(".new", my.testor5@items.new[[3]], env.obj)
    assign(".new", my.testor5@items.new[[3]]@data@value, env.val)
    assign(".ref", my.testor5@items.ref[[my.testor5@items.new.map[[3]]]], env.obj)
    assign(".ref", my.testor5@items.ref[[my.testor5@items.new.map[[3]]]]@data@value, env.val)
    expect_equal(
      evalq(testor:::testor_ls(), env.eval),
      structure(list(new = c("a", "b"), ref = c("a", "b"), tests = c(".new", ".ref")), .Names = c("new", "ref", "tests"), class = "testor_ls", mods = character(0))
    )
    env.obj <- new.env(parent=my.testor5@items.new[[9]]@env)
    env.val <- new.env(parent=env.obj)
    env.eval <- new.env(parent=env.val)
    assign(".new", my.testor5@items.new[[9]], env.obj)
    assign(".new", my.testor5@items.new[[9]]@data@value, env.val)
    assign(".ref", my.testor5@items.ref[[my.testor5@items.new.map[[9]]]], env.obj)
    assign(".ref", my.testor5@items.ref[[my.testor5@items.new.map[[9]]]]@data@value, env.val)
    expect_equal(
      evalq(testor:::testor_ls(), env.eval),
      structure(list(new = c("a", "b", "e", "f", "howdy"), ref = c("a",  "b", "e", "f", "howdy"), tests = c(".new", ".ref")), .Names = c("new",  "ref", "tests"), class = "testor_ls", mods = character(0))
    )
  } )
  test_that("testorItemTestsFuns", {
    # these two should just work fine
    new("testorItemTestsFuns", output=all.equal, value=function(x, y) TRUE)
    new("testorItemTestsFuns")
    expect_error(new("testorItemTestsFuns", output=all.equal, value=function(x, y, z) TRUE), "invalid class .* object")
    expect_error(new("testorItemTestsFuns", output=all.equal, value=function(x, y=1, z=1) TRUE), "invalid class .* object")
    expect_error(new("testorItemTestsFuns", cabbage=all.equal), "Can't initialize invalid slots .*cabbage")
  } )
  new.exps3 <- expression(a <- function() b(), b <- function() TRUE, a())
  my.testor6 <- new("testor", id=3, zero.env=new.env())
  new.exps4 <- expression(a <- function() b(), NULL, b <- function() TRUE, a())
  my.testor7 <- new("testor", id=4, zero.env=new.env())
  x <- my.testor7 + new.exps4
  
  test_that("Misc", {
    fun <- function() quote(stop("This error should not be thrown"))
    expect_true(info="Make sure tests that return calls work",
      is(new("testorItem", value=fun(), call=quote(fun()), env=sys.frame(sys.parent() + 1L)), "testorItem")
    )
    # Nested environment hand waving can break down under certain circumstances
    # this first one should work because there are no tests until after all
    # the pieces necessary to run `a()` are defined:
    
    expect_true(info="This is where `testor` nested environments fail",
      is(my.testor6 + new.exps3, "testor")
    )
    # this should break because the NULL forces `b` to be stored in a different
    # environment to `a`.    
    expect_equal(x@items.new[[4]]@data@message, "Error in a() : could not find function \"b\"")
  } )
} )