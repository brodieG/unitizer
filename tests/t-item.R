source(file.path("_helper", "init.R"))
source(file.path("aammrtf", "ref.R")); make_ref_obj_funs("item")

options(unitizer.color = FALSE)

# These tests are intended to cover all the functions/classes/methods in:
# - item.R
# - item.sub.R
# - test_eval.R     # indirectly
# - heal.R
# - unitizer.R
# Basically everything that can be tested non-interactively
# Helper funs

callDep <- function(x) paste0(deparse(x@call, width.cutoff = 500),
    collapse = "")
lsObjs <- function(x) paste0(x@ls$names, x@ls$status, collapse = ", ")
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

Sys.sleep(0.2)
my.unitizer <- new("unitizer", id = 1, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer <- my.unitizer + ref.exps)
my.unitizer2 <- new("unitizer", id = 2, zero.env = new.env())
# now convert them to reference items
coi(my.unitizer2 <- my.unitizer2 + my.unitizer@items.new)
# now test against new.exps
coi(my.unitizer2 <- my.unitizer2 + new.exps)

# - "item funs" ----------------------------------------------------------------

item <- my.unitizer@items.new[[1L]]
unitizer:::itemType(item)
try(unitizer:::itemType(item) <- "asdfasd")
unitizer:::itemType(item) <- "reference"
unitizer:::itemType(item)
try(unitizer:::itemsType(my.unitizer@items.new) <- as.character(1:1000))
try(item$booboo)

# - "unitizer creation worked as expected" -------------------------------------

validObject(my.unitizer, complete = TRUE)
all.equal(capture.output(show(my.unitizer@items.new[[1L]])), rds(100))
identical(length(my.unitizer2), length(new.exps))
identical(length(my.unitizer2@items.new), length(new.exps))
identical(length(my.unitizer2@items.ref), length(ref.exps))
all.equal(
  as.expression(
    lapply(unitizer:::as.list(my.unitizer2@items.new), slot, "call")
  ),
  new.exps
)
all.equal(
  as.expression(
    lapply(unitizer:::as.list(my.unitizer2@items.ref), slot, "call")
  ),
  ref.exps
)
vals <- lapply(
  unitizer:::as.list(my.unitizer2@items.new), function(x) x@data@value[[1L]]
)
vals.ign <- unitizer:::ignored(my.unitizer2@items.new)
all.equal(vals[!vals.ign], lapply(new.exps, eval)[!vals.ign])
all(vapply(vals[vals.ign], is, logical(1L), "unitizerDummy"))

vals <- lapply(
  unitizer:::as.list(my.unitizer2@items.ref), function(x) x@data@value[[1L]]
)
vals.ign <- unitizer:::ignored(my.unitizer2@items.ref)
all.equal(vals[!vals.ign], lapply(ref.exps, eval)[!vals.ign])
all(vapply(vals[vals.ign], is, logical(1L), "unitizerDummy"))
my.unitizer2@items.new.map
my.unitizer2@items.ref.map
my.unitizer2@tests.fail
my.unitizer2@tests.status
my.unitizer2@section.map
unitizer:::ignored(my.unitizer2@items.new)
unitizer:::ignored(my.unitizer2@items.ref)

# - "Size Measurement works" ---------------------------------------------------

# Used to produce warnings because the same base.env was used for every
# unitizer because it was created on package load as part of the S4 class
# definition instead of in "initialize", so any time we instantiated more
# than one object they all shared the same environment, causing issues with
# saveRDS
x <- unitizer:::sizeUntz(my.unitizer2)
is.matrix(x) && is.numeric(x)
colnames(x)

# - "Environment healing works" ------------------------------------------------

items.mixed <- my.unitizer2@items.new[4:5] + my.unitizer2@items.ref[[1]] +
    my.unitizer2@items.new[c(2, 6, 8)]
items.sorted <- unitizer:::healEnvs(items.mixed, my.unitizer2)
env.anc <- lapply(unitizer:::as.list(items.sorted), function(x) rev(unitizer:::env_ancestry(x@env,
    my.unitizer2@base.env)))
max.len <- max(vapply(env.anc, length, 1L))
env.anc.2 <- lapply(env.anc, function(x) {
    length(x) <- max.len
    x
})
env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors = FALSE)
# Here only the first item is reference, all others
length(unique(unlist(env.anc.df[2, ])))
all(
  apply(
    env.anc.df[-(1:2), -1], 1,
    function(x) length(unique(Filter(Negate(is.na), x)))
  ) == 1L
)
# First item is reference, all others are new
unitizer:::itemsType(items.sorted)
# Expected order of ids
vapply(unitizer:::as.list(items.sorted), function(x) x@id, integer(1L))
lapply(unitizer:::as.list(items.sorted), function(x) x@ls$names)
unique(unlist(lapply(unitizer:::as.list(items.sorted), function(x) x@ls$status)))
# Tests with conditions

# - "Items with conditions" ----------------------------------------------------

my_fun <- function() {
    warning("hello")
    25
}
ref.exps1a <- expression(stop("boom"), my_fun())
my.unitizer1a <- new("unitizer", id = 100, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer1a <- my.unitizer1a + ref.exps1a)

all.equal(capture.output(show(my.unitizer1a@items.new[[1L]])), rds(200))
all.equal(capture.output(show(my.unitizer1a@items.new[[2L]])), rds(300))
all.equal(
  capture.output(show(my.unitizer1a@items.new[[1L]]@data@conditions)), rds(400)
)
# - "Environment healing works 2" ----------------------------------------------

# Stars highlight items we are selecting, but keep in mind that unitizer only
# cares about non ignored tests, and that the selection status of ignored test
# has nothing to do with what we end up with wrt to ignored tests

new.exps2 <- expression(
  1 + 1,                   #  1 *
  a <- 54,                 #  2
  b <- runif(5),           #  3
  howdy <- "yowser",       #  4 *
  a + b,                   #  5 *
  e <- 5 * a,              #  6
  a ^ 2,                   #  7
  f <- e * a,              #  8
  matrix(rep(f, 20))       #  9 *
)
ref.exps2 <- expression(
  1 + 1,                   #  1
  a <- 54,                 #  2
  b <- runif(5),           #  3 *
  25 + 3,                  #  4
  q <- b ^ 2 / a,          #  5 *
  a + b,                   #  6
  z <- w <- list(1, 2, 3), #  7
  Reduce(`+`, z),          #  8 * Doesn't exist, should connect back to `a + b`
  e <- 5 * a,              #  9
  e ^ 3,                   # 10 *
  e * a                    # 11 *
)

# Note that healEnvs modifies objects that contain environments, and as such
# you won't get the same result if you run this function twice, so don't be
# surprised if tests fail in those circumstances
my.unitizer3 <- new("unitizer", id = 1, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer3 <- my.unitizer3 + ref.exps2)
my.unitizer4 <- new("unitizer", id = 2, zero.env = new.env())
# now convert them to reference items
coi(my.unitizer4 <- my.unitizer4 + my.unitizer3@items.new)
# now test against new.exps
coi(my.unitizer4 <- my.unitizer4 + new.exps2)
coi(
  items.mixed2 <- my.unitizer4@items.ref[c(8, 10, 3, 5, 11)] +
    my.unitizer4@items.new[c(1, 4, 5, 9)]
)
items.sorted2 <- unitizer:::healEnvs(items.mixed2, my.unitizer4)

env.anc <- lapply(unitizer:::as.list(items.sorted2), function(x) rev(unitizer:::env_ancestry(x@env,
    my.unitizer4@base.env)))
max.len <- max(vapply(env.anc, length, 1L))
env.anc.2 <- lapply(env.anc, function(x) {
    length(x) <- max.len
    x
})
# oldest ancestor the same
env.anc.df <- as.data.frame(env.anc.2, stringsAsFactors = FALSE)
length(unique(unname(unlist(env.anc.df[1, ]))))  # 1
# "base.env should be unitizer env")
identical(
  env.anc.df[1, 1], unitizer:::env_name(my.unitizer4@base.env)
)
# "all tests should also have another sub base.env")
length(unique(unlist(env.anc.df[2, ]))) == 1L
# "and it should be the items.ref here")
identical(
  env.anc.df[2, 1], unitizer:::env_name(my.unitizer4@items.ref@base.env)
)
items <- items.sorted2
items.lst <- unitizer:::as.list(items)
# "new items should all have normal status",
heal.info <- cbind(
  type = unitizer:::itemsType(items), ignored = unitizer:::ignored(items),
  id = vapply(items.lst, slot, 1L, "id"),
  call = vapply(items.lst, callDep, ""),
  ls = vapply(items.lst, lsObjs, ""),
  ls.invalid = vapply(items.lst, lsInv, TRUE)
)
# ""
unique(unlist(lapply(items.lst[unitizer:::itemsType(items) == "new"], lsStat)))
# "Reference tests should have no ls data",
unique(vapply(items.lst[unitizer:::ignored(items)], lsObjs, ""))
all(vapply(items.lst[unitizer:::ignored(items)], lsInv, logical(1L)))

# - "ls works" -----------------------------------------------------------------

my.unitizer5 <- new("unitizer", id = 2, zero.env = new.env())
# now add back our composite elements as references
coi(my.unitizer5 <- my.unitizer5 + items.sorted2)
# and new items
coi(my.unitizer5 <- my.unitizer5 + new.exps2)

# This is an ignored test, so there will be some problems
env.val <- new.env(parent = my.unitizer5@items.new[[3]]@env)
env.eval <- new.env(parent = env.val)
assign(".NEW", my.unitizer5@items.new[[3]], env.val)
assign(".new", my.unitizer5@items.new[[3]]@data@value[[1L]],
    env.val)
assign(".REF", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[3]]]],
    env.val)
assign(".ref", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[3]]]]@data@value[[1L]],
    env.val)
ls.res <- evalq(unitizer:::unitizer_ls(), env.eval)  # warn
# Reference tests won't show up since they were nuked by `healEnvs`
all.equal(ls.res, rds(500))
# These are normal tests so should work
env.val <- new.env(parent = my.unitizer5@items.new[[9]]@env)
env.eval <- new.env(parent = env.val)
assign(".NEW", my.unitizer5@items.new[[9]], env.val)
assign(".new", my.unitizer5@items.new[[9]]@data@value[[1L]],
    env.val)
assign(".REF", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[9]]]],
    env.val)
assign(".ref", my.unitizer5@items.ref[[my.unitizer5@items.new.map[[9]]]]@data@value[[1L]],
    env.val)
all.equal(evalq(unitizer:::unitizer_ls(), env.eval),
    rds(600))
all.equal(capture.output(print(evalq(unitizer:::unitizer_ls(),
    env.eval))), rds(700))

# - "Environment Healing Works #3" ---------------------------------------------
#
# Main difference to previous versions is that we're testing that moving the
# order of tests around between ref and new still works
#
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
my.unitizer10 <- new("unitizer", id = 1, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer10 <- my.unitizer10 + ref.exps6)
my.unitizer11 <- new("unitizer", id = 2, zero.env = new.env())
# now convert them to reference items
coi(my.unitizer11 <- my.unitizer11 + my.unitizer10@items.new)
# now test against new.exps
coi(my.unitizer11 <- my.unitizer11 + new.exps6)
items.mixed3 <- my.unitizer11@items.ref[c(4, 7)] +
  my.unitizer11@items.new[c(1, 7, 8)]
items.sorted3 <- unitizer:::healEnvs(items.mixed3, my.unitizer11)

# Both reference tests get appended to item #1, which means among other things
# that for the second refernce test, the `a` object is absent (but `b` is
# present because it gets sucked in by virtue of being an ignored test just
# ahead of it)
items <- items.sorted3
items.lst <- unitizer:::as.list(items)
cbind(
  type = unitizer:::itemsType(items), ignored = unitizer:::ignored(items),
  id = vapply(items.lst, slot, 1L, "id"),
  call = vapply(items.lst, callDep, ""),
  ls = vapply(items.lst, lsObjs, ""),
  ls.invalid = vapply(items.lst, lsInv, TRUE)
)
# - "No circular environment references" ---------------------------------------

# This is to test for issue #2, which resulted in a self referential environment
# in the stored items.  The following code used to fail:
new.exps3 <- expression(1 + 1, a <- 54, b <- 5, 2 + 2, runif(1))
ref.exps3 <- expression(1 + 1, a <- 54, 2 + 2, runif(1))
my.unitizer6 <- new("unitizer", id = 1, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer6 <- my.unitizer6 + ref.exps3)
my.unitizer7 <- new("unitizer", id = 2, zero.env = new.env())
# now convert them to reference items
coi(my.unitizer7 <- my.unitizer7 + my.unitizer6@items.new)
# now test against new.exps
coi(my.unitizer7 <- my.unitizer7 + new.exps3)
# Note this doesn't test that there are no circular references, only that what
# used to fail no longer fails.

cbind(my.unitizer7@tests.new, my.unitizer7@tests.result)

# - "testFuns" -----------------------------------------------------------------

# Error objects

# these two should just work fine
is(new("testFuns", output = all.equal, value = function(x, y) TRUE), "testFuns")
is(new("testFuns"), "testFuns")
try(new("testFuns", output = all.equal, value = function(x, y, z) TRUE))
# this should work too now, since technically has two args
is(
  new("testFuns", output = all.equal, value = function(x, y = 1, z = 1) TRUE),
  "testFuns"
)
try(new("testFuns", cabbage = all.equal))

# - "Misc" ---------------------------------------------------------------------

new.exps4 <- expression(a <- function() b(), b <- function() TRUE, a())
my.unitizer8 <- new("unitizer", id = 3, zero.env = new.env())
new.exps5 <- expression(a <- function() b(), NULL, b <- function() TRUE, a())
my.unitizer9 <- new("unitizer", id = 4, zero.env = new.env())
coi(x <- my.unitizer9 + new.exps5)

local({
  fun <- function() quote(stop("This error should not be thrown"))
  is(
    new(
      "unitizerItem", value = fun(), call = quote(fun()),
      env = sys.frame(sys.parent() + 1L)
    ),
    "unitizerItem"
  )
})
# Nested environment hand waving can break down under certain circumstances
# this first one should work because there are no tests until after all
# the pieces necessary to run `a()` are defined:
coi(res <- my.unitizer8 + new.exps4)
is(res, "unitizer")
# this should break because the NULL forces `b` to be stored in a different
# environment to `a`; note: funky error message matching because in
# at least some versions of rdevel reported fun name seems to change
# (possibly related to level 3 bytecode)
# could not find fun
x@items.new[[4]]@data@message[[1]]

# - "Comparison Function Errors" -----------------------------------------------

exps <- expression(fun <- function(x, y) warning("not gonna work"),
    unitizer_sect(compare = fun, expr = {
        1 + 1
    }))
my.unitizer <- new("unitizer", id = 25, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer <- my.unitizer + exps)
coi(my.unitizer2 <- new("unitizer", id = 26, zero.env = new.env()) +
    my.unitizer@items.new)
# warn: not gonna work
coi(my.unitizer2 <- my.unitizer2 + exps)
as.character(my.unitizer2@tests.status)
my.unitizer2@tests.errorDetails[[2]]@value@value

# - "Language Objects Tested Properly" -----------------------------------------

exps <- expression(quote(x), quote(x + y), quote(identity(x)),
    expression(1 + y), quote(expression(1 + y)))
my.unitizer <- new("unitizer", id = 27, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer <- my.unitizer + exps)
coi(my.unitizer2 <- new("unitizer", id = 28, zero.env = new.env()) +
    my.unitizer@items.new)
coi(my.unitizer2 <- my.unitizer2 + exps)
# This used to error b/c expressions returning unevaluated calls/symbols were
# not compared as such (they were evaluated)
as.character(my.unitizer2@tests.status)

# - "Test Fun Captured Properly" -----------------------------------------------

new("unitizerItemTestFun", fun = identical)@fun.name

