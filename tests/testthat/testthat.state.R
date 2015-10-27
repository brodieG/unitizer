library(unitizer)
context("State")
test_that("Random Seed", {
  old.seed <- if(!exists(".Random.seed")) NULL else .Random.seed
  seed.dat <- getOption("unitizer.seed")
  untz.glob <-
    unitizer:::unitizerGlobal$new(enable.which=setNames(2L, "random.seed"))
  do.call(set.seed, seed.dat)
  new.seed <- .Random.seed
  state <- untz.glob$state()
  runif(10)
  untz.glob$reset(state)
  expect_identical(.Random.seed, new.seed)
  untz.glob$resetFull()
  if(is.null(old.seed))
    expect_false(exists(".Random.seed")) else
    expect_identical(old.seed, .Random.seed)
} )
test_that("State Show", {
  expect_equal(
    capture.output(show(unitizer:::unitizerStatePristine())),
    c("           Settings Values", "1           par.env <auto>", "2       search.path      2", "3           options      2", "4 working.directory      2", "5       random.seed      2", "6        namespaces      2", "-----", "0: off", "1: track starting with initial state", "2: track starting with clean state", "<auto>: use special unitizer environment as 'par.env'", "See `?unitizerState` for more details."))
})
test_that("all.equal.unitizerDummy", {
  dummy <- new("unitizerDummy")
  blah <- "hello"
  ref.txt <- "`.REF` value was not recorded, but `.NEW` value was; they are likely different"
  expect_equal(all.equal(dummy, blah), ref.txt)
  expect_true(all.equal(dummy, dummy))
  expect_equal(
    all.equal(blah, dummy),
    "`.NEW` value was not recorded, but `.REF` value was; they are likely different"
  )
  # testing S4 / S3 methods, first works, second doesn't since we can't
  # have an S3 generic with dispatch on 2nd arg
  expect_equal(
    evalq(all.equal(new("unitizerDummy"), "hello"), getNamespace("stats")),
    ref.txt
  )
  expect_equal(
    evalq(all.equal("hello", new("unitizerDummy")), getNamespace("stats")),
    c(
      "Modes: character, S4", "Attributes: < target is NULL, current is list >",
      "target is character, current is unitizerDummy"
  ) )
})
test_that("All Equal States", {
  state.A <- new(
    "unitizerGlobalState",
    search.path=letters[1:3],
    options=list(a=5:7, b=new("unitizerDummy"), c="hello"),
    working.directory="a/b/c"
  )
  state.B <- new(
    "unitizerGlobalState",
    search.path=letters[1:3],
    options=list(
      a=5:7, b=new("unitizerDummy"), d="goodbye", c=new("unitizerDummy")
    ),
    working.directory=new("unitizerDummy"),
    random.seed=1:3
  )
  state.C <- new(
    "unitizerGlobalState",
    search.path=letters,
    options=list(
      a=list(5, 6, 7), c=LETTERS
    ),
    working.directory=new("unitizerDummy"),
    random.seed=1:3
  )
  state.D <- new(  # just compare to A
    "unitizerGlobalState",
    search.path=letters[1:3],
    options=list(a=list(1, 2, 3), b=new("unitizerDummy"), c="hello"),
    working.directory="a/b/c"
  )
  # all.equal tests are really legacy since we don't expect to use them going
  # forwards

  expect_equal(
    all.equal(state.A, state.B),
    structure(c("`options` state mismatch:\n- known differences for option \"d\"\n- likely differences for option \"c\"", "`working.directory` state mismatch: likely differences", "`random.seed` state mismatch: reference state not recorded"), .Names = c("options", "working.directory", "random.seed"))
  )
  expect_equal(
    all.equal(state.B, state.A),
    structure(c("`options` state mismatch:\n- known differences for option \"d\"\n- likely differences for option \"c\"", "`working.directory` state mismatch: likely differences"), .Names = c("options", "working.directory"))
  )
  expect_equal(
    all.equal(state.A, state.A),
    TRUE
  )
  expect_equal(
    all.equal(state.B, state.B),
    structure("`working.directory` state mismatch: possible differences", .Names = "working.directory")
  )
  # diff_state

  old.width <- options(width=80)
  expect_equal(
    diff_state(state.A, state.B, file=NULL),
    c("`options` state mismatch:", "    - c: `.NEW` value was not recorded, but `.REF` value was; they are likely ", "      different", "    - d: this option is missing from `.REF` state", "`working.directory` state mismatch:", "    @@ .REF$state@working.directory @@", "    -  [1] \"a/b/c\"", "    @@ .NEW$state@working.directory @@", "    +  <object not recorded>", "`random.seed` state mismatch:", "    @@ .REF$state@random.seed @@", "    -  NULL", "    @@ .NEW$state@random.seed @@", "    +  [1] 1 2 3",  "For a more detailed comparison you can access state values directly (e.g. ", ".NEW$state@options).  Note that there may be state differences that are not ", "reported here as state tracking is incomplete.  See vignette for details.")
  )
  expect_equal(
    diff_state(state.A, state.C, file=NULL),
    c("`search.path` state mismatch:", "    @@ .REF$state@search.path @@", "    -  [1] \"a\" \"b\" \"c\"", "    @@ .NEW$state@search.path @@", "    +   [1] \"a\" \"b\" \"c\" \"d\" \"e\" \"f\" \"g\" \"h\" \"i\" \"j\" \"k\" \"l\" \"m\" \"n\" \"o\" \"p\" \"q\"", "    +  [18] \"r\" \"s\" \"t\" \"u\" \"v\" \"w\" \"x\" \"y\" \"z\"", "`options` state mismatch:", "    - a: not `all.equal`:", "      1. Modes: numeric, list", "      2. target is numeric, current is list", "    - c: not `all.equal`:", "      1. Lengths (1, 26) differ (string compare on first 1)",  "      2. 1 string mismatch", "    - b: this option is missing from `.NEW` state", "`working.directory` state mismatch:", "    @@ .REF$state@working.directory @@", "    -  [1] \"a/b/c\"", "    @@ .NEW$state@working.directory @@", "    +  <object not recorded>", "`random.seed` state mismatch:", "    @@ .REF$state@random.seed @@", "    -  NULL", "    @@ .NEW$state@random.seed @@", "    +  [1] 1 2 3", "For a more detailed comparison you can access state values directly (e.g. ", ".NEW$state@options).  Note that there may be state differences that are not ",  "reported here as state tracking is incomplete.  See vignette for details.")
  )
  expect_equal(
    diff_state(state.A, state.D, file=NULL),
    c("`options` state mismatch:", "    @@ .REF$state@options[[\"a\"]] @@", "    -  [1] 5 6 7", "    @@ .NEW$state@options[[\"a\"]] @@", "    +  [[1]]", "    +  [1] 1", "    +  ", "    +  [[2]]", "    +  [1] 2", "    +  ", "    +  [[3]]", "    +  [1] 3", "    +  ", "For a more detailed comparison you can access state values directly (e.g. ", ".NEW$state@options).  Note that there may be state differences that are not ", "reported here as state tracking is incomplete.  See vignette for details.")
  )
  options(old.width)
})
test_that("as.state", {
  expect_identical(unitizer:::as.state("default"), unitizer:::unitizerStateDefault())
  expect_identical(
    unitizer:::as.state("pristine"),
    unitizer:::unitizerStatePristine()
  )
  expect_identical(
    unitizer:::as.state(.GlobalEnv),
    unitizer:::unitizerStateDefault(par.env=.GlobalEnv)
  )
  expect_identical(
    unitizer:::as.state(in_pkg("stats")),
    unitizer:::unitizerStateDefault(par.env=getNamespace("stats"))
  )
  stats.lib <- file.path(system.file(package="stats"), "R")
  expect_identical(
    unitizer:::as.state(in_pkg(), test.files=stats.lib),
    unitizer:::unitizerStateDefault(par.env=getNamespace("stats"))
  )
  expect_error(unitizer:::as.state(200))
  state <- unitizer:::unitizerStateOff()
  state@options <- 2L  # bypass validity method
  expect_error(validObject(state))

  # state raw conversions

  expect_identical(
    unitizer:::as.state(unitizer:::unitizerStateRaw()),
    unitizer:::unitizerStateDefault()
  )
  expect_identical(
    unitizer:::as.state(unitizer:::unitizerStateRaw(par.env="stats")),
    unitizer:::unitizerStateDefault(par.env=getNamespace("stats"))
  )
  expect_identical(
    unitizer:::as.state(
      unitizer:::unitizerStateRaw(par.env=in_pkg()), test.files="."
    ),
    unitizer:::unitizerStateDefault(par.env=getNamespace("unitizer"))
  )
  expect_error(
    unitizer:::as.state(unitizer:::unitizerStateRaw(par.env=in_pkg())),
    "Unable to convert"
  )
  expect_identical(
    unitizer:::as.state(unitizer:::unitizerStateRaw(par.env=in_pkg("stats"))),
    unitizer:::unitizerStateDefault(par.env=getNamespace("stats"))
  )
  expect_error(
    unitizer:::as.state(
      unitizer:::unitizerStateRaw(par.env=in_pkg("asdfalkdfasd"))
    ),
    "Unable to convert"
  )
  expect_error(
    unitizer:::as.state(
      unitizer:::unitizerStateRaw(par.env=in_pkg("")), test.files="."
    ),
    "Unable to convert"
  )
})
test_that("state", {
  expect_identical(
    state("stats"),
    unitizer:::unitizerStateRaw(par.env=getNamespace("stats"))
  )
  expect_identical(
    state(in_pkg("stats")),
    unitizer:::unitizerStateRaw(par.env=in_pkg("stats"))
  )
  expect_identical(
    state(in_pkg()),
    unitizer:::unitizerStateRaw(par.env=in_pkg())
  )
})
test_that("in_pkg", {
  expect_error(in_pkg(""), "Argument `package` may not be an empty string")
  expect_identical(as.character(in_pkg()), "<in: auto-detect-pkg>")
  expect_identical(as.character(in_pkg("stats")), "<in: package:stats>")
  expect_identical(capture.output(show(in_pkg())), "<in: auto-detect-pkg>")
  expect_error(
    unitizer:::in_pkg_to_env(in_pkg(), "/"),
    "Unable to detect package"
  )
})
