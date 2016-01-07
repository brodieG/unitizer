library(unitizer)
context("State")
old.opt <- options(unitizer.color=FALSE, width=80L)
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
  state.E <- new(
    "unitizerGlobalState", options=setNames(as.list(1:20), head(letters, 20))
  )
  state.F <- new(
    "unitizerGlobalState", options=setNames(as.list(1:20), tail(letters, 20))
  )
  # This one is supposed to return something non-character or TRUE when used
  # with the provided all.equal

  state.G <- new(
    "unitizerGlobalState",
    options=list(a=structure(TRUE, class="unitizer_glob_state_test"), b=0)
  )
  state.H <- new(
    "unitizerGlobalState",
    options=list(a=structure(FALSE, class="unitizer_glob_state_test"), b=2)
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
    c("`options` state mismatch:", "    @@ .REF$state@options$a @@", "    -  [1] 5 6 7", "    @@ .NEW$state@options$a @@", "    +  [[1]]", "    +  [1] 1", "    +  ", "    +  [[2]]", "    +  [1] 2", "    +  ", "    +  [[3]]", "    +  [1] 3", "    +  ", "For a more detailed comparison you can access state values directly (e.g. ", ".NEW$state@options).  Note that there may be state differences that are not ", "reported here as state tracking is incomplete.  See vignette for details.")
  )
  # These have big enough options differences to kick off the condensed
  # options comparison

  expect_equal(
    diff_state(state.E, state.F, file=NULL),
    c("`options` state mismatch:", "    The following options have mismatches: ", "     [1] \"g\" \"h\" \"i\" \"j\" \"k\" \"l\" \"m\" \"n\" \"o\" \"p\" \"q\" \"r\" \"s\" \"t\"", "    The following options are missing from `.NEW`: ", "    [1] \"a\" \"b\" \"c\" \"d\" \"e\" \"f\"", "    The following options are missing from `.REF`: ", "    [1] \"u\" \"v\" \"w\" \"x\" \"y\" \"z\"", "For a more detailed comparison you can access state values directly (e.g. ", ".NEW$state@options).  Note that there may be state differences that are not ",  "reported here as state tracking is incomplete.  See vignette for details.")
  )
  expect_equal(
    diff_state(state.G, state.H, file=NULL),
    c("`options` state mismatch:", "    - a: <unknown difference>", "    - b: Mean absolute difference: 2", "For a more detailed comparison you can access state values directly (e.g. ", ".NEW$state@options).  Note that there may be state differences that are not ", "reported here as state tracking is incomplete.  See vignette for details.")
  )
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
      unitizer:::unitizerStateRaw(par.env=in_pkg()), test.files=getwd()
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
      unitizer:::unitizerStateRaw(par.env=in_pkg("")), test.files=getwd()
    ),
    "Argument `package` may not be"
  )
  # impossible states

  state.obj <- unitizer:::unitizerStateRaw()
  state.obj@options <- 2L

  expect_error(unitizer:::as.state(state.obj), "Options state tracking")

  state.obj@namespaces <- 2L
  state.obj@search.path <- 1L

  expect_error(unitizer:::as.state(state.obj), "Namespace state tracking")
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
test_that("merge states", {
  trk.new <- new("unitizerGlobalTrackingStore",
    search.path=list(1, 2, 3),
    options=list("a", "b")
  )
  trk.ref <- new("unitizerGlobalTrackingStore",
    search.path=list(4, 5, 6),
    options=list("c", "d")
  )
  items <- new("unitizerItems")

  items <- items + new(
    "unitizerItem", call=quote(1 + 1), glob.indices=new(
      "unitizerGlobalIndices", search.path=1L, options=2L
  ) )
  items <- items + new(
    "unitizerItem", call=quote(2 + 1), glob.indices=new(
      "unitizerGlobalIndices", search.path=2L, options=1L
  ) )
  items <- items + new(
    "unitizerItem", call=quote(1 * 1), reference=TRUE, glob.indices=new(
      "unitizerGlobalIndices", search.path=1L, options=1L
  ) )
  items <- items + new(
    "unitizerItem", call=quote(2 * 1), reference=TRUE, glob.indices=new(
      "unitizerGlobalIndices", search.path=3L, options=2L
  ) )
  res <- unitizer:::mergeStates(items, trk.new, trk.ref)
  expect_equal(
    sapply(res$items, function(x) as.integer(slot(x, "glob.indices"))),
    structure(c(1L, 2L, 0L, 0L, 0L, 2L, 1L, 0L, 0L, 0L, 4L, 3L, 0L, 0L, 0L, 5L, 4L, 0L, 0L, 0L), .Dim = c(5L, 4L), .Dimnames = list(c("search.path", "options", "working.directory", "random.seed", "namespaces"), NULL))
  )
  s.n.to.check <- c(
    "search.path", "options", "working.directory", "random.seed", "namespaces"
  )
  expect_equal(
    sapply(s.n.to.check, slot, object=res$state),
    structure(list(search.path = list(1, 2, 3, 4, 6), options = list("a", "b", "c", "d"), working.directory = list(), random.seed = list(), namespaces = list()), .Names = c("search.path", "options", "working.directory", "random.seed", "namespaces"))
  )
  # No reference items

  items.no.ref <- items[1:2]
  expect_identical(
    unitizer:::mergeStates(items.no.ref, trk.new, trk.ref),
    list(items=items.no.ref, states=trk.new)
  )
  # No new items; note that we only remap the used states to the new state
  # which is why we need all the .mod objects

  items.no.new <- items[3:4]
  items.no.new.mod <- items.no.new
  items.no.new.mod[[2L]]@glob.indices@search.path <- 2L
  trk.ref.mod <- trk.ref
  trk.ref.mod@search.path[[2L]] <- NULL

  expect_identical(
    unitizer:::mergeStates(
      items.no.new, new("unitizerGlobalTrackingStore"), trk.ref
    ),
    list(items=items.no.new.mod, states=trk.ref.mod)
  )
})
options(old.opt)
