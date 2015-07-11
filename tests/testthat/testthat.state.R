library(unitizer)
test_that("Random Seed", {
  old.seed <- if(!exists(".Random.seed")) NULL else .Random.seed
  seed.dat <- getOption("unitizer.seed")
  untz.glob <- unitizer:::unitizerGlobal$new(enable.which="random.seed")
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
  expect_equal(
    all.equal(state.A, state.B),
    structure(c("`options` state mismatch:\n- definite differences for option \"d\"\n- likely differences for option \"c\"\n- possible differences for option \"b\"", "`working.directory` state mismatch: likely differences", "`random.seed` state mismatch: likely differences"), .Names = c("options", "working.directory", "random.seed"))
  )
  expect_equal(
    all.equal(state.B, state.A),
    structure(c("`options` state mismatch:\n- definite differences for option \"d\"\n- likely differences for option \"c\"\n- possible differences for option \"b\"", "`working.directory` state mismatch: likely differences"), .Names = c("options", "working.directory"))
  )
  expect_equal(
    all.equal(state.A, state.A),
    structure("`options` state mismatch: possible differences for option \"b\"", .Names = "options")
  )
  expect_equal(
    all.equal(state.B, state.B),
    structure(c("`options` state mismatch: possible differences for options \"b\", \"c\"", "`working.directory` state mismatch: possible differences"), .Names = c("options", "working.directory"))
  )
})
