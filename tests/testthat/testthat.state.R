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
