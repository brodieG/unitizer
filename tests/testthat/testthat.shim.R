library(unitizer)
library(devtools)
library(testthat)
context("Shim")

prev.trace.state <- tracingState(TRUE)
if(!prev.trace.state) message("Turned on tracing state for tests")
unitizer.dir <- system.file(package="unitizer")
install(paste0(unitizer.dir, "/example.pkgs/unitizerdummypkg1"))
try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
while("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))
unitizer.dummy.list <- list(z=23, x=1, y="hello")

my.env <- new.env()
state.set <- c(search.path=2L)
untz.glob <- unitizer:::unitizerGlobal$new(
  par.env=my.env, enable.which=state.set,
  set.global=TRUE  # make sure to unset this at end
)
untz.glob$shimFuns()

sp <- search()
curr2 <- sp[[2L]]

test_that("Parent Env Stays on Top", {

  expect_identical(environmentName(parent.env(my.env)), curr2)

  library("unitizerdummypkg1")
  expect_identical(
    environmentName(parent.env(my.env)), "package:unitizerdummypkg1"
  )

  attach(unitizer.dummy.list)
  expect_identical(environmentName(parent.env(my.env)), "unitizer.dummy.list")

  detach("unitizer.dummy.list")
  expect_identical(
    environmentName(parent.env(my.env)), "package:unitizerdummypkg1"
  )

  detach("package:unitizerdummypkg1", unload=TRUE)
  expect_identical(environmentName(parent.env(my.env)), curr2)

  expect_true(untz.glob$checkShims())
} )

test_that("Parent env tracking with search path manip", {
  untz.glob$state()
  keep.more <- c(
    "package:testthat", getOption("unitizer.search.path.keep.base")
  )
  unitizer:::search_path_trim(keep.more, global=untz.glob)
  untz.glob$state()

  expect_identical(
    environmentName(parent.env(my.env)), search()[[2L]]
  )

  untz.glob$resetFull()
  expect_identical(environmentName(parent.env(my.env)), curr2)
} )

test_that("Disable Unshims, etc.", {
  untz.glob$unshimFuns()
  expect_true(
    !any(
      vapply(
        list(library, detach, attach),
        inherits,
        logical(1L),
        "functionWithTrace"
  ) ) )
} )
untz.glob$release()
test_that("Checks, errors, etc.", {
  untz.glob <- unitizer:::unitizerGlobal$new(
    par.env=my.env, enable.which=state.set,
    set.global=TRUE  # make sure to unset this at end
  )
  tracingState(FALSE)
  expect_warning(untz.glob$shimFuns(), "tracing state is FALSE")
  expect_identical(parent.env(my.env), .GlobalEnv)

  tracingState(TRUE)

  untz.glob$release()
  untz.glob <- unitizer:::unitizerGlobal$new(par.env=my.env, set.global=TRUE)
  trace("library", quote(cat("I am traced\n")), where=.BaseNamespaceEnv)
  lib.trace <- library

  expect_warning(untz.glob$shimFuns(), "already traced")
  expect_identical(parent.env(my.env), .GlobalEnv)

  expect_false(inherits(attach, "functionWithTrace"))
  expect_false(inherits(detach, "functionWithTrace"))
  expect_true(inherits(library, "functionWithTrace"))
  expect_identical(lib.trace, library)

  untrace("library", where=.BaseNamespaceEnv)

  untz.glob$release()
  untz.glob <- unitizer:::unitizerGlobal$new(par.env=my.env, set.global=TRUE)
  expect_true(untz.glob$shimFuns())

  trace("attach", quote(cat("I am traced\n")), where=.BaseNamespaceEnv)
  attach.trace <- attach

  expect_warning(untz.glob$checkShims(), "functions unexpectedly changed")
  expect_identical(parent.env(my.env), .GlobalEnv)

  expect_false(inherits(detach, "functionWithTrace"))
  expect_false(inherits(library, "functionWithTrace"))
  expect_true(inherits(attach, "functionWithTrace"))
  expect_identical(attach.trace, attach)

  untrace("attach", where=.BaseNamespaceEnv)
  untz.glob$release()
  untz.glob <- unitizer:::unitizerGlobal$new(par.env=my.env, set.global=TRUE)
  expect_true(untz.glob$shimFuns())

  tracingState(FALSE)
  expect_warning(untz.glob$checkShims(), "Tracing state off")
  expect_identical(parent.env(my.env), .GlobalEnv)

  tracingState(TRUE)

  expect_false(inherits(detach, "functionWithTrace"))
  expect_false(inherits(library, "functionWithTrace"))
  expect_false(inherits(attach, "functionWithTrace"))
  untz.glob$release()
} )

try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
while("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))
remove.packages(c("unitizerdummypkg1"))
tracingState(prev.trace.state)
