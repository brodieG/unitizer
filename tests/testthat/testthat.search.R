library(testthat)
library(devtools)

unitizer.dir <- system.file(package="unitizer")
install(paste0(unitizer.dir, "/example.pkgs/unitizerdummypkg1"))
install(paste0(unitizer.dir, "/example.pkgs/unitizerdummypkg2"))
unitizer.dummy.list <- list(A=1, B=2, C=3)
unitizer.dummy.list.2 <- list(A=13, B=24, C=35)

try(detach("package:unitizer", unload=TRUE), silent=TRUE)
try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
try(detach("package:unitizerdummypkg2", unload=TRUE), silent=TRUE)
while("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))

state.set <- setNames(
  rep(2L, length(unitizer:::.unitizer.global.settings.names)),
  unitizer:::.unitizer.global.settings.names
)

library(unitizer)
library(unitizerdummypkg1)
library(unitizerdummypkg2)

test_that("Detecting packages", {
  expect_true(unitizer:::is.loaded_package("package:unitizer"))
  expect_false(unitizer:::is.loaded_package("unitizer"))
  expect_true(unitizer:::is.loaded_package("package:stats"))
  expect_error(unitizer:::is.loaded_package(1))
  expect_error(unitizer:::is.loaded_package(letters))
  expect_false(unitizer:::is.loaded_package("Autoloads"))
} )
search.init <- unitizer:::search_as_envs()

test_that("Path Compression", {
  expect_identical(
    head(unitizer:::unitizerCompressTracking(search.init), 3L),
    c(".GlobalEnv", "package:unitizerdummypkg2 (v0.1)", "package:unitizerdummypkg1 (v0.1)")
  )
})
test_that("Moving Objects on Search Path Works", {
  if(length(search.init) < 6L) stop("Unexpetedly short search path")

  expect_error(unitizer:::move_on_path(5L, 2L))
  expect_error(unitizer:::move_on_path(1L, 2L))
  unitizer:::move_on_path(2L, 5L)
  expect_equal(  # can't compare actual environments as they change when detached and re-attached
    unitizer:::search_as_envs(),
    search.init[c(1L, 5L, 2L:4L, 6L:length(search.init))]
  )
  # Now let's undo the previous move

  for(i in rep(5L, 3L))            # Push second pack back to original position
    unitizer:::move_on_path(2L, 5L)

  expect_equal(  # can't compare actual environments as they change when detached and re-attached
    unitizer:::search_as_envs(),
    search.init
  )
})
try(detach("package:unitizer", unload=TRUE), silent=TRUE)
try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
try(detach("package:unitizerdummypkg2", unload=TRUE), silent=TRUE)
library(unitizer)

# Initialize a global tracking object.  Doing it funny here because we don't
# want to run the search_path_trim command yet, and that would happen if we
# did a normal init

search.ref <- NULL # will be modified later
search.init <- unitizer:::search_as_envs()

untz.glob <- unitizer:::unitizerGlobal$new(enable.which=state.set)

test_that("Search Path Journaling Works", {
  # Note, these are intended to be run without the shimming in place

  expect_identical(
    untz.glob$status,
    new(
      "unitizerGlobalStatus", search.path=2L, working.directory=2L,
      options=2L, random.seed=2L
    )
  )
  # state should only be recorded if it changes

  st.0 <- untz.glob$indices.last
  st.1 <- untz.glob$state()
  expect_identical(
    st.0,
    new(
      "unitizerGlobalIndices", search.path=1L, working.directory=1L,
      options=1L, random.seed=1L
    )
  )
  expect_identical(st.0, st.1)

  # Add a package

  library("unitizerdummypkg1")
  st.2 <- untz.glob$state()
  expect_equal(st.2@search.path, 2L) # have two recorded states
  expect_equal(diff(sapply(untz.glob$tracking@search.path, length)), 1L)  # should have one more item
  expect_equal(
    environmentName(untz.glob$tracking@search.path[[2L]][[2L]]),
    "package:unitizerdummypkg1"
  )
  sp.tmp <- untz.glob$tracking@search.path
  # note we compare attribute separately because subsetting drops them
  expect_identical(sp.tmp[[1L]]@.items, sp.tmp[[2L]][-2L]@.items)
  expect_identical(
    sp.tmp[[1L]]@ns.dat,
    sp.tmp[[2L]]@ns.dat[names(sp.tmp[[2L]]@ns.dat) != "unitizerdummypkg1"]
  )
  # Add another package at a different position

  library("unitizerdummypkg2", pos=4L)
  st.3 <- untz.glob$state()

  expect_equal(diff(sapply(untz.glob$tracking@search.path, length)), c(1L, 1L))
  expect_equal(
    environmentName(untz.glob$tracking@search.path[[st.3@search.path]][[4L]]),
    "package:unitizerdummypkg2"
  )
  # Attach a list

  attach(unitizer.dummy.list)
  search.ref <<- untz.glob$state()

  expect_equal(
    environmentName(
      untz.glob$tracking@search.path[[search.ref@search.path]][[2L]]
    ),
    "unitizer.dummy.list"
  )
  expect_identical(
    as.list(untz.glob$tracking@search.path[[search.ref@search.path]][[2L]]),
    unitizer.dummy.list
  )
  # And one more, but modified

  unitizer.dummy.list.2 <- list(A=13, B=24, C=35)

  attach(unitizer.dummy.list.2, pos=4L, name="unitizer.dummy.list")
  st.4 <- untz.glob$state()
  curr.sp.ind <- untz.glob$indices.last@search.path

  expect_equal(
    environmentName(untz.glob$tracking@search.path[[curr.sp.ind]][[4L]]),
    "unitizer.dummy.list"
  )
  # Make sure search path is lining up

  expect_equal(names(untz.glob$tracking@search.path[[curr.sp.ind]]), search())

  expect_identical(
    as.list(untz.glob$tracking@search.path[[curr.sp.ind]][[4L]]),
    unitizer.dummy.list.2
  )
  expect_identical(
    as.list(untz.glob$tracking@search.path[[curr.sp.ind]][[2L]]),
    unitizer.dummy.list
  )
  expect_identical(  # should still point to same environment
    untz.glob$tracking@search.path[[curr.sp.ind - 1L]][[2L]],
    untz.glob$tracking@search.path[[curr.sp.ind]][[2L]]
  )
  # state shouldn't have changed

  expect_identical(untz.glob$state(), st.4)

  # detach some stuff

  detach(2L)  # this is the first list
  untz.glob$state()
  curr.sp.ind <- untz.glob$indices.last@search.path

  expect_identical(
    untz.glob$tracking@search.path[[curr.sp.ind]]@.items,
    untz.glob$tracking@search.path[[curr.sp.ind - 1L]][-2L]@.items
  )
  detach("package:unitizerdummypkg2")
  untz.glob$state()
  curr.sp.ind <- untz.glob$indices.last@search.path

  expect_identical(
    untz.glob$tracking@search.path[[curr.sp.ind]]@.items,
    untz.glob$tracking@search.path[[curr.sp.ind - 1L]][-5L]@.items
  )
})
# Now, lets try to restore some stuff

test_that("Resetting search path", {
  expect_identical(
    as.list(as.environment("unitizer.dummy.list")),
    unitizer.dummy.list.2
  )
  # set to just after we added the original dummy list

  untz.glob$reset(search.ref)
  expect_identical(
    as.list(as.environment("unitizer.dummy.list")),
    unitizer.dummy.list
  )
  # Confirm we actually set to expected path
  # NOTE: not sure if with updates this can work

  expect_equal(
    unitizer:::search_as_envs(),
    untz.glob$tracking@search.path[[search.ref@search.path]]
  )
  # Reset to very beginning

  untz.glob$resetFull()

  expect_true(all.equal(unitizer:::search_as_envs(), search.init))
} )

test_that("Search Path Trim / Restore", {
  search.init <- unitizer:::search_as_envs()
  untz.glob <- unitizer:::unitizerGlobal$new(enable.which=state.set)

  library(unitizerdummypkg1)
  library(unitizerdummypkg2)

  keep.more <- c("package:testthat", getOption("unitizer.search.path.keep"))
  keep.all <- c(keep.more, .unitizer.base.packages)
  unitizer:::search_path_trim(keep.more)
  untz.glob$state()

  expect_identical(
    search(),
    keep.all[match(names(search.init), keep.all, nomatch=0L)]
  )
  untz.glob$resetFull()

  expect_true(all.equal(unitizer:::search_as_envs(), search.init))
} )
try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
try(detach("package:unitizerdummypkg2", unload=TRUE), silent=TRUE)
while("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))

test_that("Loaded Namespaces don't cause issues", {
  # had a problem earlier trying to re-attach namespaces

  loadNamespace("unitizerdummypkg1")
  untz.glob <- unitizer:::unitizerGlobal$new(enable.which=state.set)
  keep.more <- c("package:testthat", getOption("unitizer.search.path.keep"))
  unitizer:::search_path_trim(keep.more)
  untz.glob$state()
  loadNamespace("unitizerdummypkg2")
  untz.glob$state()
  expect_false("unitizerdummypkg1" %in% loadedNamespaces())
  expect_true("unitizerdummypkg2" %in% loadedNamespaces())
  untz.glob$resetFull()
  expect_true("unitizerdummypkg1" %in% loadedNamespaces())
  expect_false("unitizerdummypkg2" %in% loadedNamespaces())
  unloadNamespace("unitizerdummypkg1")
})
test_that("Prevent Namespace Unload Works", {
  old.opt <- options(unitizer.namespace.keep="unitizerdummypkg1")
  loadNamespace("unitizerdummypkg1")
  glb <- unitizer:::unitizerGlobal$new()
  glb$status@options <- 2L
  unitizer:::unload_namespaces("unitizerdummypkg1", global=glb)
  expect_true(glb$ns.opt.conflict@conflict)
  expect_equal(glb$ns.opt.conflict@namespaces, "unitizerdummypkg1")
  options(old.opt)
  unloadNamespace("unitizerdummypkg1")
})

try(detach("package:unitizer", unload=TRUE), silent=TRUE)
try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
try(detach("package:unitizerdummypkg2", unload=TRUE), silent=TRUE)
remove.packages(c("unitizerdummypkg1", "unitizerdummypkg2"))
