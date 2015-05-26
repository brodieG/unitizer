library(testthat)
library(devtools)

unitizer.dir <- system.file(package="unitizer")
install(paste0(unitizer.dir, "/example.pkgs/unitizerdummypkg1"))
install(paste0(unitizer.dir, "/example.pkgs/unitizerdummypkg2"))
unitizer.dummy.list <- list(A=1, B=2, C=3)

try(detach("package:unitizer", unload=TRUE))
try(detach("package:unitizerdummypkg1", unload=TRUE))
try(detach("package:unitizerdummypkg2", unload=TRUE))
while("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))

library(unitizer)
library(unitizerdummypkg1)
library(unitizerdummypkg2)

search.init <- unitizer:::search_as_envs()

test_that("Moving Objects on Search Path Works", {
  if(length(search.init) < 6L) stop("Unexpetedly short search path")

  expect_error(unitizer:::move_on_path(5L, 2L))
  expect_error(unitizer:::move_on_path(1L, 2L))
  unitizer:::move_on_path(2L, 5L)
  expect_identical(  # can't compare actual environments as they change when detached and re-attached
    sapply(unitizer:::search_as_envs(), attributes, simplify=FALSE),
    sapply(
      search.init[c(1L, 5L, 2L:4L, 6L:length(search.init))], attributes,
      simplify=FALSE
  ) )
  # Now let's undo the previous move

  for(i in rep(5L, 3L))            # Push second pack back to original position
    unitizer:::move_on_path(2L, 5L)

  expect_identical(  # can't compare actual environments as they change when detached and re-attached
    sapply(unitizer:::search_as_envs(), attributes, simplify=FALSE),
    sapply(search.init, attributes, simplify=FALSE)
  )
})

try(detach("package:unitizer", unload=TRUE))
try(detach("package:unitizerdummypkg1", unload=TRUE))
try(detach("package:unitizerdummypkg2", unload=TRUE))
library(unitizer)

search.ref <- NULL # will be modified later
search.init <- unitizer:::search_as_envs()

test_that("Search Path Journaling Works", {
  # Note, these are intended to be run without the shimming in place

  untz.glob <- unitizer:::unitizerGlobal$new()
  untz.glob$enable()
  expect_identical(
    untz.glob$status,
    new(
      "unitizerGlobalStatus", search.path=TRUE, working.directory=TRUE,
      par.env=TRUE, options=TRUE
    )
  )
  st.0 <- untz.glob$state()
  expect_identical(
    st.0,
    new(
      "unitizerGlobalIndices", search.path=1L, working.directory=1L,
      options=1L
    )
  )
  # Add a package

  library("unitizerdummypkg1")
  st.1 <- untz.glob$state()
  expect_equal(st.1@search.path, 2L) # have two recorded states
  expect_equal(diff(sapply(untz.glob$tracking@search.path, length)), 1L)  # should have one more item
  expect_equal(
    environmentName(untz.glob$tracking@search.path[[2L]][[2L]]),
    "package:unitizerdummypkg1"
  )
  expect_identical(
    untz.glob$tracking@search.path[[1L]],
    untz.glob$tracking@search.path[[2L]][-2L]
  )
  # Add another package at a different position

  library("unitizerdummypkg2", pos=4L)
  st.2 <- untz.glob$state()

  expect_equal(diff(sapply(untz.glob$tracking@search.path, length)), c(1L, 1L))
  expect_equal(
    environmentName(untz.glob$tracking@search.path[[st.2@search.path]][[4L]]),
    "package:unitizerdummypkg2"
  )
  # Attach a list

  attach(unitizer.dummy.list)
  st.3 <- untz.glob$state()

  expect_equal(
    environmentName(untz.glob$tracking@search.path[[st.3@search.path]][[2L]]),
    "unitizer.dummy.list"
  )
  expect_identical(
    as.list(untz.glob$tracking@search.path[[st.3@search.path]][[2L]]),
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
  st.5 <- untz.glob$state()
  curr.sp.ind <- untz.glob$indices.last@search.path

  expect_identical(
    untz.glob$tracking@search.path[[curr.sp.ind]],
    untz.glob$tracking@search.path[[curr.sp.ind - 1L]][-2L]
  )
  detach("package:unitizerdummypkg2")
  st.6 <- untz.glob$state()
  curr.sp.ind <- untz.glob$indices.last@search.path

  expect_identical(
    untz.glob$tracking@search.path[[curr.sp.ind]],
    untz.glob$tracking@search.path[[curr.sp.ind - 1L]][-5L]
  )
})



# Now, lets try to restore some stuff

  # set to just after we added t

  st.7 <- untz.glob$reset(st.3)
