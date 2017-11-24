library(testthat)
context("Search")

local({
  old.opts <- options(
    unitizer.search.path.keep=c(
      getOption("unitizer.search.path.keep"), "package:testthat"
  ) )
  on.exit({
    try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
    try(detach("package:unitizerdummypkg2", unload=TRUE), silent=TRUE)
    options(old.opts)
  })
  unitizer.dummy.list <- list(A=1, B=2, C=3)
  unitizer.dummy.list.2 <- list(A=13, B=24, C=35)

  # can't unload `unitizer`, ruins `covr`

  try(detach("package:unitizer"), silent=TRUE)
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
    expect_true(is.list(pkg.dat <- unitizer:::get_package_data()))
    expect_true(
      all(
        vapply(
          pkg.dat,
          function(x)
            is.list(x) &&
            identical(names(x), c("name", "lib.loc", "version")),
          logical(1L)
    ) ) )
  } )
  search.init.full <- unitizer:::search_as_envs()
  search.init <- search.init.full$search.path

  test_that("Path Compression", {
    expect_identical(
      head(unitizer:::compress_search_data(search.init.full), 3L),
      c(
        ".GlobalEnv", "package:unitizerdummypkg2 (v0.1)",
        "package:unitizerdummypkg1 (v0.1)"
      )
    )
  })
  test_that("Moving Objects on Search Path Works", {
    if(length(search.init) < 6L) stop("Unexpetedly short search path")
    untz.glob <- unitizer:::unitizerGlobal$new(
      enable.which=state.set, set.global=TRUE
    )
    on.exit(untz.glob$release())
    expect_error(unitizer:::move_on_path(5L, 2L, untz.glob))
    expect_error(unitizer:::move_on_path(1L, 2L, untz.glob))
    unitizer:::move_on_path(2L, 5L, untz.glob)
    # can't compare actual environments as they change when detached and
    # re-attached
    expect_equal(
      names(unitizer:::search_as_envs()$search.path),
      names(search.init[c(1L, 5L, 2L:4L, 6L:length(search.init))])
    )
    # Now let's undo the previous move

    for(i in rep(5L, 3L))            # Push second pack back to original position
      unitizer:::move_on_path(2L, 5L, untz.glob)

    expect_true(
      unitizer:::search_dat_equal(unitizer:::search_as_envs(), search.init.full)
    )
  })
  try(detach("package:unitizer"), silent=TRUE)
  try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
  try(detach("package:unitizerdummypkg2", unload=TRUE), silent=TRUE)
  library(unitizer)

  # Initialize a global tracking object.  Doing it funny here because we don't
  # want to run the search_path_trim command yet, and that would happen if we
  # did a normal init

  search.ref <- NULL # will be modified later
  search.init <- unitizer:::search_as_envs()

  untz.glob <- unitizer:::unitizerGlobal$new(
    enable.which=state.set, set.global=TRUE
  )
  on.exit(untz.glob$release(), add=TRUE)
  test_that("Search Path Journaling Works", {
    # Note, these are intended to be run without the shimming in place

    expect_identical(
      untz.glob$status,
      new(
        "unitizerGlobalStatus", search.path=2L, working.directory=2L,
        options=2L, random.seed=2L, namespaces=2L
      )
    )
    # state should only be recorded if it changes

    st.0 <- untz.glob$indices.last
    st.1 <- untz.glob$state()
    expect_identical(
      st.0,
      new(
        "unitizerGlobalIndices", search.path=1L, working.directory=1L,
        options=1L, random.seed=1L, namespaces=1L
      )
    )
    expect_identical(st.0, st.1)

    # Add a package

    library("unitizerdummypkg1")
    st.2 <- untz.glob$state()
    expect_equal(st.2@search.path, 2L) # have two recorded states
    # should have one more item
    expect_equal(
      diff(
        sapply(
          untz.glob$tracking@search.path, function(x) length(x$search.path)
      ) ),
      1L
    )
    expect_equal(
      environmentName(untz.glob$tracking@search.path[[2L]]$search.path[[2L]]),
      "package:unitizerdummypkg1"
    )
    sp.tmp <- untz.glob$tracking@search.path
    # note we compare attribute separately because subsetting drops them
    expect_identical(sp.tmp[[1L]]$search.path, sp.tmp[[2L]]$search.path[-2L])
    expect_identical(
      sp.tmp[[1L]]$ns.dat,
      sp.tmp[[2L]]$ns.dat[names(sp.tmp[[2L]]$ns.dat) != "unitizerdummypkg1"]
    )
    # Add another package at a different position

    library("unitizerdummypkg2", pos=4L)
    st.3 <- untz.glob$state()

    expect_equal(
      diff(
        sapply(
          untz.glob$tracking@search.path, function(x) length(x$search.path)
      ) ),
      c(1L, 1L)
    )
    expect_equal(
      environmentName(
        untz.glob$tracking@search.path[[st.3@search.path ]]$search.path[[4L]]
      ),
      "package:unitizerdummypkg2"
    )
    # Attach a list

    attach(unitizer.dummy.list)
    search.ref <<- untz.glob$state()

    expect_equal(
      environmentName(
        untz.glob$tracking@search.path[[
          search.ref@search.path
        ]]$search.path[[2L]]
      ),
      "unitizer.dummy.list"
    )
    expect_identical(
      as.list(untz.glob$tracking@search.path[[
        search.ref@search.path
      ]]$search.path[[2L]]),
      unitizer.dummy.list
    )
    # And one more, but modified

    unitizer.dummy.list.2 <- list(A=13, B=24, C=35)

    attach(unitizer.dummy.list.2, pos=4L, name="unitizer.dummy.list")
    st.4 <- untz.glob$state()
    curr.sp.ind <- untz.glob$indices.last@search.path

    expect_equal(
      environmentName(
        untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[4L]]
      ),
      "unitizer.dummy.list"
    )
    # Make sure search path is lining up

    expect_equal(
      names(untz.glob$tracking@search.path[[curr.sp.ind]]$search.path), search()
    )
    expect_identical(
      as.list(untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[4L]]),
      unitizer.dummy.list.2
    )
    expect_identical(
      as.list(untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[2L]]),
      unitizer.dummy.list
    )
    expect_identical(  # should still point to same environment
      untz.glob$tracking@search.path[[curr.sp.ind - 1L]]$search.path[[2L]],
      untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[2L]]
    )
    # state shouldn't have changed

    expect_identical(untz.glob$state(), st.4)

    # detach some stuff

    detach(2L)  # this is the first list
    untz.glob$state()
    curr.sp.ind <- untz.glob$indices.last@search.path

    expect_identical(
      untz.glob$tracking@search.path[[curr.sp.ind]]$search.path,
      untz.glob$tracking@search.path[[curr.sp.ind - 1L]]$search.path[-2L]
    )
    detach("package:unitizerdummypkg2")
    untz.glob$state()
    curr.sp.ind <- untz.glob$indices.last@search.path

    expect_identical(
      untz.glob$tracking@search.path[[curr.sp.ind]]$search.path,
      untz.glob$tracking@search.path[[curr.sp.ind - 1L]]$search.path[-5L]
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
      names(unitizer:::search_as_envs()$search.path),
      names(
        untz.glob$tracking@search.path[[search.ref@search.path]]$search.path
    ) )
    # Reset to very beginning

    untz.glob$resetFull()
    untz.glob$release()

    # compare with all.equal to make sure we use S4 method
    expect_true(
      unitizer:::search_dat_equal(unitizer:::search_as_envs(), search.init)
    )
  } )

  test_that("Search Path Trim / Restore", {
    search.init <- unitizer:::search_as_envs()
    untz.glob <- unitizer:::unitizerGlobal$new(
      enable.which=state.set, set.global=TRUE
    )
    library(unitizerdummypkg1)
    library(unitizerdummypkg2)

    unitizer:::search_path_trim(global=untz.glob)
    untz.glob$state()
    sp.keep <- unitizer:::keep_sp_default()
    expect_identical(
      search(),
      sp.keep[match(names(search.init$search.path), sp.keep,  nomatch=0L)]
    )
    untz.glob$resetFull()
    untz.glob$release()

    expect_true(
      unitizer:::search_dat_equal(unitizer:::search_as_envs(), search.init)
    )
  } )
  try(detach("package:unitizerdummypkg1", unload=TRUE), silent=TRUE)
  try(detach("package:unitizerdummypkg2", unload=TRUE), silent=TRUE)
  while("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))

  test_that("Loaded Namespaces don't cause issues", {
    # had a problem earlier trying to re-attach namespaces
    loadNamespace("unitizerdummypkg1")
    untz.glob <- unitizer:::unitizerGlobal$new(
      enable.which=state.set, set.global=TRUE
    )
    unitizer:::search_path_trim(global=untz.glob)
    unitizer:::namespace_trim(global=untz.glob)
    untz.glob$state()
    loadNamespace("unitizerdummypkg2")
    untz.glob$state()
    expect_false("unitizerdummypkg1" %in% loadedNamespaces())
    expect_true("unitizerdummypkg2" %in% loadedNamespaces())
    untz.glob$resetFull()
    untz.glob$release()
    expect_true("unitizerdummypkg1" %in% loadedNamespaces())
    expect_false("unitizerdummypkg2" %in% loadedNamespaces())
    unloadNamespace("unitizerdummypkg1")
  })
  test_that("Prevent Namespace Unload Works", {
    old.opt <- options(unitizer.namespace.keep="unitizerdummypkg1")
    loadNamespace("unitizerdummypkg1")
    glb <- unitizer:::unitizerGlobal$new(set.global=TRUE)
    on.exit({
      options(old.opt)
      glb$release()
    })
    glb$status@options <- 2L
    unitizer:::unload_namespaces("unitizerdummypkg1", global=glb)
    expect_true(glb$ns.opt.conflict@conflict)
    expect_equal(glb$ns.opt.conflict@namespaces, "unitizerdummypkg1")
    unloadNamespace("unitizerdummypkg1")
  })
  test_that("Generate unique names", {
    expect_equal(
      unitizer:::unitizerUniqueNames(
        list(search.path=c(goodbye="0", hello="1", goodbye="2", goodbye="3"))
      ),
      c("goodbye", "hello", "goodbye.1", "goodbye.2")
    )
  })
})
