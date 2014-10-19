library(testthat)

try(detach("package:unitizer", unload=TRUE))
try(detach("package:unitizerdummypkg1", unload=TRUE))
try(detach("package:unitizerdummypkg2", unload=TRUE))

library(unitizer)

test_that("Detecting packages", {
  expect_true(unitizer:::is.loaded_package("package:unitizer"))
  expect_false(unitizer:::is.loaded_package("unitizer"))
  expect_true(unitizer:::is.loaded_package("package:stats"))
  expect_error(unitizer:::is.loaded_package(1))
  expect_error(unitizer:::is.loaded_package(letters))
  expect_false(unitizer:::is.loaded_package("Autoloads"))
} )
test_that("Package shimming working", {
  search.path.init <- search()

  pack.env <- unitizer:::reset_packenv()
  expect_true(unitizer:::search_path_setup())
  expect_warning(sps <- unitizer:::search_path_setup())    # Can't re-shim
  expect_false(sps)
  expect_identical(unitizer:::search_path_unsetup(), TRUE) # undo shimming
  expect_true(unitizer:::search_path_setup())              # now re-shim should work

  expect_identical(search.path.init, pack.env$search.init)
  expect_identical(length(pack.env$history), 0L)

  # Add one package

  library(unitizerdummypkg1)

  expect_identical(length(pack.env$history), 1L)
  expect_identical(
    pack.env$history[[1L]],
    new("searchHist", name="unitizerdummypkg1", type="package", mode="add", pos=2L, extra=NULL)
  )
  expect_identical(setdiff(search(), search.path.init), "package:unitizerdummypkg1")

  # But only once

  library(unitizerdummypkg1)
  expect_identical(length(pack.env$history), 1L)

  # Another pack, but different location

  library(unitizerdummypkg2, pos=3L)

  expect_identical(length(pack.env$history), 2L)
  expect_identical(
    pack.env$history[[2L]],
    new("searchHist", name="unitizerdummypkg2", type="package", mode="add", pos=3L, extra=NULL)
  )
  expect_identical(
    setdiff(search(), search.path.init),
    c("package:unitizerdummypkg1", "package:unitizerdummypkg2")
  )
  # Confirm stuff is working as expected

  expect_true(unitizer:::search_path_check())

  # Detach by position

  rem.obj <- as.environment(3L)  # Capture object we're about to detach
  detach(3L)
  expect_identical(setdiff(search(), search.path.init), "package:unitizerdummypkg1")  # detached pkg2
  expect_identical(length(pack.env$history), 3L)  # but history should grow
  expect_identical(
    pack.env$history[[3L]],
    new(
      "searchHist", name="unitizerdummypkg2", type="package", mode="remove",
      pos=3L, extra=rem.obj
  ) )
  # Detach by name, and also remove namespace

  rem.obj <- as.environment("package:unitizerdummypkg1")  # Capture object we're about to detach
  detach("package:unitizerdummypkg1", unload=TRUE)

  expect_identical(search(), search.path.init)
  expect_identical(length(pack.env$history), 4L)
  expect_identical(
    pack.env$history[[4L]],
    new(
      "searchHist", name="unitizerdummypkg1", type="package", mode="remove",
      pos=2L, extra=rem.obj
  ) )
  # Confirm stuff is working as expected

  expect_true(unitizer:::search_path_check())

  # Turn off search path manip

  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_unsetup()
} )
test_that("Search Path Trim / Restore", {
  try(detach("package:unitizerdummypkg1", unload=TRUE))
  try(detach("package:unitizerdummypkg2", unload=TRUE))
  library(unitizerdummypkg1)
  library(unitizerdummypkg2)
  search.path.init <- search()

  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_setup()
  expect_true(unitizer:::search_path_trim())
  testthat::expect_identical(  # note testthat no longer on search path
    length(search.path.init),
    length(search()) + length(pack.env$history)
  )
  testthat::expect_true(unitizer:::search_path_restore())
  expect_identical(search(), search.path.init)

  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_unsetup()   # not strictly necessary after restore()
} )

test_that("Search Path Trim / Restore And Add Stuff / Environment Tests", {
  try(detach("package:unitizerdummypkg1", unload=TRUE))
  try(detach("package:unitizerdummypkg2", unload=TRUE))
  if(!inherits(try(get(".unitizer.tests.x", envir=.GlobalEnv), silent=TRUE), "try-error"))
    stop("Logic error, `.unitizer.tests.x` should not be defined.")
  assign(".unitizer.tests.x", 42, envir=.GlobalEnv)

  library(unitizerdummypkg1)
  library(unitizerdummypkg2)
  search.path.init <- search()

  # Clean path

  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_setup()
  expect_true(unitizer:::search_path_trim())

  # Make sure namespaces are working as expected

  testthat::expect_error(evalq(dummy_fun1(), pack.env$zero.env.par))
  testthat::expect_error(evalq(dummy_fun2(), pack.env$zero.env.par))
  library(unitizerdummypkg1)
  testthat::expect_identical(evalq(dummy_fun1(), pack.env$zero.env.par), NULL)
  testthat::expect_identical(evalq(dummy_fun1(), .GlobalEnv), NULL)

  # Confirm we can't see global env

  testthat::expect_error(evalq(.unitizer.tests.x, pack.env$zero.env.par))
  testthat::expect_identical(evalq(.unitizer.tests.x, .GlobalEnv), 42)
  rm(.unitizer.tests.x, envir=.GlobalEnv)

  # Add another package in weird position

  library(unitizerdummypkg2, pos=6L)
  testthat::expect_identical(evalq(dummy_fun2(), pack.env$zero.env.par), NULL)
  testthat::expect_identical(evalq(dummy_fun2(), .GlobalEnv), NULL)

  # restore

  testthat::expect_true(unitizer:::search_path_restore())
  expect_identical(search(), search.path.init)

  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_unsetup()    # not strictly necessary after restore()
} )
test_that("require / attach / detach", {
  try(detach("package:unitizerdummypkg1", unload=TRUE))
  try(detach("package:unitizerdummypkg2", unload=TRUE))
  if(!inherits(try(get(".unitizer.tests.x1", envir=.GlobalEnv), silent=TRUE), "try-error"))
    stop("Logic error, `.unitizer.tests.x1` should not be defined.")
  if(!inherits(try(get(".unitizer.tests.x2", envir=.GlobalEnv), silent=TRUE), "try-error"))
    stop("Logic error, `.unitizer.tests.x2` should not be defined.")
  assign(".unitizer.tests.x1", 42, envir=.GlobalEnv)
  assign(".unitizer.tests.x2", c("the", "answer"), envir=.GlobalEnv)

  # Add some objects to path

  library(unitizerdummypkg1)
  library(unitizerdummypkg2)

  a <- data.frame(.unitizer.tests.x1=1:10)
  b <- list(.unitizer.tests.x2=new.env())

  attach(a)
  attach(b)

  search.path.init <- search()

  # Clean path

  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_setup()
  expect_true(unitizer:::search_path_trim())
  testthat::expect_false(
    any(
      c("a", "b", "package:unitizerdummypkg1", "package:unitizerdummypkg2") %in%
      search()
  ) )
  # Test Require

  require(unitizerdummypkg1)
  pkg.char <- "unitizerdummypkg2"
  require(pkg.char, character.only=TRUE)
  testthat::expect_true(
    all(c("package:unitizerdummypkg1", "package:unitizerdummypkg2") %in% search())
  )
  # Test attach / detach

  testthat::expect_identical(evalq(.unitizer.tests.x1, .GlobalEnv), 42)
  testthat::expect_identical(evalq(.unitizer.tests.x2, .GlobalEnv), c("the", "answer"))

  testthat::expect_error(evalq(.unitizer.tests.x1, pack.env$zero.env.par))  # original a/b should be removed by trim
  testthat::expect_error(evalq(.unitizer.tests.x2, pack.env$zero.env.par))

  a1 <- data.frame(.unitizer.tests.x1=2:3)
  attach(a1, name="a")

  testthat::expect_identical(evalq(.unitizer.tests.x1, .GlobalEnv), 42)
  testthat::expect_identical(evalq(.unitizer.tests.x1, pack.env$zero.env.par), 2:3)

  b2 <- data.frame(.unitizer.tests.x1=999L, .unitizer.tests.x2="boo", stringsAsFactors=FALSE)
  attach(b2, name="a", pos=3L)  # Note purposefully writing to "a"

  testthat::expect_identical(evalq(.unitizer.tests.x1, pack.env$zero.env.par), 2:3)
  testthat::expect_identical(evalq(.unitizer.tests.x2, .GlobalEnv), c("the", "answer"))
  testthat::expect_identical(evalq(.unitizer.tests.x2, pack.env$zero.env.par), "boo")

  detach("a")  # should detach first "a"

  testthat::expect_identical(evalq(.unitizer.tests.x1, pack.env$zero.env.par), 999L)
  testthat::expect_identical(evalq(.unitizer.tests.x2, pack.env$zero.env.par), "boo")

  detach("a")  # should detach second "a"

  testthat::expect_error(evalq(.unitizer.tests.x1, pack.env$zero.env.par))  # original a/b should be removed by trim
  testthat::expect_error(evalq(.unitizer.tests.x2, pack.env$zero.env.par))

  # More package detach testing

  detach("package:unitizerdummypkg1")
  testthat::expect_false("package:unitizerdummypkg1" %in% search())
  testthat::expect_true("unitizerdummypkg1" %in% loadedNamespaces())

  detach("package:unitizerdummypkg2", unload=TRUE)
  testthat::expect_false("package:unitizerdummypkg2" %in% search())
  testthat::expect_false("unitizerdummypkg2" %in% loadedNamespaces())

  # cross fingers and restore...

  testthat::expect_true(unitizer:::search_path_restore())
  expect_identical(search(), search.path.init)

  rm(.unitizer.tests.x1, .unitizer.tests.x2, envir=.GlobalEnv)
  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_unsetup()  # not strictly necessary after restore()

  # Now early objects should be restored

  expect_identical(search(), search.path.init)
  expect_identical(.unitizer.tests.x1, 1:10)
  expect_identical(.unitizer.tests.x2, b$.unitizer.tests.x2)

  # Restore search path (this was done outside of serach path manip)

  detach("a")
  detach("b")
  detach("package:unitizerdummypkg1", unload=TRUE)
  detach("package:unitizerdummypkg2", unload=TRUE)
} )
test_that("Messing with path is detected", {
  try(detach("package:unitizerdummypkg1", unload=TRUE))
  try(detach("package:unitizerdummypkg2", unload=TRUE))

  search.path.init <- search()
  pack.env <- unitizer:::reset_packenv()
  unitizer:::search_path_setup()
  expect_true(unitizer:::search_path_trim())
  library(unitizerdummypkg1)
  tracingState(FALSE)
  library(unitizerdummypkg2)
  tracingState(TRUE)
  testthat::expect_warning(sp_rest <- unitizer:::search_path_restore())
  testthat::expect_false(sp_rest)
  testthat::expect_identical(unitizer_search_path_backup(), search.path.init)
  tracingState(FALSE)
  detach("package:unitizerdummypkg2", unload=TRUE)  # re-synchronize so we can restore path
  tracingState(TRUE)
  testthat::expect_true(unitizer:::search_path_restore())
})
try(detach("package:unitizerdummypkg1", unload=TRUE))
try(detach("package:unitizerdummypkg2", unload=TRUE))

message("COMPLETED zeroenv tests")
