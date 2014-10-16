library(testthat)

try(detach("package:unitizer", unload=TRUE))
try(detach("package:unitizerdummypkg1", unload=TRUE))
try(detach("package:unitizerdummypkg2", unload=TRUE))

library(unitizer)

test_that("Detecting packages", {
  expect_true(unitizer:::is.loaded_package("package:unitizer"))
  expect_error(unitizer:::is.loaded_package("unitizer"))
  expect_true(unitizer:::is.loaded_package("package:stats"))
  expect_error(unitizer:::is.loaded_package(1))
  expect_error(unitizer:::is.loaded_package(letters))
  expect_error(unitizer:::is.loaded_package("Autoloads"))
} )
test_that("Package shimming working", {

  search.path.init <- search()

  pack.env <- unitizer:::reset_packenv()
  expect_true(unitizer:::search_path_setup())
  expect_warning(sps <- unitizer:::search_path_setup())    # Can't re-shim
  expect_false(sps)
  expect_identical(unitizer:::search_path_unsetup(), NULL) # undo shimming
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
