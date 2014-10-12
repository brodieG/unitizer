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
test_that("Package added properly", {
  search.path <- search()
  unitizer:::unitizer_library(unitizerdummypkg1)
  expect_equal(new.pack <- setdiff(search(), search.path), "package:unitizerdummypkg1")
  expect_equal(new.pack, names(unitizer:::pack.env$objects.attached))  

  # And only once

  unitizer:::unitizer_library(unitizerdummypkg1)
  expect_equal(new.pack <- setdiff(search(), search.path), "package:unitizerdummypkg1")
  expect_equal(new.pack, names(unitizer:::pack.env$objects.attached))
} )
test_that("Don't add if already in search path", {
  library(unitizerdummypkg2)
  search.path <- search()
  unitizer:::unitizer_library(unitizerdummypkg2)
  expect_equal(new.pack <- setdiff(search(), search.path), character())

  # Require too

  try(detach("package:unitizerdummypkg2", unload=TRUE))
  search.path <- search()
  unitizer:::unitizer_require(unitizerdummypkg2)
  expect_equal(new.pack <- setdiff(search(), search.path), "package:unitizerdummypkg2")
  expect_equal(
    c("package:unitizerdummypkg1", "package:unitizerdummypkg2"), 
    names(unitizer:::pack.env$objects.attached)
  )
} )
test_that("Trim and Reset Search Path", {
  try(detach("package:unitizer", unload=TRUE))
  try(detach("package:unitizerdummypkg1", unload=TRUE))
  try(detach("package:unitizerdummypkg2", unload=TRUE))

  library(unitizer)
  library(unitizerdummypkg1)  # at least one pack to explicitly remove

  search.pre <- search()
  search.pre.objs <- lapply(search.pre, as.environment)

  testthat::expect_identical(unitizer:::search_path_trim(), NULL)  # note this actually removes testthat from search path...

  search.post <- search()
  search.base <- c(
    ".GlobalEnv", "package:unitizer", "package:stats", "package:graphics",  
    "package:grDevices", "package:utils", "package:datasets", "package:methods",  
    "Autoloads", "package:base"
  )
  testthat::expect_true(
    identical(search.post, search.base) || 
    identical(search.post, append(search.base, "tools:rstudio", 2L))
  )
  testthat::expect_true(
    "package:unitizerdummypkg1" %in% 
    vapply(unitizer:::pack.env$objects.detached, `[[`, "", "name")
  )
  unitizer:::search_path_restore()
  expect_identical(search.pre, search())

  # Can't do identical b/c environments are reloaded and as such are different
  # but this should at least compare attributes and all that

  expect_equal(search.pre.objs, lapply(search(), as.environment))
} )
