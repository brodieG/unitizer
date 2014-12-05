library(unitizer)
library(testthat)

local({
  toy.stor <- readRDS("../interactive/unitizer/misc.unitizer/data.rds")
  test_that("Error Cases", {
    expect_error(get_unitizer(1), "No method defined")
    expect_error(get_unitizer(letters), "Argument `store.id`")
    expect_error(get_unitizer("../interactive"), "a directory containing`unitizer` objects")
    expect_error(get_unitizer("testthat.get.R"), "Argument `store.id` must refer to a directory")
    expect_error(set_unitizer(1), "No method defined")
    expect_error(set_unitizer(letters), "Argument `store.id` must be a 1 length character vector")
    expect_error(set_unitizer("a"), "argument \"unitizer\" is missing, with no default")
    expect_error(set_unitizer("a", "blergh"), "Argument `unitizer` must be a unitizer")
    expect_true(!file.exists("a"))
    expect_error(set_unitizer("tests/# ;!./# \\/", toy.stor), "Could not create")
  } )
  test_that("Get works as expected", {
    expect_false(get_unitizer("asldkfjskfa"))
    expect_equal(get_unitizer("../interactive/unitizer/misc.unitizer"), toy.stor)
    expect_true(is(toy.stor, "unitizer"))
  } )
  tmp.dir <- tempdir()
  tmp.sub.dir <- paste0(tmp.dir, "/get.test.dir")
  tmp.sub.dir2 <- paste0(tmp.dir, "/get.test.dir2")

  test_that("Set works as expected", {
    dir.create(tmp.sub.dir)
    expect_true(set_unitizer(tmp.sub.dir, toy.stor))
    expect_equal(readRDS(paste0(tmp.sub.dir, "/data.rds")), toy.stor)
  } )
  test_that("load/store_unitizer", {
    par.frame <- new.env()

    expect_true(is(unitizer:::load_unitizer(tmp.sub.dir, par.frame), "unitizer"))
    expect_true(is(untz <- unitizer:::load_unitizer(tmp.sub.dir2, par.frame), "unitizer"))  # empty folder, but this should still create unitizer
    expect_identical(parent.env(untz@zero.env), par.frame)
    untz@id <- "bananas"

    wd <- getwd()
    expect_true(unitizer:::store_unitizer(untz, tmp.sub.dir2, wd))
    expect_identical(getwd(), wd)
    expect_equal(unitizer:::load_unitizer(tmp.sub.dir2, par.frame)@id, "bananas")
  } )
  file.remove(paste0(tmp.sub.dir, "/data.rds"))
  file.remove(tmp.sub.dir)
  file.remove(paste0(tmp.sub.dir2, "/data.rds"))
  file.remove(tmp.sub.dir2)
  print("random print to flush warnings")
} )
