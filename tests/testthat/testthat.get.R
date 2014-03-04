library(testor)
library(testthat)

local({
  toy.stor <- readRDS("../interactive/testor/misc.testor.rds")
  test_that("Error Cases", {
    expect_error(get_store(1), "No method defined")
    expect_error(get_store(letters), "Argument `store.id`")
    expect_error(get_store("../interactive"))
    expect_error(get_store("../../R/get.R"), "Failed loading testor")
    expect_error(set_store(1), "No method defined")
    expect_error(set_store(letters), "Argument `store.id` must be")
    expect_error(set_store("../random.file.rds", "blergh"), "Argument `testor` must be")
    expect_true(!file.exists("../random.file.rds"))
    expect_error(set_store("tests/# ;!./# \\/", toy.stor), "Could not create")
  } )
  test_that("Get works as expected", {
    expect_false(get_store("../asldkfjskfa"))
    expect_equal(get_store("../interactive/testor/misc.testor.rds"), toy.stor)
    expect_true(is(toy.stor, "testor"))
  } )
  test_that("Set works as expected", {
    tmp <- tempfile()
    expect_true(set_store(tmp, toy.stor))
    expect_equal(readRDS(tmp), toy.stor)
    file.remove(tmp)
  } )
} )
