library(testor)
library(testthat)

local( {
  obj.item <- new("testorItem", call=quote(1 + 1))
  obj.item@data@value <- 2 
  obj.item@data@output <- c("two", "dos", "due")
  obj.item@data@conditions <- list(simpleError("hello"), simpleWarning("What a warning"))
  obj.item@data@message <- vapply(obj.item@data@conditions, conditionMessage, character(1L))
  obj.item@data@aborted <- TRUE
  .new <- obj.item@data@value

  par.env <- new.env()
  assign(".new", obj.item, par.env)
  child.env <- new.env(parent=par.env)
  assign(".new", .new, child.env)
  eval.env <- new.env(parent=child.env)

  test_that("testorItem accessor functions work", {
    expect_equal(evalq(testor:::getItemFuns[["getVal"]](.new), eval.env), obj.item@data@value)
    expect_equal(evalq(testor:::getItemFuns[["getOut"]](.new), eval.env), obj.item@data@output)
    expect_equal(evalq(testor:::getItemFuns[["getConds"]](.new), eval.env), obj.item@data@conditions)
    expect_equal(evalq(testor:::getItemFuns[["getTest"]](.new), eval.env), obj.item)

    ref <- .new
    assign(".ref", obj.item, par.env)
    assign(".ref", .new, eval.env)

    expect_equal(evalq(testor:::getItemFuns[["getMsg"]](.ref), eval.env), obj.item@data@message)
    expect_equal(evalq(testor:::getItemFuns[["getAborted"]](.ref), eval.env), obj.item@data@aborted)
  } )

  test_that("testorItem accessor handler function works and errors with incorrect inputs", {
    expect_equal(evalq(testor:::getItemData(.new, ".new", "value", par.env), eval.env), obj.item@data@value)
    expect_error(evalq(testor:::getItemData(.new, ".new", "value", eval.env), eval.env))
    expect_error(evalq(testor:::getItemData(.new, ".new", "vaLue", par.env), eval.env))
    expect_error(evalq(testor:::getItemData(.new, "object", "value", par.env), eval.env))
    item <- .new
    assign("item", obj.item, par.env)
    assign("item", .new, eval.env)
    expect_error(evalq(testor:::getItemFuns[["getConds"]](item), eval.env))
  } )
  # Create 

  items <- expression(
    library(stats),
    testor_sect("Section 1", {
      1 + 1
      runif(20)
      matrix(1:9, 3)
    } )
    testor_sect("Section 2", {
      1 + 20
      sample(20)
      matrix(1:9, ncol=3)
      lm(x ~ y, data.frame(x=1:10, y=c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5)))
    } )
  )
  my.testor <- new("testor", id=1, zero.env=new.env())
  browsePrep(my.testor)
} )