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
  # Create a bunch of expressions for testing, has to be done outside of 
  # `test_that`

  exps1 <- expression(
    library(stats),
    testor_sect("Section 1", {
      runif(20)
      stop("woohoo")
    } ),
    testor_sect("Section 2", {
      sample(20)
    } )
  )
  exps2 <- expression(
    library(stats),
    testor_sect("Section 1", {
      1 + 1
      runif(20)
      stop("woohoo")
      var <- 200
      matrix(1:9, 3)
    } ),
    testor_sect("Section 2", {
      1 + 20
      var1 <- list(1, 2, 3)
      sample(20)
      matrix(1:9, ncol=3)
      lm(x ~ y, data.frame(x=1:10, y=c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5)))
    } )
  )
  my.testor <- new("testor", id=1, zero.env=new.env())  
  my.testor <- my.testor + exps1
  my.testor2 <- new("testor", id=2, zero.env=new.env())
  my.testor2 <- my.testor2 + my.testor@items.new          # make previous items into reference items
  my.testor2 <- my.testor2 + exps2                        # now add back items to compare

  test_that("testorBrowse correctly processes testor for display", {
    # force all tests to be reviewed so they will be shown
    testor.prepped <- testor:::browsePrep(my.testor2)
    testor.prepped@mapping@reviewed <- rep(TRUE, length(testor.prepped@mapping@reviewed))
    expect_equal(
      c("Section 1", "  1. runif(20) ------------------------------------ Failed:Y", "  2. 1 + 1 ----------------------------------------    New:Y", "  4. matrix(1:9, 3) -------------------------------    New:Y", "Section 2", "  5. sample(20) ----------------------------------- Failed:Y", "  6. 1 + 20 ---------------------------------------    New:Y", "  8. matrix(1:9, ncol = 3) ------------------------    New:Y", "  9. lm(x ~ y, data.frame(x = 1:10, y = c(5, 3... -    New:Y"),
      as.character(testor.prepped, 60)
    )
    # Alternating tests
    testor.prepped@mapping@reviewed <- as.logical(seq(length(testor.prepped@mapping@reviewed)) %% 2)
    expect_equal(
      c("Section 1", "  1. runif(20) ------------------------------------ Failed:Y", "Section 2", "  5. sample(20) ----------------------------------- Failed:Y", "  9. lm(x ~ y, data.frame(x = 1:10, y = c(5, 3... -    New:Y"),
      as.character(testor.prepped, 60)
    )
  } )

} )