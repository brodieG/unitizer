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
      "I'll be removed"
      "I too will be removed"
    } ),
    testor_sect("Section 2", {
      "I three will be removed"
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
  testor.prepped <- testor:::browsePrep(my.testor2)

  test_that("testorBrowse correctly processes testor for display", {
    # force all tests to be reviewed so they will be shown
    testor.prepped@mapping@reviewed <- rep(TRUE, length(testor.prepped@mapping@reviewed))
    expect_equal(
      c("Section 1", "   1. runif(20) -----------------------------------  Failed:Y", "   2. 1 + 1 ---------------------------------------     New:Y", "   4. matrix(1:9, 3) ------------------------------     New:Y", "Section 2", "   5. sample(20) ----------------------------------  Failed:Y", "   6. 1 + 20 --------------------------------------     New:Y", "   8. matrix(1:9, ncol = 3) -----------------------     New:Y", "   9. lm(x ~ y, data.frame(x = 1:10, y = c(5, ... -     New:Y", "Removed Items", "  10. \"I'll be removed\" --------------------------- Removed:Y",  "  11. \"I too will be removed\" --------------------- Removed:Y", "  12. \"I three will be removed\" ------------------- Removed:Y"),
      as.character(testor.prepped, 60)
    )
    # Alternating tests
    testor.prepped@mapping@reviewed <- as.logical(seq(length(testor.prepped@mapping@reviewed)) %% 2)
    expect_equal(
      c("Section 1", "   1. runif(20) -----------------------------------  Failed:Y", "Section 2", "   5. sample(20) ----------------------------------  Failed:Y", "   9. lm(x ~ y, data.frame(x = 1:10, y = c(5, ... -     New:Y", "Removed Items", "  11. \"I too will be removed\" --------------------- Removed:Y"),
      as.character(testor.prepped, 60)
    )
  } )
  test_that("processInput generates Correct Item Structure", {
    # Here we just test that the calls of each item are what we expect, making 
    # sure that different behavior for Y or N depending on sub-section type is
    # observed correctly (e.g. a Y for new test means keep it, where as for 
    # removed test means don't keep it)

    testor.prepped@mapping@reviewed <- rep(TRUE, length(testor.prepped@mapping@reviewed))
    # Assume user accepted all tests
    expect_equal(
      list(quote(runif(20)), quote(1 + 1), quote(var <- 200), quote(matrix(1:9, 3)), quote(sample(20)), quote(1 + 20), quote(var1 <- list(1, 2, 3)), quote(matrix(1:9, ncol = 3)), quote(lm(x ~ y, data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5))))),
      lapply(testor:::as.list(testor:::processInput(testor.prepped)), slot, "call")
    )
    # Assume user accepted all but 1, 4, 6 and 10
    testor.prepped@mapping@review.val[c(1, 4, 6, 10)] <- "N"
    expect_equal(
      list(quote(runif(20)), quote(1 + 1), quote(var <- 200), quote(sample(20)), quote(var1 <- list(1, 2, 3)), quote(matrix(1:9, ncol = 3)), quote(lm(x ~ y, data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5)))), "I'll be removed"),
      lapply(testor:::as.list(testor:::processInput(testor.prepped)), slot, "call")
    )
  } )

} )