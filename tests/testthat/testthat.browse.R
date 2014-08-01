library(unitizer)
library(testthat)

local( {
  obj.item <- new("unitizerItem", call=quote(1 + 1), env=new.env())
  obj.item@data@value <- 2 
  obj.item@data@output <- c("two", "dos", "due")
  obj.item@data@conditions <- new(
    "conditionList", .items=list(simpleError("hello"), simpleWarning("What a warning"))
  )
  obj.item@data@message <- vapply(
    unitizer:::as.list(obj.item@data@conditions), conditionMessage, character(1L)
  )
  obj.item@data@aborted <- TRUE
  .new <- obj.item@data@value

  par.env <- new.env()
  assign(".new", obj.item, par.env)
  child.env <- new.env(parent=par.env)
  assign(".new", .new, child.env)
  eval.env <- new.env(parent=child.env)

  test_that("unitizerItem accessor functions work", {
    expect_equal(evalq(unitizer:::getItemFuns[["getVal"]](.new), eval.env), obj.item@data@value)
    expect_equal(evalq(unitizer:::getItemFuns[["getOut"]](.new), eval.env), obj.item@data@output)
    expect_equal(evalq(unitizer:::getItemFuns[["getConds"]](.new), eval.env), obj.item@data@conditions)
    expect_equal(evalq(unitizer:::getItemFuns[["getTest"]](.new), eval.env), obj.item)

    ref <- .new
    assign(".ref", obj.item, par.env)
    assign(".ref", .new, eval.env)

    expect_equal(evalq(unitizer:::getItemFuns[["getMsg"]](.ref), eval.env), obj.item@data@message)
    expect_equal(evalq(unitizer:::getItemFuns[["getAborted"]](.ref), eval.env), obj.item@data@aborted)
  } )

  test_that("unitizerItem accessor handler function works and errors with incorrect inputs", {
    expect_equal(evalq(unitizer:::getItemData(.new, ".new", "value", par.env), eval.env), obj.item@data@value)
    expect_error(evalq(unitizer:::getItemData(.new, ".new", "value", eval.env), eval.env))
    expect_error(evalq(unitizer:::getItemData(.new, ".new", "vaLue", par.env), eval.env))
    expect_error(evalq(unitizer:::getItemData(.new, "object", "value", par.env), eval.env))
    item <- .new
    assign("item", obj.item, par.env)
    assign("item", .new, eval.env)
    expect_error(evalq(unitizer:::getItemFuns[["getConds"]](item), eval.env))
  } )
  # Create a bunch of expressions for testing, has to be done outside of 
  # `test_that`

  exps1 <- expression(
    library(stats),
    unitizer_sect("Section 1", {
      runif(20)
      stop("woohoo")
      "I'll be removed"
      "I too will be removed"
    } ),
    unitizer_sect("Section 2", {
      "I three will be removed"
      sample(20)
    } )
  )
  exps2 <- expression(
    library(stats),
    unitizer_sect("Section 1", {
      1 + 1
      runif(20)
      stop("woohoo")
      var <- 200
      matrix(1:9, 3)
    } ),
    unitizer_sect("Section 2", {
      1 + 20
      var1 <- list(1, 2, 3)
      sample(20)
      matrix(1:9, ncol=3)
      lm(x ~ y, data.frame(x=1:10, y=c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5)))
    } )
  )
  my.unitizer <- new("unitizer", id=1, zero.env=new.env())  
  my.unitizer <- my.unitizer + exps1
  my.unitizer2 <- new("unitizer", id=2, zero.env=new.env())
  my.unitizer2 <- my.unitizer2 + my.unitizer@items.new          # make previous items into reference items
  my.unitizer2 <- my.unitizer2 + exps2                          # now add back items to compare
  unitizer.prepped <- unitizer:::browsePrep(my.unitizer2)

  test_that("unitizerBrowse correctly processes unitizer for display", {
    # force all tests to be reviewed so they will be shown
    unitizer.prepped@mapping@reviewed <- rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    unitizer.prepped@mapping@review.val <- rep("Y", length(unitizer.prepped@mapping@reviewed))
    expect_equal(
      c("Section 1", "   1. runif(20) -----------------------------------  Failed:Y", "   2. 1 + 1 ---------------------------------------     New:Y", "   4. matrix(1:9, 3) ------------------------------     New:Y", "Section 2", "   5. sample(20) ----------------------------------  Failed:Y", "   6. 1 + 20 --------------------------------------     New:Y", "   8. matrix(1:9, ncol = 3) -----------------------     New:Y", "   9. lm(x ~ y, data.frame(x = 1:10, y = c(5, ... -     New:Y", "Removed Items", "  10. \"I'll be removed\" --------------------------- Removed:Y",  "  11. \"I too will be removed\" --------------------- Removed:Y", "  12. \"I three will be removed\" ------------------- Removed:Y"),
      as.character(unitizer.prepped, 60)
    )
    # Alternating tests
    unitizer.prepped@mapping@reviewed <- as.logical(seq(length(unitizer.prepped@mapping@reviewed)) %% 2)
    expect_equal(
      c("Section 1", "   1. runif(20) -----------------------------------  Failed:Y", "Section 2", "   5. sample(20) ----------------------------------  Failed:Y", "   9. lm(x ~ y, data.frame(x = 1:10, y = c(5, ... -     New:Y", "Removed Items", "  11. \"I too will be removed\" --------------------- Removed:Y"),
      as.character(unitizer.prepped, 60)
    )
  } )
  test_that("processInput generates Correct Item Structure", {
    # Here we just test that the calls of each item are what we expect, making 
    # sure that different behavior for Y or N depending on sub-section type is
    # observed correctly (e.g. a Y for new test means keep it, where as for 
    # removed test means don't keep it)

    unitizer.prepped@mapping@reviewed <- rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    unitizer.prepped@mapping@review.val <- rep("Y", length(unitizer.prepped@mapping@reviewed))
    # Assume user accepted all tests
    expect_equal(
      list(quote(runif(20)), quote(1 + 1), quote(var <- 200), quote(matrix(1:9, 3)), quote(sample(20)), quote(1 + 20), quote(var1 <- list(1, 2, 3)), quote(matrix(1:9, ncol = 3)), quote(lm(x ~ y, data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5))))),
      lapply(unitizer:::as.list(unitizer:::processInput(unitizer.prepped)), slot, "call")
    )
    # Assume user accepted all but 1, 4, 6 and 10
    unitizer.prepped@mapping@review.val[c(1, 4, 6, 10)] <- "N"
    expect_equal(
      list(quote(runif(20)), quote(1 + 1), quote(var <- 200), quote(sample(20)), quote(var1 <- list(1, 2, 3)), quote(matrix(1:9, ncol = 3)), quote(lm(x ~ y, data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5)))), "I'll be removed"),
      lapply(unitizer:::as.list(unitizer:::processInput(unitizer.prepped)), slot, "call")
    )
  } )

} )