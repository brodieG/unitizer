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
      1 + 1
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
  unitizer.prepped <- unitizer:::browsePrep(my.unitizer2, mode="unitize")

  test_that("unitizerBrowse correctly processes unitizer for display", {
    # force all tests to be reviewed so they will be shown
    unitizer.prepped@mapping@reviewed <- rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    unitizer.prepped@mapping@review.val <- rep("Y", length(unitizer.prepped@mapping@reviewed))
    expect_equal(
      c("= <untitled> ===================================================================================================\n", "    *1. library(stats) .  .  .  .  .  .  .  .  .         -:Y\n", "= Section 1 ====================================================================================================\n", "     5. 1 + 1 .  .  .  .  .  .  .  .  .  .  .  .    Passed:Y\n", "     2. runif(20)   .  .  .  .  .  .  .  .  .  .    Failed:Y\n", "     6. stop(\"woohoo\") .  .  .  .  .  .  .  .  .    Passed:Y\n", "    *3. var <- 200  .  .  .  .  .  .  .  .  .  .         -:Y\n", "     4. matrix(1:9, 3) .  .  .  .  .  .  .  .  .       New:Y\n", "= Section 2 ====================================================================================================\n", "     8. 1 + 20   .  .  .  .  .  .  .  .  .  .  .       New:Y\n", "    *9. var1 <- list(1, 2, 3)   .  .  .  .  .  .         -:Y\n", "     7. sample(20)  .  .  .  .  .  .  .  .  .  .    Failed:Y\n", "    10. matrix(1:9, ncol = 3)   .  .  .  .  .  .       New:Y\n", "    11. lm(x ~ y, data.frame(x = 1:10, y = c(5,...     New:Y\n", "= Removed Items ================================================================================================\n", "    12. \"I'll be removed\" .  .  .  .  .  .  .  .   Removed:Y\n", "    13. \"I too will be removed\" .  .  .  .  .  .   Removed:Y\n", "    14. \"I three will be removed\"  .  .  .  .  .   Removed:Y\n"),
      as.character(unitizer.prepped, 60)
    )
    # Alternating tests
    unitizer.prepped@mapping@reviewed <- as.logical(seq(length(unitizer.prepped@mapping@reviewed)) %% 2)
    expect_equal(
      c("= <untitled> ===================================================================================================\n", "    *1. library(stats) .  .  .  .  .  .  .  .  .         -:Y\n", "= Section 1 ====================================================================================================\n", "     5. 1 + 1 .  .  .  .  .  .  .  .  .  .  .  .    Passed:Y\n", "     2. runif(20)   .  .  .  .  .  .  .  .  .  .    Failed:-\n", "     6. stop(\"woohoo\") .  .  .  .  .  .  .  .  .    Passed:-\n", "    *3. var <- 200  .  .  .  .  .  .  .  .  .  .         -:Y\n", "     4. matrix(1:9, 3) .  .  .  .  .  .  .  .  .       New:-\n", "= Section 2 ====================================================================================================\n", "     8. 1 + 20   .  .  .  .  .  .  .  .  .  .  .       New:-\n", "    *9. var1 <- list(1, 2, 3)   .  .  .  .  .  .         -:Y\n", "     7. sample(20)  .  .  .  .  .  .  .  .  .  .    Failed:Y\n", "    10. matrix(1:9, ncol = 3)   .  .  .  .  .  .       New:-\n", "    11. lm(x ~ y, data.frame(x = 1:10, y = c(5,...     New:Y\n", "= Removed Items ================================================================================================\n", "    12. \"I'll be removed\" .  .  .  .  .  .  .  .   Removed:-\n", "    13. \"I too will be removed\" .  .  .  .  .  .   Removed:Y\n", "    14. \"I three will be removed\"  .  .  .  .  .   Removed:-\n"),
      as.character(unitizer.prepped, 60)
    )
  } )
  test_that("processInput generates Correct Item Structure", {
    # Here we just test that the calls of each item are what we expect, making
    # sure that different behavior for Y or N depending on sub-section type is
    # observed correctly (e.g. a Y for new test means keep it, where as for
    # removed test means don't keep it)

    # For debugging:
    # cbind(substr(unitizer:::deparseCalls(unitizer.prepped), 1, 15), as.character(unitizer.prepped@mapping@review.type), unitizer.prepped@mapping@review.val, unitizer.prepped@mapping@reviewed)
    # cat(deparse(width=500,
    #   lapply(
    #     unitizer:::as.list(unitizer:::processInput(unitizer.prepped)),
    #     function(x) call("quote", slot(x, "call")))
    # ) )

    unitizer.prepped@mapping@reviewed <- rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    unitizer.prepped@mapping@review.val <- rep("Y", length(unitizer.prepped@mapping@reviewed))
    # Assume user accepted all tests
    expect_equal(
      list(quote(runif(20)), quote(var <- 200), quote(matrix(1:9, 3)), quote(sample(20)), quote(1 + 20), quote(var1 <- list(1, 2, 3)), quote(matrix(1:9, ncol = 3)), quote(lm(x ~ y, data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5))))),
      lapply(unitizer:::as.list(unitizer:::processInput(unitizer.prepped)), slot, "call")
    )
    # Assume user accepted all but 1, 4, 6 and 11, note it isn't completely obvious
    # what should be kept since an N for anything but a new test will result in
    # some object remaining in the list (typically the reference copy thereof)
    unitizer.prepped@mapping@review.val[] <- "N"
    unitizer.prepped@mapping@review.val[c(2, 6, 8, 12)] <- "Y"
    expect_equal(
      list(quote(library(stats)), quote(runif(20)), quote(1 + 1), quote(sample(20)), quote(1 + 20), quote("I too will be removed"), quote("I three will be removed")),
      lapply(unitizer:::as.list(unitizer:::processInput(unitizer.prepped)), slot, "call")
    )
  } )
  test_that("Reference section mapping works", {

    # Copy over just two sections

    my.unitizer3 <-
      new("unitizer", id=3, zero.env=new.env()) +
      my.unitizer2@items.new[-(2:6)]  # Exclude section two tests
    my.unitizer3 <- unitizer:::refSections(my.unitizer3, my.unitizer2)  # sections should copy over

    expect_identical(my.unitizer3@sections.ref, my.unitizer2@sections[-2])  # just copy over 1st and 3rd sections
    expect_identical(my.unitizer3@section.ref.map, c(1L, rep(2L, 5L)))

    # Make sure "removed" sections are NA when kept

    unitizer.prepped@mapping@reviewed <- rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    unitizer.prepped@mapping@review.val <- ifelse(unitizer.prepped@mapping@review.type %in% c("Passed", "Removed"), "N", "Y")  # don't delete removed or passed
    items.processed <- unitizer:::processInput(unitizer.prepped)

    expect_identical(
      c(1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, NA, NA, NA),
      vapply(unitizer:::as.list(items.processed), slot, 1L, "section.id")
    )
    # Now try to re-establish sections with removed tests

    my.unitizer4 <- new("unitizer", id=4, zero.env=new.env()) + items.processed
    my.unitizer4 <- unitizer:::refSections(my.unitizer4, my.unitizer2)  # sections should copy over

    expect_true(is(my.unitizer4@sections.ref[[4L]], "unitizerSectionNA"))
    expect_identical(
      c(1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L),
      my.unitizer4@section.ref.map
    )
  } )
  test_that("Item Extraction", {
    items <- unitizer:::extractItems(unitizer.prepped)
    item.calls <- vapply(unitizer:::as.list(items), function(x) paste0(deparse(x@call, width=500), collapse=""), character(1L))
    item.types <- vapply(unitizer:::as.list(items), slot, FALSE, "reference")
    item.ids <- vapply(unitizer:::as.list(items), slot, 1L, "id")
    item.df <- data.frame(item.calls, item.types, item.ids, stringsAsFactors=FALSE)
    expect_identical(
      structure(list(item.calls = c("library(stats)", "1 + 1", "runif(20)", "stop(\"woohoo\")", "var <- 200", "matrix(1:9, 3)", "1 + 20", "var1 <- list(1, 2, 3)", "sample(20)", "matrix(1:9, ncol = 3)", "lm(x ~ y, data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5)))", "\"I'll be removed\"", "\"I too will be removed\"", "\"I three will be removed\""), item.types = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE), item.ids = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 5L, 6L, 7L)), .Names = c("item.calls", "item.types", "item.ids"), row.names = c(1L, 5L, 2L, 6L, 3L, 4L, 8L, 9L, 7L, 10L, 11L, 12L, 13L, 14L), class = "data.frame"),
      item.df[order(item.types, item.ids),]
    )
  } )
} )
