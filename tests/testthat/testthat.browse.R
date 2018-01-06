library(unitizer)
library(testthat)
context("Browse")

local( {
  old.opt.outer <- options(unitizer.color=FALSE)
  on.exit(options(old.opt.outer))

  zero.env <- parent.env(.GlobalEnv)
  obj.item <- new("unitizerItem", call=quote(1 + 1), env=new.env())
  obj.item@data@value <- list(2)
  obj.item@data@output <- c("two", "dos", "due")
  obj.item@data@conditions <- new(
    "conditionList",
    .items=list(simpleError("hello"), simpleWarning("What a warning"))
  )
  obj.item@data@message <- vapply(
    unitizer:::as.list(obj.item@data@conditions), conditionMessage, character(1L)
  )
  obj.item@data@aborted <- TRUE

  test_that("unitizerItem accessor functions work", {
    expect_equal(obj.item$value, obj.item@data@value[[1L]])
    expect_equal(obj.item$output, obj.item@data@output)
    expect_equal(obj.item$conditions, obj.item@data@conditions)
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
  my.unitizer <- new("unitizer", id=1, zero.env=zero.env)
  capture_output(my.unitizer <- my.unitizer + exps1)
  my.unitizer2 <- new("unitizer", id=2, zero.env=zero.env)
  # make previous items into reference items
  capture_output(my.unitizer2 <- my.unitizer2 + my.unitizer@items.new)
  # now add back items to compare
  capture.output(my.unitizer2 <- my.unitizer2 + exps2)
  unitizer.prepped <- unitizer:::browsePrep(my.unitizer2, mode="unitize")

  # NOTE: for some reason, changes in between revisions d9619db and a46e941
  # should have caused the tests to fail, but didn't.  We did not notice
  # failures until we ran tests quite a bit later at ca9f540364.  Not sure why
  # this happened.  The failures were due to the order of tests changing because
  # we moved ignored tests to be in the same sub-section as the subsequent non-
  # ignored tests

  test_that("Can convert to data.frame", {
    expect_equal_to_reference(
      unitizer:::as.data.frame(unitizer.prepped),
      file.path("helper", "refobjs", "browse_df1.rds")
    )
  })
  test_that("unitizerBrowse correctly processes unitizer for display", {
    # force all tests to be reviewed so they will be shown
    unitizer.prepped@mapping@reviewed <-
      rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    unitizer.prepped@mapping@review.val <-
      rep("Y", length(unitizer.prepped@mapping@reviewed))

    expect_equal_to_reference(
      as.character(unitizer.prepped, 60),
      file.path("helper", "refobjs", "browse_aschar1.rds")
    )
    # Alternating tests
    unitizer.prepped@mapping@reviewed <- as.logical(
      seq(length(unitizer.prepped@mapping@reviewed)) %% 2
    )
    expect_equal_to_reference(
      as.character(unitizer.prepped, 60),
      file.path("helper", "refobjs", "browse_aschar2.rds")
    )
    # Errors / warnings

    expect_error(as.character(unitizer.prepped, -1), 'positive')
    expect_warning(
      prep.narrow <- as.character(unitizer.prepped, 5), 'width too small'
    )
    expect_equal_to_reference(
      prep.narrow, file.path("helper", "refobjs", "browse_ascharnarrow.rds")
    )

    # Colors work (should be last in this section) since the reference @global

    unitizer.prepped@global$unitizer.opts[["unitizer.color"]] <- TRUE
    old.opt <- options(crayon.enabled=TRUE)
    on.exit({
      options(old.opt)
      unitizer.prepped@global$unitizer.opts[["unitizer.color"]] <- FALSE
    })
    prep.color <- as.character(unitizer.prepped, 60)
    expect_equal_to_reference(
      prep.color, file.path("helper", "refobjs", "browse_aschar3.rds")
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

    unitizer.prepped@mapping@reviewed <-
      rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    unitizer.prepped@mapping@review.val <-
      rep("Y", length(unitizer.prepped@mapping@reviewed))
      #
    # Assume user accepted all tests
    expect_equal(
      lapply(
        unitizer:::as.list(unitizer:::processInput(unitizer.prepped)),
        slot, "call"
      ),
      list(
        quote(library(stats)),
        quote(runif(20)),
        quote(var <- 200),
        quote(matrix(1:9, 3)),
        quote(1 + 1),
        quote(stop("woohoo")),
        quote(var1 <- list(1, 2, 3)),
        quote(sample(20)),
        quote(1 + 20),
        quote(matrix(1:9, ncol = 3)),
        quote(
          lm(x ~ y,
            data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2, 1, 4, 1.5)))))
    )
    # Assume user accepted all but 1, 4, 6 and 11, note it isn't completely
    # obvious what should be kept since an N for anything but a new and passed
    # test will result in some object remaining in the list (typically the
    # reference copy thereof)

    unitizer.prepped@mapping@review.val[] <- "N"
    unitizer.prepped@mapping@review.val[c(2, 6, 8, 12)] <- "Y"
    expect_equal(
      lapply(unitizer:::as.list(unitizer:::processInput(unitizer.prepped)), slot, "call"),
      list(quote(runif(20)), quote(stop("woohoo")), quote("I'll be removed"), quote(sample(20)), quote(matrix(1:9, ncol = 3)), quote("I three will be removed"))
    )
  } )
  test_that("unitizerBrowse subsetting works", {
    # note single bracket subsetting for `unitizerBrowse` overrides the `unitizerList`
    # subsetting

    expect_equal(
      unitizer:::deparseCalls(unitizer:::extractItems(unitizer.prepped[c(4, 8, 10)])),
      c("matrix(1:9, 3)", "\"I too will be removed\"", "sample(20)")
    )
    expect_equal(
      unitizer:::deparseCalls(unitizer:::extractItems(unitizer.prepped[c(2, 3, 11)])),
      c("runif(20)", "var <- 200", "1 + 20")
    )
  })
  test_that("Reference section mapping works", {

    # Copy over just two sections

    my.unitizer3 <-
      new("unitizer", id=3, zero.env=zero.env) +
      my.unitizer2@items.new[-(2:6)]  # Exclude section two tests

    # sections should copy over
    my.unitizer3 <- unitizer:::refSections(my.unitizer3, my.unitizer2)

    # just copy over 1st and 3rd sections
    expect_identical(my.unitizer3@sections.ref, my.unitizer2@sections[-2])
    expect_identical(my.unitizer3@section.ref.map, c(1L, rep(2L, 5L)))

    # Make sure "removed" sections are NA when kept
    unitizer.prepped@mapping@reviewed <-
      rep(TRUE, length(unitizer.prepped@mapping@reviewed))
    # don't delete removed
    unitizer.prepped@mapping@review.val <-
      ifelse(unitizer.prepped@mapping@review.type %in% c("Removed"), "N", "Y")
    items.processed <- unitizer:::processInput(unitizer.prepped)

    expect_identical(
      vapply(unitizer:::as.list(items.processed), slot, 1L, "section.id"),
      c(1L, 2L, 2L, 2L, 2L, 2L, NA, NA, 3L, 3L, 3L, 3L, 3L, NA)
    )
    # Now try to re-establish sections with removed tests

    my.unitizer4 <- new("unitizer", id=4, zero.env=zero.env) + items.processed

    # sections should copy over
    my.unitizer4 <- unitizer:::refSections(my.unitizer4, my.unitizer2)

    expect_true(is(my.unitizer4@sections.ref[[4L]], "unitizerSectionNA"))
    expect_identical(
      my.unitizer4@section.ref.map,
      c(1L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 3L, 4L)
    )
  } )
  test_that("Item Extraction", {
    items <- unitizer:::extractItems(unitizer.prepped)
    item.calls <- vapply(unitizer:::as.list(items), function(x) paste0(deparse(x@call, width=500), collapse=""), character(1L))
    item.types <- vapply(unitizer:::as.list(items), slot, FALSE, "reference")
    item.ids <- vapply(unitizer:::as.list(items), slot, 1L, "id")
    item.df <- data.frame(item.calls, item.types, item.ids, stringsAsFactors=FALSE)
    expect_equal_to_reference(
      item.df[order(item.types, item.ids),],
      file.path("helper", "refobjs", "browse_itemord.rds")
    )
  } )
} )
