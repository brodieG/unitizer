library(testthat)
library(unitizer)
context("List")

local( {
  lst <- new("unitizerList")
  test_that("unitizerList basic tests", {
    expect_identical(length(lst), 0L)
    expect_is(lst <- unitizer:::append(lst, 5), "unitizerList")
    expect_identical(length(lst), 1L)
    expect_is(lst <- unitizer:::append(lst, list("booyah", list(1:3), matrix(1:9, nrow=3))), "unitizerList")
    expect_identical(length(lst), 4L)
    expect_is(lst[3L], "unitizerList")
    expect_is(lst[[3L]], "list")
    lst <- unitizer:::append(lst, list(data.frame(a=letters[1:3])), 2L)
    expect_is(lst[[3L]], "data.frame")
    expect_identical(length(lst[1:4]), 4L)
    lst[[4L]] <- "boo"
    expect_is(lst[[4L]], "character")
    lst[4L:5L] <- letters[1:2]
    expect_identical(c(lst[[4L]], lst[[5L]]), letters[1:2])
    expect_identical(lst[[4L]], "a")
    expect_is(unitizer:::as.list(lst), "list")
    expect_identical(length(unitizer:::as.list(lst)), 5L)
    expect_is(unitizer:::as.expression(lst), "expression")
    expect_error(unitizer:::getItem(lst), "Internal pointer .* not initialized")
    lst <- unitizer:::nextItem(lst)
    expect_equal(unitizer:::getItem(lst), 5)
    lst <- unitizer:::nextItem(lst)
    expect_equal(unitizer:::getItem(lst), "booyah")
    lst <- unitizer:::prevItem(lst)
    expect_equal(unitizer:::getItem(lst), 5)
    lst <<- lst
  } )
  test_that("unitizerList pointer seeking", {
    for(i in 1:10) lst <- unitizer:::nextItem(lst)
    expect_error(unitizer:::getItem(lst), "Internal pointer .* corrupted")
    expect_true(unitizer:::done(lst))
    expect_is(lst <- unitizer:::reset(lst, "back"), "unitizerList")
    expect_error(unitizer:::reset(lst, letters), "Argument `position` must")
    expect_error(unitizer:::reset(lst, NA_character_), "Argument `position` must")
    expect_error(unitizer:::getItem(lst), "Internal pointer .* not initialized")
    lst <- unitizer:::prevItem(lst)
    expect_identical(unitizer:::getItem(lst), "b")

    while(!unitizer:::done(lst)) {
      item <- unitizer:::getItem(lst)
      lst <- unitizer:::prevItem(lst)
    }
    expect_equal(item, 5)
    expect_error(unitizer:::getItem(lst), "Internal pointer .* outside")
    withCallingHandlers(lst[[4]] <- "new value", warning=function() stop("A Warning!"))
    for(i in 1:5) lst <- unitizer:::nextItem(lst)
    expect_equal(lst@.pointer, 5)
    lst <<- lst

  } )
  test_that("unitizerList value replacement and pointer adjustments", {
    lst[[4]] <- NULL
    expect_equal(lst@.pointer, 4)
    unitizer:::reset(lst, "back")
    lst.len <- length(lst)
    expect_equal(lst@.pointer, lst.len)
    lst[2:3] <- letters[1:2]
    expect_equal(lst@.pointer, lst.len)
    lst[2:3] <- list(NULL, NULL)
    expect_equal(lst@.pointer, lst.len)
    lst[2:3] <- NULL
    expect_equal(lst@.pointer, lst.len - 2L)
    lst <- unitizer:::reset(lst, "front")
    for(i in 1:2) lst <- unitizer:::nextItem(lst)
    curr.point <- lst@.pointer
    lst[[3]] <- NULL
    expect_equal(curr.point, lst@.pointer)
    lst <- unitizer:::append(lst, list(5, 6, "blaskdjf"), 1L)
    expect_equal(curr.point + 3L, lst@.pointer)
    lst <- unitizer:::append(lst, list(matrix(1:9, nrow=3)), 5L)
    expect_equal(curr.point + 3L, lst@.pointer)
  } )
  test_that("Append Factors Works", {
    vec <- factor(letters[1:3], levels=letters)
    vec2 <- factor(letters[10:15], levels=letters)

    expect_equal(
      structure(c(1L, 2L, 3L, 10L, 11L, 12L, 13L, 14L, 15L), .Label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"), class = "factor"),
      append(vec, vec2)
    )
    expect_equal(
      structure(c(1L, 2L, 10L, 11L, 12L, 13L, 14L, 15L, 3L), .Label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"), class = "factor"),
      append(vec, vec2, 2)
    )
    expect_equal(
      structure(c(10L, 11L, 12L, 13L, 1L, 2L, 3L, 14L, 15L), .Label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"), class = "factor"),
      append(vec2, vec, 4)
    )
    expect_error(append(vec2, vec, 20))
    expect_error(append(vec2, vec, -5))
  } )
  test_that("List coersion works even inside apply functions", {
    ulist <- new("unitizerList", .items=list("a", 1, 2, "b"))
    expect_identical(lapply(ulist, identity), ulist@.items)
  })
  test_that("Errors", {
    setClass("uhtsdfoqiuerhzb", slots=c(a='integer'))
    dummy <- new("uhtsdfoqiuerhzb", a=1L)

    lst2 <- new('unitizerList', .items=list(1, 2, 3))
    expect_error(append(lst2, 5, after = -1), "greater than zero")
    expect_error(
      capture.output(append(lst2, dummy), type='message'), "Unable to coerce"
    )

    lst3 <- new('unitizerList', .items=expression(1, 2, 3))
    expect_error(
      capture.output(append(lst3, dummy), type='message'),
      "Unable to coerce"
    )
  })

  test_that("Set Names", {
    nlst <- new("unitizerList", .items=list(a="a", b="b"))
    names(nlst) <- toupper(names(nlst))
    expect_equal(as.list(nlst), list(A="a", B="b"))
  })
} )
