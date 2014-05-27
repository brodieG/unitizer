library(testthat)
library(testor)

local( {
  lst <- new("testorList")
  test_that("testorList basic tests", {
    expect_identical(length(lst), 0L)
    expect_is(lst <- testor:::append(lst, 5), "testorList")
    expect_identical(length(lst), 1L)
    expect_is(lst <- testor:::append(lst, list("booyah", list(1:3), matrix(1:9, nrow=3))), "testorList")
    expect_identical(length(lst), 4L)
    expect_is(lst[3L], "testorList")
    expect_is(lst[[3L]], "list")
    lst <- testor:::append(lst, list(data.frame(a=letters[1:3])), 2L)
    expect_is(lst[[3L]], "data.frame")
    expect_identical(length(lst[1:4]), 4L)
    lst[[4L]] <- "boo"
    expect_is(lst[[4L]], "character")
    lst[4L:5L] <- letters[1:2]
    expect_identical(c(lst[[4L]], lst[[5L]]), letters[1:2])
    expect_identical(lst[[4L]], "a")
    expect_is(testor:::as.list(lst), "list")
    expect_identical(length(testor:::as.list(lst)), 5L)
    expect_is(testor:::as.expression(lst), "expression")
    expect_error(testor:::getItem(lst), "Internal pointer .* not initialized")
    lst <- testor:::nextItem(lst)
    expect_equal(testor:::getItem(lst), 5)
    lst <- testor:::nextItem(lst)
    expect_equal(testor:::getItem(lst), "booyah")
    lst <- testor:::prevItem(lst)
    expect_equal(testor:::getItem(lst), 5)
    lst <<- lst
  } )
  test_that("testorList pointer seeking", {
    for(i in 1:10) lst <- testor:::nextItem(lst)
    expect_error(testor:::getItem(lst), "Internal pointer .* corrupted")
    expect_true(testor:::done(lst))
    expect_is(lst <- testor:::reset(lst, "back"), "testorList")
    expect_error(testor:::reset(lst, letters), "Argument `position` must")
    expect_error(testor:::reset(lst, NA_character_), "Argument `position` must")
    expect_error(testor:::getItem(lst), "Internal pointer .* not initialized")
    lst <- testor:::prevItem(lst)
    expect_identical(testor:::getItem(lst), "b")

    while(!testor:::done(lst)) {
      item <- testor:::getItem(lst)
      lst <- testor:::prevItem(lst)
    }
    expect_equal(item, 5)
    expect_error(testor:::getItem(lst), "Internal pointer .* outside")
    withCallingHandlers(lst[[4]] <- "new value", warning=function() stop("A Warning!"))
    for(i in 1:5) lst <- testor:::nextItem(lst)
    expect_equal(lst@.pointer, 5)
    lst <<- lst

  } )
  test_that("testorList value replacement and pointer adjustments", {
    lst[[4]] <- NULL
    expect_equal(lst@.pointer, 4)
    testor:::reset(lst, "back")
    lst.len <- length(lst)
    expect_equal(lst@.pointer, lst.len)
    lst[2:3] <- letters[1:2]
    expect_equal(lst@.pointer, lst.len)
    lst[2:3] <- list(NULL, NULL)
    expect_equal(lst@.pointer, lst.len)
    lst[2:3] <- NULL
    expect_equal(lst@.pointer, lst.len - 2L)
    lst <- testor:::reset(lst, "front")
    for(i in 1:2) lst <- testor:::nextItem(lst)
    curr.point <- lst@.pointer
    lst[[3]] <- NULL
    expect_equal(curr.point, lst@.pointer)
    lst <- testor:::append(lst, list(5, 6, "blaskdjf"), 1L)
    expect_equal(curr.point + 3L, lst@.pointer)
    lst <- testor:::append(lst, list(matrix(1:9, nrow=3)), 5L)
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
} )