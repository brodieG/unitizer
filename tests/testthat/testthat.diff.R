library(unitizer)
context("Diff")
local({
  mx.1 <- matrix(1:9, nrow=3)
  mx.2 <- matrix(1:100, ncol=2)
  mx.3 <- mx.2
  mx.3[31, 2] <- 111L

  test.obj.s3 <- structure("hello", class="test_obj")
  setClass("testObj", list(a="character"))
  test.obj.s4 <- new("testObj", a="goodday")
  print.test_obj <- function(x, ...) stop("Error in Print")
  setMethod("show", "testObj", function(object) stop("Error in Show"))

  # These have to be outside of error handlers

  oc1 <- unitizer:::obj_capt(test.obj.s3)
  oc2 <- unitizer:::obj_capt(test.obj.s4)
  do1 <- unitizer:::diff_obj_out(
    test.obj.s3, test.obj.s3, width=60L, max.len=c(10L, 5L), file=stdout()
  )

  test_that("S4 objs", {
    x <- new(
      "unitizerDiff", tar.capt=letters, cur.capt=letters,
      tar.exp=quote(letters), cur.exp=quote(letters),
      diffs=new(
        "unitizerDiffDiffs",
        target=c(rep(TRUE, 25), FALSE),
        current=c(FALSE, rep(TRUE, 25))
      ), mode="str"
    )
    y <- x
    y@diffs@target <- rep(FALSE, 26)
    y@diffs@current <- rep(FALSE, 26)
    expect_true(any(x))
    expect_false(any(y))
    x@tar.capt <- letters[1:5]
    expect_error(validObject(x), "same length")
    y@cur.capt <- letters[1:5]
    expect_error(validObject(y), "same length")
  } )
  test_that("capt with print errors", {
    expect_equal(
      c("<Error in print/show method for object of class \"test_obj\">",  "Error in Print"),
      oc1
    )
    expect_equal(
      c("<Error in print/show method for object of class \"testObj\">",  "Error in Show"),
      oc2
    )
  } )
  test_that("diff", {
    expect_identical(
      unitizer:::diff_obj_out(mx.1, mx.2, width=60L, max.len=c(10L, 5L), file=stdout()),
      c("@@ mx.1 @@", "-       [,1] [,2] [,3]", "-  [1,]    1    4    7", "-  [2,]    2    5    8", "-  [3,]    3    6    9", "@@ mx.2 @@", "+        [,1] [,2]", "+   [1,]    1   51", "+   [2,]    2   52", "+   [3,]    3   53", "+   [4,]    4   54", "+   [5,]    5   55", "+   [6,]    6   56", "+   [7,]    7   57", "+   [8,]    8   58", "+   [9,]    9   59", "   ... omitted 41 lines; see `mx.2` ...")
    )
    expect_identical(
      unitizer:::diff_obj_out(mx.2, mx.3, width=60L, max.len=c(10L, 5L), file=stdout()),
      c("@@ mx.2 @@", "   ... omitted 31 lines ...", "-  [31,]   31   81", "   [32,]   32   82", "   [33,]   33   83", "   [34,]   34   84", "   [35,]   35   85", "   ... omitted 15 lines; see `mx.2` ...", "@@ mx.3 @@", "   ... omitted 31 lines ...", "+  [31,]   31  111", "   [32,]   32   82", "   [33,]   33   83", "   [34,]   34   84", "   [35,]   35   85", "   ... omitted 15 lines; see `mx.3` ...")
    )
    expect_identical(
      do1,
      c("@@ test.obj.s3 @@", "   <Error in print/show method for object of class \"test_obj\">", "   Error in Print", "@@ test.obj.s3 @@", "   <Error in print/show method for object of class \"test_obj\">", "   Error in Print")
    )
    # More tests
    set.seed(1, "Mersenne-Twister")
    mx.3 <- matrix(runif(100), ncol=2)
    stop("these tests not fully formulated")
    diff_obj(mx.3[1:6, ], mx.3[1:5, ])
    diff_obj(mx.3[1:6, ], mx.3[2:6, ])  # indeces different...
    lst.1 <- list(
      NULL,
      z=list(
        list(letters[1:3]), list(NULL),
        z=list(1:3, 1, 2, z=list(1, z=list(z=5))),
        matrix(1:9, 3)
    ) )
    lst.2 <- lst.1
    lst.2$z$z$z$z$z <- 6
    lst.2$z[[1L]][[1L]][2L] <- "bananas"
    lst.2$z[[4L]] <- matrix(9:1, 3)
    diff_obj(lst.1, lst.2, context=c(10, 5))
    diff_obj(lst.1, lst.2, context=c(2, 1))
  } )
} )
