library(unitizer)
context("Diff")
local({

  test.obj.s3 <- structure("hello", class="test_obj")
  setClass("testObj", list(a="character"))
  test.obj.s4 <- new("testObj", a="goodday")
  print.test_obj <- function(x, ...) stop("Error in Print")
  setMethod("show", "testObj", function(object) stop("Error in Show"))

  # These have to be outside of error handlers

  oc1 <- unitizer:::obj_capt(test.obj.s3)
  oc2 <- unitizer:::obj_capt(test.obj.s4)
  do1 <- unitizer:::diff_obj_internal(
    test.obj.s3, test.obj.s3, width=60L, context=c(10L, 5L), file=stdout()
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
      c("<Error in print method for object of class \"test_obj\">",  "Error in Print"),
      oc1
    )
    expect_equal(
      c("<Error in show method for object of class \"testObj\">",  "Error in Show"),
      oc2
    )
  } )
  old.opt <- options(unitizer.color=TRUE, width=60L)
  test_that("diff", {
    mx.1 <- matrix(1:9, nrow=3)
    mx.2 <- matrix(1:100, ncol=2)
    set.seed(1, "Mersenne-Twister")
    mx.3 <- matrix(runif(100), ncol=2)
    mx.3 <- mx.2
    mx.3[31, 2] <- 111L

    mx.1.2.str.ref <- c("\033[36m@@ str(mx.1, max.level = 1L) @@\033[39m", "\033[31m-  \033[39m int \033[31m[1:3,\033[39m \033[31m1:3]\033[39m 1 2 3 4 5 6 7 8 9", "\033[36m@@ str(mx.2, max.level = 1L) @@\033[39m", "\033[32m+  \033[39m int \033[32m[1:50,\033[39m \033[32m1:2]\033[39m 1 2 3 4 5 6 7 8 9 \033[32m10\033[39m \033[32m...\033[39m")
    mx.1.2.prn.ref <- c("\033[36m@@ mx.1 @@\033[39m", "\033[31m-  \033[39m     [,1] [,2] \033[31m[,3]\033[39m", "\033[31m-  \033[39m\033[31m[1,]    1    4    7\033[39m", "\033[31m-  \033[39m\033[31m[2,]\033[39m    \033[31m2\033[39m    \033[31m5\033[39m    \033[31m8\033[39m", "\033[31m-  \033[39m\033[31m[3,]\033[39m    \033[31m3\033[39m    \033[31m6\033[39m    \033[31m9\033[39m", "\033[36m@@ mx.2 @@\033[39m", "\033[32m+  \033[39m      [,1] [,2]", "\033[32m+  \033[39m \033[32m[1,]\033[39m    \033[32m1\033[39m   \033[32m51\033[39m",  "\033[32m+  \033[39m \033[32m[2,]\033[39m    \033[32m2\033[39m   \033[32m52\033[39m", "\033[32m+  \033[39m\033[32m [3,]    3   53\033[39m", "\033[32m+  \033[39m\033[32m [4,]    4   54\033[39m", "\033[90m   ~~ omitted 46 lines w/ 46 diffs ~~\033[39m")

    # should be choosing str

    expect_identical(diff_str(mx.1, mx.2, context=c(10L, 2L)), mx.1.2.str.ref)
    expect_identical(diff_print(mx.1, mx.2, context=c(10L, 2L)), mx.1.2.prn.ref)
    expect_identical(diff_obj(mx.1, mx.2, context=c(10L, 2L)), mx.1.2.str.ref)

    mx.2.3.prn.ref <- c("\033[36m@@ mx.2 @@\033[39m", "\033[90m   ~~ omitted 29 lines w/o diffs ~~\033[39m", "   [29,]   29   79", "   [30,]   30   80", "\033[31m-  \033[39m[31,]   31   \033[31m81\033[39m", "   [32,]   32   82", "   [33,]   33   83", "\033[90m   ~~ omitted 17 lines w/o diffs ~~\033[39m", "\033[36m@@ mx.3 @@\033[39m", "\033[90m   ~~ omitted 29 lines w/o diffs ~~\033[39m", "   [29,]   29   79", "   [30,]   30   80", "\033[32m+  \033[39m[31,]   31  \033[32m111\033[39m", "   [32,]   32   82", "   [33,]   33   83",  "\033[90m   ~~ omitted 17 lines w/o diffs ~~\033[39m")

    expect_identical(diff_obj(mx.2, mx.3, context=c(10L, 2L)), mx.2.3.prn.ref)
    expect_identical(diff_print(mx.2, mx.3, context=c(10L, 2L)), mx.2.3.prn.ref)

    # List tests

    expect_identical(
      diff_obj(mx.3[1:6, ], mx.3[1:5, ], context=c(10L, 2L)),
      c("\033[36m@@ mx.3[1:6, ] @@\033[39m", "        [,1] [,2]", "   [1,]    1   51", "   [2,]    2   52", "   [3,]    3   53", "   [4,]    4   54", "   [5,]    5   55", "\033[31m-  \033[39m\033[31m[6,]    6   56\033[39m", "\033[36m@@ mx.3[1:5, ] @@\033[39m", "        [,1] [,2]", "   [1,]    1   51", "   [2,]    2   52", "   [3,]    3   53", "   [4,]    4   54", "   [5,]    5   55")
    )
    expect_identical(
      diff_obj(mx.3[1:6, ], mx.3[2:6, ], context=c(10L, 2L)),
      c("\033[36m@@ mx.3[1:6, ] @@\033[39m", "        [,1] [,2]", "\033[31m-  \033[39m[1,]    \033[31m1\033[39m   \033[31m51\033[39m", "\033[31m-  \033[39m[2,]    \033[31m2\033[39m   \033[31m52\033[39m", "\033[31m-  \033[39m[3,]    \033[31m3\033[39m   \033[31m53\033[39m", "\033[31m-  \033[39m[4,]    \033[31m4\033[39m   \033[31m54\033[39m", "\033[31m-  \033[39m[5,]    \033[31m5\033[39m   \033[31m55\033[39m", "\033[31m-  \033[39m\033[31m[6,]    6   56\033[39m", "\033[36m@@ mx.3[2:6, ] @@\033[39m", "        [,1] [,2]",  "\033[32m+  \033[39m[1,]    \033[32m2\033[39m   \033[32m52\033[39m", "\033[32m+  \033[39m[2,]    \033[32m3\033[39m   \033[32m53\033[39m", "\033[32m+  \033[39m[3,]    \033[32m4\033[39m   \033[32m54\033[39m", "\033[32m+  \033[39m[4,]    \033[32m5\033[39m   \033[32m55\033[39m", "\033[32m+  \033[39m[5,]    \033[32m6\033[39m   \033[32m56\033[39m")
    )
    expect_identical(
      diff_obj(mx.3[1:6, ], mx.3[1:6, ], context=c(10L, 2L)),
      "No visible differences between objects"
    )
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
    lst.2$z[[4L]] <- matrix(12:1, ncol=3)
    expect_identical(
      diff_obj(lst.1, lst.2, context=c(10, 5)),
      c("\033[36m@@ str(lst.1, max.level = 5) @@\033[39m", "   List of 2", "    $  : NULL", "    $ z:List of 4", "     ..$  :List of 1", "\033[31m-  \033[39m  .. ..$ : chr [1:3] \"a\" \033[31m\"b\"\033[39m \"c\"", "     ..$  :List of 1", "     .. ..$ : NULL", "     ..$ z:List of 4", "     .. ..$  : int [1:3] 1 2 3", "     .. ..$  : num 1", "     .. ..$  : num 2", "     .. ..$ z:List of 2", "     .. .. ..$  : num 1", "     .. .. ..$ z:List of 1", "\033[31m-  \033[39m  .. .. .. ..$ z: num \033[31m5\033[39m",  "\033[31m-  \033[39m  ..$  : int \033[31m[1:3,\033[39m 1:3] \033[31m1\033[39m \033[31m2\033[39m 3 \033[31m4\033[39m \033[31m5\033[39m \033[31m6\033[39m \033[31m7\033[39m \033[31m8\033[39m \033[31m9\033[39m", "\033[36m@@ str(lst.2, max.level = 5) @@\033[39m", "   List of 2", "    $  : NULL", "    $ z:List of 4", "     ..$  :List of 1", "\033[32m+  \033[39m  .. ..$ : chr [1:3] \"a\" \033[32m\"bananas\"\033[39m \"c\"", "     ..$  :List of 1", "     .. ..$ : NULL", "     ..$ z:List of 4", "     .. ..$  : int [1:3] 1 2 3",  "     .. ..$  : num 1", "     .. ..$  : num 2", "     .. ..$ z:List of 2", "     .. .. ..$  : num 1", "     .. .. ..$ z:List of 1", "\033[32m+  \033[39m  .. .. .. ..$ z: num \033[32m6\033[39m", "\033[32m+  \033[39m  ..$  : int \033[32m[1:4,\033[39m 1:3] \033[32m12\033[39m \033[32m11\033[39m \033[32m10\033[39m \033[32m9\033[39m \033[32m8\033[39m \033[32m7\033[39m \033[32m6\033[39m \033[32m5\033[39m \033[32m4\033[39m 3 \033[32m...\033[39m")
    )
    expect_identical(
      diff_obj(lst.1, lst.2, context=c(2, 1)),
      c("\033[36m@@ lst.1 @@\033[39m", "\033[90m   ~~ omitted 5 lines w/o diffs ~~\033[39m", "   $z[[1]][[1]]", "\033[31m-  \033[39m[1] \"a\" \033[31m\"b\"\033[39m \"c\"", "   ", "\033[90m   ~~ omitted 34 lines w/ 4 diffs ~~\033[39m", "\033[36m@@ lst.2 @@\033[39m", "\033[90m   ~~ omitted 5 lines w/o diffs ~~\033[39m", "   $z[[1]][[1]]", "\033[32m+  \033[39m[1] \"a\"       \033[32m\"bananas\"\033[39m \"c\"", "   ", "\033[90m   ~~ omitted 35 lines w/ 5 diffs ~~\033[39m")
    )
  } )
  set.seed(2)
  words <- c(
    "carrot", "cat", "cake", "eat", "rabbit", "holes", "the", "a", "pasta",
    "boom", "noon", "sky", "parenthesis", "blah"
  )
  nums <- runif(5, -1e9, 1e9)
  scinums <- format(c(nums, 1/nums), scientific=TRUE)
  other <- c(paste0(sample(1:200, 5), "%"), "5.34e-8", "-2.534e6")
  wl <- c(words, nums, scinums, other)

  # Initial sample

  s1 <- s2 <- s3 <- s4 <- sample(wl, 20, replace=T)
  s2 <- s1[5:20]                             # subset
  s3[sample(seq_along(s1), 10)] <- sample(s1, 10)     # change some
  s4 <- c(s1[1:5], sample(s1, 2), s1[6:15], sample(s1, 2), s1[16:20])

  test_that("brackets", {
    unitizer:::find_brackets(capture.output(1:100))

  } )
  test_that("diff_word", {
    # Make sure not fooled by repeats of same tokens in same string

    expect_identical(
      unitizer:::diff_word(
        "[1] \"`1:3` should be length 5 (is 3)\"",
        "[1] \"should be length 5 (is 3)\""
      ),
      structure(list(target = "[1] \033[31m\"`\033[39m\033[31m1\033[39m\033[31m:\033[39m\033[31m3\033[39m\033[31m`\033[39m should be length 5 (is 3)\"", current = "[1] \033[32m\"\033[39mshould be length 5 (is 3)\""), .Names = c("target", "current"))
    )
    # Test `across.lines`

    a <- c("a b", "c d")
    b <- c("b c", "d e")
    expect_identical(
      unitizer:::diff_word(a, b, across.lines=TRUE),
      structure(list(target = c("\033[31ma\033[39m \033[31mb\033[39m c d", "e f g h"), current = c("c d e f", "g h \033[32mi\033[39m \033[32mj\033[39m")), .Names = c("target", "current"))
    )
    a <- c("x a b", "c d z")
    b <- c("x b c", "d e z")
    expect_identical(
      unitizer:::diff_word(a, b, across.lines=TRUE),
      structure(list(target = c("x \033[31ma\033[39m b", "c d z"), current = c("x b c", "d \033[32me\033[39m z")), .Names = c("target", "current"))
    )
    a <- c("x a b", "c d z")
    b <- c("z b c", "d e x")
    expect_identical(
      unitizer:::diff_word(a, b, across.lines=TRUE),
      structure(list(target = c("x \033[31ma\033[39m \033[31mb\033[39m", "\033[31mc\033[39m \033[31md\033[39m \033[31mz\033[39m"), current = c("\033[32mz\033[39m \033[32mb\033[39m \033[32mc\033[39m", "\033[32md\033[39m \033[32me\033[39m x")), .Names = c("target", "current"))
    )
    lapply(
      unitizer:::diff_word(b, a, across.lines=TRUE),
      cat, sep="\n"
    )
  }
  options(old.opt)
  test_that("char_diff", {
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b", "c")),
      new(
        "unitizerDiffDiffs", target=integer(3L),
        current=integer(3L)
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b"), c("a", "b", "c")),
      new(
        "unitizerDiffDiffs", target=integer(2L),
        current=c(0L, 0L, NA)
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b")),
      new(
        "unitizerDiffDiffs", target=c(0L, 0L, NA),
        current=integer(2L)
    ) )
    expect_identical(
      unitizer:::char_diff(c("b", "c"), c("a", "b")),
      new(
        "unitizerDiffDiffs", target=c(0L, NA),
        current=c(NA, 0L)
    ) )
    expect_identical(
      unitizer:::char_diff(letters[1:3], letters[2:4]),
      new(
        "unitizerDiffDiffs", target=c(NA, 0L, 0L),
        current=c(0L, 0L, NA)
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c", "d"), c("a", "b", "b", "d", "e")),
      new(
        "unitizerDiffDiffs", target=c(0L, 0L, 1L, 0L),
        current=c(0L, 0L, 1L, 0L, NA)
    ) )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b", "d")),
      new(
        "unitizerDiffDiffs", target=c(0L, 0L, 1L),
        current=c(0L, 0L, 1L)
    ) )
  })
  test_that("Rdiff_obj", {
    a <- matrix(1:3, ncol=1)
    b <- matrix(c(1, 3, 2), ncol=1)
    expect_identical(
      capture.output(res <- Rdiff_obj(a, b)),
      c("", "3c3", "< [2,]    2", "---", "> [2,]    3", "4c4", "< [3,]    3",  "---", "> [3,]    2")
    )
    expect_equal(res, 1)
    expect_identical(capture.output(Rdiff_obj(a, a)), character())
    expect_equal(Rdiff_obj(a, a), 0)

    # Try with RDS object

    f <- tempfile()
    saveRDS(a, f)
    expect_identical(
      capture.output(res <- Rdiff_obj(f, b)),
      c("", "3c3", "< [2,]    2", "---", "> [2,]    3", "4c4", "< [3,]    3",  "---", "> [3,]    2")
    )
    expect_equal(res, 1)
    expect_identical(capture.output(Rdiff_obj(f, f)), character())
    expect_equal(Rdiff_obj(a, a), 0)
    unlink(f)
  })}
)
