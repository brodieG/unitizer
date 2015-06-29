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
  do1 <- unitizer:::diff_obj_out(test.obj.s3, test.obj.s3, width=60L, max.len=c(10L, 5L), file=stdout())

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
      c("@@ mx.1 @@", "-        [,1] [,2] [,3]", "-   [1,]    1    4    7", "-   [2,]    2    5    8", "-   [3,]    3    6    9", "@@ mx.2 @@", "+         [,1] [,2]", "+    [1,]    1   51", "+    [2,]    2   52", "+    [3,]    3   53", "+    [4,]    4   54", "+    [5,]    5   55", "+    [6,]    6   56", "+    [7,]    7   57", "+    [8,]    8   58", "+    [9,]    9   59", "+   ... omitted 41 lines; see `mx.2` ..."),
      unitizer:::diff_obj_out(mx.1, mx.2, width=60L, max.len=c(10L, 5L), file=stdout())
    )
    expect_identical(
      c("@@ mx.2 @@", "-   ... omitted 31 lines ...", "-   [31,]   31   81", "-   [32,]   32   82", "-   [33,]   33   83", "-   [34,]   34   84", "-   [35,]   35   85", "-   ... omitted 15 lines; see `mx.2` ...", "@@ mx.3 @@", "+   ... omitted 31 lines ...", "+   [31,]   31  111", "+   [32,]   32   82", "+   [33,]   33   83", "+   [34,]   34   84", "+   [35,]   35   85", "+   ... omitted 15 lines; see `mx.3` ..."),
      unitizer:::diff_obj_out(mx.2, mx.3, width=60L, max.len=c(10L, 5L), file=stdout())
    )
    expect_identical(
      c("@@ test.obj.s3 @@", "-   <Error in print/show method for object of class \"test_obj\">", "-   Error in Print", "@@ test.obj.s3 @@", "+   <Error in print/show method for object of class \"test_obj\">", "+   Error in Print"),
      do1
    )
  } )
  test_that("cap_first", {
    set.seed(1, "Mersenne-Twister")
    words <- replicate(2, paste0(sample(letters, 5), collapse=""))
    WORDS <- toupper(words)
    expect_identical(
      c("", "A", "B", "Y", "Z", "Gjnue", "Xzpob", "GJNUE", "XZPOB"),
      unitizer:::cap_first(c("", letters[1:2], letters[25:26], words, WORDS))
    )
    expect_identical(
      c("", "a", "b", "y", "z", "gjnue", "xzpob", "gJNUE", "xZPOB"),
      unitizer:::decap_first(c("", letters[1:2], letters[25:26], words, WORDS))
    )
  })
  test_that("word_wrap", {
    lorem1 <- "Today, with Kiernan on the stand offering confirmation, Howard walked the jury through the enormous amount of data pulled from Ulbricht's computer. Defense lawyers haven't had a chance yet to respond to this evidence—that will likely come tomorrow. The mountain they have to climb looks higher than ever, though. Last week, Ulbricht's lawyer outlined a defense in which Ulbricht walked away from the marketplace he created and was \"lured back.\" But what will explain the dozens of folders of data on this laptop, with data from the upper echelons of Silk Road management—mixed with the most intimate details of Ulbricht's personal life?"
    lorem2 <- "/Volumes/FIXED/folder1/folder2/folder.2345/folderabac/file.text.batch"

    expect_equal(
      c(18L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 25L), -1L)))
    )
    expect_equal(
      c(23L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 25L, 3L), -1L)))
    )
    expect_identical(
      c("Today, with Kiernan on the stand offering co-", "nfirmation, Howard walked the jury through ", "the enormous amount of data pulled from Ulb-", "richt's computer."),
      unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L)
    )
    expect_identical(
      c("Today, with Kiernan on the stand offering con", "firmation, Howard walked the jury through the", "enormous amount of data pulled from Ulbricht'", "s computer."),
      unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L, FALSE)
    )
    expect_identical(
      c("/Volumes/FIXED/", "folder1/folder2", "/folder.2345/", "folderabac/file", ".text.batch"),
      unitizer:::word_wrap(lorem2, 15L, 3L)
    )
    expect_identical(
      c("/Volumes/FIXED/", "folder1/", "folder2/folder.", "2345/folderabac", "/file.text.", "batch"),
      unitizer:::word_wrap(lorem2, 15L, 8L)
    )
    expect_identical(
      c("hello ", "sunset ", "", "there ", "moonrise"),
      unitizer:::word_wrap("hello sunset \nthere moonrise", width=12L)
    )
    x1 <- c("this is supposed to be a particularly long string\nthat allows us to test the behavior of bullets once we start seeing\nsome wrapping kicking in which was a problem once upon a time")
    expect_identical(
      unitizer:::word_wrap(x1, unlist=FALSE, width=80L),
      list(c("this is supposed to be a particularly long string", "", "that allows us to test the behavior of bullets once we start seeing", "", "some wrapping kicking in which was a problem once upon a time"))
    )
  })
  test_that("bullets", {
    x <- c("there was once a time when the fantastic unicorns could fly", "bugs bunny ate carrots and drank milk while hunting ducks")
    xx <- unitizer:::UL(x)
    expect_identical(
      c("- there was once a time when ", "  the fantastic unicorns ", "  could fly", "- bugs bunny ate carrots and ", "  drank milk while hunting ", "  ducks"),
      as.character(xx, width=30L)
    )
  })
  test_that("substr_const", {
    expect_equal(unitizer:::substr_cons(c("ab", "abcde", "abce"), 4L), c("ab  ", "abcd", "abc "))
    expect_equal(unitizer:::substr_cons(c("ab", "abcde", "abce"), 4L, justify="right"), c("  ab", "abcd", " abc"))
    expect_equal(unitizer:::substr_cons(c("NEW", "PASS", "FAIL", "DELETED", "Error"), 4L), c("NEW ", "PASS", "FAIL", "DEL ", "Err "))
  })
  test_that("str_reduce_unique", {
    str1 <- c("abcdef", "abcdefgh", "abcql")
    res1 <- c("def", "defgh", "ql")
    expect_equal(unitizer:::str_reduce_unique(str1), res1)
    expect_equal(unitizer:::str_reduce_unique(str1, from="right"), str1)
    str2 <- vapply(
      strsplit(str1, ""), function(x) paste0(rev(x), collapse=""), ""
    )
    res2 <- vapply(
      strsplit(res1, ""), function(x) paste0(rev(x), collapse=""), ""
    )
    expect_equal(unitizer:::str_reduce_unique(str2, from="right"), res2)
    expect_equal(unitizer:::str_reduce_unique("aaa"), "")
    expect_equal(unitizer:::str_reduce_unique(rep("aaa", 5L)), rep("", 5L))
  })
  test_that("strtrunc", {
    str1 <- c(paste0(letters, collapse=""), paste0(LETTERS, collapse=""))
    expect_equal(unitizer:::strtrunc(str1, 10L), c("abcdefg...", "ABCDEFG..."))
    expect_equal(unitizer:::strtrunc(str1, 10L, from="left"), c("...tuvwxyz", "...TUVWXYZ"))
  })

})
