library(unitizer)
context("Text")
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
    lorem3 <- "\"untz.state.test\", \"add.smooth\", \"bitmapType\", \"browser\", \"browserNLdisabled\", \"CBoundsCheck\", \"check.bounds\", \"citation.bibtex.max\", \"continue\", \"contrasts\""

    expect_equal(
      c(18L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 25L), -1L)))
    )
    expect_equal(
      c(23L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 25L, 3L), -1L)))
    )
    expect_identical(
      unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L),
      c("Today, with Kiernan on the stand offering co-", "nfirmation, Howard walked the jury through ", "the enormous amount of data pulled from Ulb-", "richt's computer.")
    )
    expect_identical(
      unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L, FALSE),
      c("Today, with Kiernan on the stand offering con", "firmation, Howard walked the jury through the", "enormous amount of data pulled from Ulbricht'", "s computer.")
    )
    expect_identical(
      unitizer:::word_wrap(lorem2, 15L, 3L),
      c("/Volumes/FIXED/", "folder1/fol-", "der2/folder.23-", "45/folderabac/", "file.text.batch")
    )
    expect_identical(
      unitizer:::word_wrap(lorem2, 15L, 8L),
      c("/Volumes/FIXED/", "folder1/", "folder2/folder.", "2345/fol-", "derabac/file.", "text.batch")

    )
    expect_identical(
      unitizer:::word_wrap(lorem3, 76L, 8L),
      c("\"untz.state.test\", \"add.smooth\", \"bitmapType\", \"browser\", \"browserNLdisab-", "led\", \"CBoundsCheck\", \"check.bounds\", \"citation.bibtex.max\", \"continue\", ", "\"contrasts\"")
    )
    expect_identical(
      unitizer:::word_wrap("hello sunset \nthere moonrise", width=12L),
      c("hello sunset", "", "there ", "moonrise")
    )
    x1 <- c("this is supposed to be a particularly long string\nthat allows us to test the behavior of bullets once we start seeing\nsome wrapping kicking in which was a problem once upon a time")
    expect_identical(
      unitizer:::word_wrap(x1, unlist=FALSE, width=80L),
      list(c("this is supposed to be a particularly long string", "", "that allows us to test the behavior of bullets once we start seeing", "", "some wrapping kicking in which was a problem once upon a time"))
    )
    com <- "# this is supposed to be a relatively long comment that will get re-flowed"
    expect_identical(
      unitizer:::word_comment(com, width=30L),
      c("# this is supposed to be a ", "#relatively long comment that ", "#will get re-flowed")
    )
  })
  test_that("bullets", {
    x <- c("there was once a time when the fantastic unicorns could fly", "bugs bunny ate carrots and drank milk while hunting ducks")
    xx <- unitizer:::UL(x)
    expect_identical(
      as.character(xx, width=30L),
      c("- there was once a time when ", "  the fantastic unicorns could", "  fly", "- bugs bunny ate carrots and ", "  drank milk while hunting ", "  ducks")
    )
    expect_identical(
      capture.output(print(xx, width=80L)),
      c("- there was once a time when the fantastic unicorns could fly", "- bugs bunny ate carrots and drank milk while hunting ducks")
    )
    yy <- unitizer:::OL(x)
    expect_identical(
      as.character(yy, width=30L),
      c("1. there was once a time when ", "   the fantastic unicorns ",  "   could fly", "2. bugs bunny ate carrots and ", "   drank milk while hunting ",  "   ducks"),
    )
    expect_identical(
      as.character(unitizer:::OL(rep(letters, 2), style="LETTERS")),
      c(" A. a", " B. b", " C. c", " D. d", " E. e", " F. f", " G. g", " H. h", " I. i", " J. j", " K. k", " L. l", " M. m", " N. n", " O. o", " P. p", " Q. q", " R. r", " S. s", " T. t", " U. u", " V. v", " W. w", " X. x", " Y. y", " Z. z", "AA. a", "AB. b", "AC. c", "AD. d", "AE. e", "AF. f", "AG. g", "AH. h", "AI. i", "AJ. j", "AK. k", "AL. l", "AM. m", "AN. n", "AO. o", "AP. p", "AQ. q", "AR. r", "AS. s", "AT. t", "AU. u", "AV. v", "AW. w", "AX. x", "AY. y", "AZ. z")
    )
    xl <- as.list(x)
    y <- unitizer:::UL(
      c(xl, list(unitizer:::OL(c(xl, list(unitizer:::UL(x))))), "yowza it is raining toads today!")
    )
    expect_identical(
      as.character(y, width=30),
      c("- there was once a time when ", "  the fantastic unicorns could", "  fly", "- bugs bunny ate carrots and ", "  drank milk while hunting ", "  ducks", "  1. there was once a time ", "     when the fantastic ", "     unicorns could fly", "  2. bugs bunny ate carrots ", "     and drank milk while ", "     hunting ducks", "    - there was once a time ", "      when the fantastic ", "      unicorns could fly", "    - bugs bunny ate carrots ", "      and drank milk while ", "      hunting ducks", "- yowza it is raining toads ",  "  today!")
    )
    expect_error(unitizer:::as.character.bullet(hello, 1:10))
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
  test_that("oneline", {
    dep <- c(
      "res <- data %>% group_by(ID) %>% summarise(date2 = nth(date, ",
      "    2), time2 = nth(time, 2), first_date = first(date), last_date = last(date), ",
      "    first_time = first(time), last_time = last(time))"
    )
    expect_equal(
      unitizer:::one_line(dep),
      "res <- data %>% group_by(ID) %>% summarise(date2 = nth(date, 2), time2 = nth(time, 2), first_date = first(date), last_date = last(date), first_time = first(time), last_time = last(time))"
    )
    expect_equal(
      unitizer:::one_line(dep, 50),
       "res <- data %>% group_by(ID) %>% summarise(date..."
    )
  } )
  test_that("char_diff", {
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b", "c")),
      list(c(FALSE, FALSE, FALSE), c(FALSE, FALSE, FALSE))
    )
    expect_identical(
      unitizer:::char_diff(c("a", "b"), c("a", "b", "c")),
      list(c(FALSE, FALSE), c(FALSE, FALSE, TRUE))
    )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b")),
      list(c(FALSE, FALSE, TRUE), c(FALSE, FALSE))
    )
    expect_identical(
      unitizer:::char_diff(c("b", "c"), c("a", "b")),
      list(c(FALSE, TRUE), c(TRUE, FALSE))
    )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c", "d"), c("a", "b", "b", "d", "e")),
      list(c(FALSE, FALSE, TRUE, FALSE), c(FALSE, FALSE, TRUE, FALSE, TRUE))
    )
    expect_identical(
      unitizer:::char_diff(c("a", "b", "c"), c("a", "b", "d")),
      list(c(FALSE, FALSE, TRUE), c(FALSE, FALSE, TRUE))
    )
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
  })
  test_that("let_comb_fun",{
    expect_identical(
      unitizer:::make_let_combn_fun(letters)(12),
      c("a.", "b.", "c.", "d.", "e.", "f.", "g.", "h.", "i.", "j.",  "k.", "l.")
    )
  })
  test_that("cc",{
    expect_equal(unitizer:::cc("a", "b"), "ab")
    expect_equal(unitizer:::cc(c("a", "b"), "c"), "abc")
    expect_equal(unitizer:::cc(c("a", "b"), "c", c=" "), "a b c")
  })
})
