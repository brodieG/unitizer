library(unitizer)
# context("Text")
# test_that("cap_first", {
#     set.seed(1, "Mersenne-Twister")
#     words <- replicate(2, paste0(sample(letters, 5), collapse = ""))
#     WORDS <- toupper(words)
#     expect_identical(c("", "A", "B", "Y", "Z", "Gjnue", "Xzpob", 
#         "GJNUE", "XZPOB"), unitizer:::cap_first(c("", letters[1:2], 
#         letters[25:26], words, WORDS)))
#     expect_identical(c("", "a", "b", "y", "z", "gjnue", "xzpob", 
#         "gJNUE", "xZPOB"), unitizer:::decap_first(c("", letters[1:2], 
#         letters[25:26], words, WORDS)))
# })
# - "cap_first" ----------------------------------------------------------------


set.seed(1, "Mersenne-Twister")
words <- replicate(2, paste0(sample(letters, 5), collapse = ""))
WORDS <- toupper(words)
# expect_identical(c("", "A", "B", "Y", "Z", "Gjnue", "Xzpob", 
#     "GJNUE", "XZPOB"), unitizer:::cap_first(c("", letters[1:2], 
#     letters[25:26], words, WORDS)))
c("", "A", "B", "Y", "Z", "Gjnue", "Xzpob", "GJNUE", "XZPOB")
# expect_identical(c("", "a", "b", "y", "z", "gjnue", "xzpob", 
#     "gJNUE", "xZPOB"), unitizer:::decap_first(c("", letters[1:2], 
#     letters[25:26], words, WORDS)))
c("", "a", "b", "y", "z", "gjnue", "xzpob", "gJNUE", "xZPOB")
# test_that("header", {
#     expect_error(unitizer:::header("hello world", letters), "must be 1 length integer-like")
#     expect_error(unitizer:::header(letters, 1), "must be a one length character")
# })
# - "header" -------------------------------------------------------------------


# expect_error(unitizer:::header("hello world", letters), "must be 1 length integer-like")
unitizer:::header("hello world", letters)
# expect_error(unitizer:::header(letters, 1), "must be a one length character")
unitizer:::header(letters, 1)
# test_that("word_wrap", {
#     lorem1 <- "Today, with Kiernan on the stand offering confirmation, Howard walked the jury through the enormous amount of data pulled from Ulbricht's computer. Defense lawyers haven't had a chance yet to respond to this evidence—that will likely come tomorrow. The mountain they have to climb looks higher than ever, though. Last week, Ulbricht's lawyer outlined a defense in which Ulbricht walked away from the marketplace he created and was \"lured back.\" But what will explain the dozens of folders of data on this laptop, with data from the upper echelons of Silk Road management—mixed with the most intimate details of Ulbricht's personal life?"
#     lorem2 <- "/Volumes/FIXED/folder1/folder2/folder.2345/folderabac/file.text.batch"
#     lorem3 <- "\"untz.state.test\", \"add.smooth\", \"bitmapType\", \"browser\", \"browserNLdisabled\", \"CBoundsCheck\", \"check.bounds\", \"citation.bibtex.max\", \"continue\", \"contrasts\""
#     expect_equal(c(18L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 
#         25L), -1L))))
#     t.rn <- range(nchar(head(unitizer:::word_wrap(lorem1, 25L, 
#         3L), -1L)))
#     expect_true(min(t.rn) > 20 && max(t.rn) <= 25)
#     expect_identical(unitizer:::word_wrap(substr(lorem1, 1, 147), 
#         45L, 3L), c("Today, with Kiernan on the stand offering co-", 
#         "nfirmation, Howard walked the jury through ", "the enormous amount of data pulled from Ulb-", 
#         "richt's computer."))
#     expect_identical(unitizer:::word_wrap(substr(lorem1, 1, 147), 
#         45L, 3L, FALSE), c("Today, with Kiernan on the stand offering con", 
#         "firmation, Howard walked the jury through the", "enormous amount of data pulled from Ulbricht'", 
#         "s computer."))
#     expect_identical(unitizer:::word_wrap(lorem2, 15L, 3L), c("/Volumes/FIXED/", 
#         "folder1/fol-", "der2/folder.23-", "45/folderabac/", 
#         "file.text.batch"))
#     expect_identical(unitizer:::word_wrap(lorem2, 15L, 8L), c("/Volumes/FIXED/", 
#         "folder1/", "folder2/folder.", "2345/fol-", "derabac/file.", 
#         "text.batch"))
#     expect_identical(unitizer:::word_wrap(lorem3, 76L, 8L), c("\"untz.state.test\", \"add.smooth\", \"bitmapType\", \"browser\", \"browserNLdisab-", 
#         "led\", \"CBoundsCheck\", \"check.bounds\", \"citation.bibtex.max\", \"continue\", ", 
#         "\"contrasts\""))
#     expect_identical(unitizer:::word_wrap("hello sunset \nthere moonrise", 
#         width = 12L), c("hello sunset", "there ", "moonrise"))
#     x1 <- c("this is supposed to be a particularly long string\nthat allows us to test the behavior of bullets once we start seeing\nsome wrapping kicking in which was a problem once upon a time")
#     expect_identical(unitizer:::word_wrap(x1, unlist = FALSE, 
#         width = 80L), list(c("this is supposed to be a particularly long string", 
#         "that allows us to test the behavior of bullets once we start seeing", 
#         "some wrapping kicking in which was a problem once upon a time")))
#     com <- "# this is supposed to be a relatively long comment that will get re-flowed"
#     old.opt <- options(crayon.enabled = FALSE)
#     on.exit(options(old.opt))
#     expect_identical(unitizer:::word_comment(com, width = 30L), 
#         c("# this is supposed to be a ", "#relatively long comment that ", 
#             "#will get re-flowed"))
#     expect_identical(unitizer:::word_wrap(c("\nhello\nthere", 
#         "\nhow")), c("", "hello", "there", "", "how"))
#     no.wrap <- "hello I won't be wrapped"
#     expect_warning(txt.wrap <- unitizer:::word_wrap(no.wrap, 
#         width = 3), "too narrow")
#     expect_equal(txt.wrap, no.wrap)
# })
# - "word_wrap" ----------------------------------------------------------------


lorem1 <- "Today, with Kiernan on the stand offering confirmation, Howard walked the jury through the enormous amount of data pulled from Ulbricht's computer. Defense lawyers haven't had a chance yet to respond to this evidence—that will likely come tomorrow. The mountain they have to climb looks higher than ever, though. Last week, Ulbricht's lawyer outlined a defense in which Ulbricht walked away from the marketplace he created and was \"lured back.\" But what will explain the dozens of folders of data on this laptop, with data from the upper echelons of Silk Road management—mixed with the most intimate details of Ulbricht's personal life?"
lorem2 <- "/Volumes/FIXED/folder1/folder2/folder.2345/folderabac/file.text.batch"
lorem3 <- "\"untz.state.test\", \"add.smooth\", \"bitmapType\", \"browser\", \"browserNLdisabled\", \"CBoundsCheck\", \"check.bounds\", \"citation.bibtex.max\", \"continue\", \"contrasts\""
# expect_equal(c(18L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 
#     25L), -1L))))
c(18L, 25L)
t.rn <- range(nchar(head(unitizer:::word_wrap(lorem1, 25L, 3L), 
    -1L)))
# for some reason can't get test to produce same thing in windows when
# running all tests vs. single one at the prompt; the > 20 is a cop-out that
# should catch both the expected case (23) and what actually happens when
# you run the tests on windows
# expect_true(min(t.rn) > 20 && max(t.rn) <= 25)
min(t.rn) > 20 && max(t.rn) <= 25
# expect_identical(unitizer:::word_wrap(substr(lorem1, 1, 147), 
#     45L, 3L), c("Today, with Kiernan on the stand offering co-", 
#     "nfirmation, Howard walked the jury through ", "the enormous amount of data pulled from Ulb-", 
#     "richt's computer."))
unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L)
# expect_identical(unitizer:::word_wrap(substr(lorem1, 1, 147), 
#     45L, 3L, FALSE), c("Today, with Kiernan on the stand offering con", 
#     "firmation, Howard walked the jury through the", "enormous amount of data pulled from Ulbricht'", 
#     "s computer."))
unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L, FALSE)
# expect_identical(unitizer:::word_wrap(lorem2, 15L, 3L), c("/Volumes/FIXED/", 
#     "folder1/fol-", "der2/folder.23-", "45/folderabac/", "file.text.batch"))
unitizer:::word_wrap(lorem2, 15L, 3L)
# expect_identical(unitizer:::word_wrap(lorem2, 15L, 8L), c("/Volumes/FIXED/", 
#     "folder1/", "folder2/folder.", "2345/fol-", "derabac/file.", 
#     "text.batch"))
unitizer:::word_wrap(lorem2, 15L, 8L)
# expect_identical(unitizer:::word_wrap(lorem3, 76L, 8L), c("\"untz.state.test\", \"add.smooth\", \"bitmapType\", \"browser\", \"browserNLdisab-", 
#     "led\", \"CBoundsCheck\", \"check.bounds\", \"citation.bibtex.max\", \"continue\", ", 
#     "\"contrasts\""))
unitizer:::word_wrap(lorem3, 76L, 8L)
# expect_identical(unitizer:::word_wrap("hello sunset \nthere moonrise", 
#     width = 12L), c("hello sunset", "there ", "moonrise"))
unitizer:::word_wrap("hello sunset \nthere moonrise", width = 12L)
x1 <- c("this is supposed to be a particularly long string\nthat allows us to test the behavior of bullets once we start seeing\nsome wrapping kicking in which was a problem once upon a time")
# expect_identical(unitizer:::word_wrap(x1, unlist = FALSE, width = 80L), 
#     list(c("this is supposed to be a particularly long string", 
#         "that allows us to test the behavior of bullets once we start seeing", 
#         "some wrapping kicking in which was a problem once upon a time")))
unitizer:::word_wrap(x1, unlist = FALSE, width = 80L)
com <- "# this is supposed to be a relatively long comment that will get re-flowed"
old.opt <- options(crayon.enabled = FALSE)
on.exit(options(old.opt))
# expect_identical(unitizer:::word_comment(com, width = 30L), c("# this is supposed to be a ", 
#     "#relatively long comment that ", "#will get re-flowed"))
unitizer:::word_comment(com, width = 30L)
# expect_identical(unitizer:::word_wrap(c("\nhello\nthere", "\nhow")), 
#     c("", "hello", "there", "", "how"))
unitizer:::word_wrap(c("\nhello\nthere", "\nhow"))
# too narrow
no.wrap <- "hello I won't be wrapped"
# expect_warning(txt.wrap <- unitizer:::word_wrap(no.wrap, width = 3), 
#     "too narrow")
txt.wrap <- unitizer:::word_wrap(no.wrap, width = 3)
# expect_equal(txt.wrap, no.wrap)
txt.wrap
# test_that("bullets", {
#     x <- c("there was once a time when the fantastic unicorns could fly", 
#         "bugs bunny ate carrots and drank milk while hunting ducks")
#     xx <- unitizer:::UL(x)
#     expect_identical(as.character(xx, width = 30L), c("- there was once a time when ", 
#         "  the fantastic unicorns could", "  fly", "- bugs bunny ate carrots and ", 
#         "  drank milk while hunting ", "  ducks"))
#     expect_identical(capture.output(print(xx, width = 80L)), 
#         c("- there was once a time when the fantastic unicorns could fly", 
#             "- bugs bunny ate carrots and drank milk while hunting ducks"))
#     yy <- unitizer:::OL(x)
#     expect_identical(as.character(yy, width = 30L), c("1. there was once a time when ", 
#         "   the fantastic unicorns ", "   could fly", "2. bugs bunny ate carrots and ", 
#         "   drank milk while hunting ", "   ducks"), )
#     expect_identical(sort(as.character(unitizer:::OL(rep(letters, 
#         2), style = "LETTERS"))), sort(c(" A. a", " B. b", " C. c", 
#         " D. d", " E. e", " F. f", " G. g", " H. h", " I. i", 
#         " J. j", " K. k", " L. l", " M. m", " N. n", " O. o", 
#         " P. p", " Q. q", " R. r", " S. s", " T. t", " U. u", 
#         " V. v", " W. w", " X. x", " Y. y", " Z. z", "AA. a", 
#         "AB. b", "AC. c", "AD. d", "AE. e", "AF. f", "AG. g", 
#         "AH. h", "AI. i", "AJ. j", "AK. k", "AL. l", "AM. m", 
#         "AN. n", "AO. o", "AP. p", "AQ. q", "AR. r", "AS. s", 
#         "AT. t", "AU. u", "AV. v", "AW. w", "AX. x", "AY. y", 
#         "AZ. z")))
#     xl <- as.list(x)
#     y <- unitizer:::UL(c(xl, list(unitizer:::OL(c(xl, list(unitizer:::UL(x))))), 
#         "yowza it is raining toads today!"))
#     expect_identical(as.character(y, width = 30), c("- there was once a time when ", 
#         "  the fantastic unicorns could", "  fly", "- bugs bunny ate carrots and ", 
#         "  drank milk while hunting ", "  ducks", "  1. there was once a time ", 
#         "     when the fantastic ", "     unicorns could fly", 
#         "  2. bugs bunny ate carrots ", "     and drank milk while ", 
#         "     hunting ducks", "    - there was once a time ", 
#         "      when the fantastic ", "      unicorns could fly", 
#         "    - bugs bunny ate carrots ", "      and drank milk while ", 
#         "      hunting ducks", "- yowza it is raining toads ", 
#         "  today!"))
#     expect_error(unitizer:::as.character.bullet(hello, 1:10))
#     expect_error(as.character(unitizer:::OL(c("hello", "there")), 
#         unlist = TRUE), "argument is used internally")
#     expect_equal(as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), 
#         width = 20L), c("1. asdfasdfqwerjhdf-", "   kasdfasdfasd"))
#     expect_equal(as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), 
#         width = 20L, hyphens = FALSE), c("1. asdfasdfqwerjhdfk", 
#         "   asdfasdfasd"))
# })
# - "bullets" ------------------------------------------------------------------


x <- c("there was once a time when the fantastic unicorns could fly", 
    "bugs bunny ate carrots and drank milk while hunting ducks")
xx <- unitizer:::UL(x)
# expect_identical(as.character(xx, width = 30L), c("- there was once a time when ", 
#     "  the fantastic unicorns could", "  fly", "- bugs bunny ate carrots and ", 
#     "  drank milk while hunting ", "  ducks"))
as.character(xx, width = 30L)
# expect_identical(capture.output(print(xx, width = 80L)), c("- there was once a time when the fantastic unicorns could fly", 
#     "- bugs bunny ate carrots and drank milk while hunting ducks"))
capture.output(print(xx, width = 80L))
yy <- unitizer:::OL(x)
# expect_identical(as.character(yy, width = 30L), c("1. there was once a time when ", 
#     "   the fantastic unicorns ", "   could fly", "2. bugs bunny ate carrots and ", 
#     "   drank milk while hunting ", "   ducks"), )
as.character(yy, width = 30L)
# expect_identical(sort(as.character(unitizer:::OL(rep(letters, 
#     2), style = "LETTERS"))), sort(c(" A. a", " B. b", " C. c", 
#     " D. d", " E. e", " F. f", " G. g", " H. h", " I. i", " J. j", 
#     " K. k", " L. l", " M. m", " N. n", " O. o", " P. p", " Q. q", 
#     " R. r", " S. s", " T. t", " U. u", " V. v", " W. w", " X. x", 
#     " Y. y", " Z. z", "AA. a", "AB. b", "AC. c", "AD. d", "AE. e", 
#     "AF. f", "AG. g", "AH. h", "AI. i", "AJ. j", "AK. k", "AL. l", 
#     "AM. m", "AN. n", "AO. o", "AP. p", "AQ. q", "AR. r", "AS. s", 
#     "AT. t", "AU. u", "AV. v", "AW. w", "AX. x", "AY. y", "AZ. z")))
sort(as.character(unitizer:::OL(rep(letters, 2), style = "LETTERS")))
xl <- as.list(x)
y <- unitizer:::UL(c(xl, list(unitizer:::OL(c(xl, list(unitizer:::UL(x))))), 
    "yowza it is raining toads today!"))
# expect_identical(as.character(y, width = 30), c("- there was once a time when ", 
#     "  the fantastic unicorns could", "  fly", "- bugs bunny ate carrots and ", 
#     "  drank milk while hunting ", "  ducks", "  1. there was once a time ", 
#     "     when the fantastic ", "     unicorns could fly", "  2. bugs bunny ate carrots ", 
#     "     and drank milk while ", "     hunting ducks", "    - there was once a time ", 
#     "      when the fantastic ", "      unicorns could fly", 
#     "    - bugs bunny ate carrots ", "      and drank milk while ", 
#     "      hunting ducks", "- yowza it is raining toads ", "  today!"))
as.character(y, width = 30)
# expect_error(unitizer:::as.character.bullet(hello, 1:10))
unitizer:::as.character.bullet(hello, 1:10)
# Extra args to word_wrap
# expect_error(as.character(unitizer:::OL(c("hello", "there")), 
#     unlist = TRUE), "argument is used internally")
as.character(unitizer:::OL(c("hello", "there")), unlist = TRUE)
# expect_equal(as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), 
#     width = 20L), c("1. asdfasdfqwerjhdf-", "   kasdfasdfasd"))
as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), width = 20L)
# expect_equal(as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), 
#     width = 20L, hyphens = FALSE), c("1. asdfasdfqwerjhdfk", 
#     "   asdfasdfasd"))
as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), width = 20L, 
    hyphens = FALSE)
# test_that("substr_const", {
#     expect_equal(unitizer:::substr_cons(c("ab", "abcde", "abce"), 
#         4L), c("ab  ", "abcd", "abc "))
#     expect_equal(unitizer:::substr_cons(c("ab", "abcde", "abce"), 
#         4L, justify = "right"), c("  ab", "abcd", " abc"))
#     expect_equal(unitizer:::substr_cons(c("NEW", "PASS", "FAIL", 
#         "DELETED", "Error"), 4L), c("NEW ", "PASS", "FAIL", "DEL ", 
#         "Err "))
# })
# - "substr_const" -------------------------------------------------------------


# expect_equal(unitizer:::substr_cons(c("ab", "abcde", "abce"), 
#     4L), c("ab  ", "abcd", "abc "))
unitizer:::substr_cons(c("ab", "abcde", "abce"), 4L)
# expect_equal(unitizer:::substr_cons(c("ab", "abcde", "abce"), 
#     4L, justify = "right"), c("  ab", "abcd", " abc"))
unitizer:::substr_cons(c("ab", "abcde", "abce"), 4L, justify = "right")
# expect_equal(unitizer:::substr_cons(c("NEW", "PASS", "FAIL", 
#     "DELETED", "Error"), 4L), c("NEW ", "PASS", "FAIL", "DEL ", 
#     "Err "))
unitizer:::substr_cons(c("NEW", "PASS", "FAIL", "DELETED", "Error"), 
    4L)
# test_that("str_reduce_unique", {
#     str1 <- c("abcdef", "abcdefgh", "abcql")
#     res1 <- c("def", "defgh", "ql")
#     expect_equal(unitizer:::str_reduce_unique(str1), res1)
#     expect_equal(unitizer:::str_reduce_unique(str1, from = "right"), 
#         str1)
#     str2 <- vapply(strsplit(str1, ""), function(x) paste0(rev(x), 
#         collapse = ""), "")
#     res2 <- vapply(strsplit(res1, ""), function(x) paste0(rev(x), 
#         collapse = ""), "")
#     expect_equal(unitizer:::str_reduce_unique(str2, from = "right"), 
#         res2)
#     expect_equal(unitizer:::str_reduce_unique("aaa"), "")
#     expect_equal(unitizer:::str_reduce_unique(rep("aaa", 5L)), 
#         rep("", 5L))
# })
# - "str_reduce_unique" --------------------------------------------------------


str1 <- c("abcdef", "abcdefgh", "abcql")
res1 <- c("def", "defgh", "ql")
# expect_equal(unitizer:::str_reduce_unique(str1), res1)
unitizer:::str_reduce_unique(str1)
# expect_equal(unitizer:::str_reduce_unique(str1, from = "right"), 
#     str1)
unitizer:::str_reduce_unique(str1, from = "right")
str2 <- vapply(strsplit(str1, ""), function(x) paste0(rev(x), 
    collapse = ""), "")
res2 <- vapply(strsplit(res1, ""), function(x) paste0(rev(x), 
    collapse = ""), "")
# expect_equal(unitizer:::str_reduce_unique(str2, from = "right"), 
#     res2)
unitizer:::str_reduce_unique(str2, from = "right")
# expect_equal(unitizer:::str_reduce_unique("aaa"), "")
unitizer:::str_reduce_unique("aaa")
# expect_equal(unitizer:::str_reduce_unique(rep("aaa", 5L)), rep("", 
#     5L))
unitizer:::str_reduce_unique(rep("aaa", 5L))
# test_that("strtrunc", {
#     str1 <- c(paste0(letters, collapse = ""), paste0(LETTERS, 
#         collapse = ""))
#     expect_equal(unitizer:::strtrunc(str1, 10L), c("abcdefg...", 
#         "ABCDEFG..."))
#     expect_equal(unitizer:::strtrunc(str1, 10L, from = "left"), 
#         c("...tuvwxyz", "...TUVWXYZ"))
#     expect_equal(unitizer:::strtrunc(c("abc", "cab"), 3L), c("abc", 
#         "cab"))
#     expect_error(unitizer:::strtrunc(c("abc", "cab"), 2L), "too small")
# })
# - "strtrunc" -----------------------------------------------------------------


str1 <- c(paste0(letters, collapse = ""), paste0(LETTERS, collapse = ""))
# expect_equal(unitizer:::strtrunc(str1, 10L), c("abcdefg...", 
#     "ABCDEFG..."))
unitizer:::strtrunc(str1, 10L)
# expect_equal(unitizer:::strtrunc(str1, 10L, from = "left"), c("...tuvwxyz", 
#     "...TUVWXYZ"))
unitizer:::strtrunc(str1, 10L, from = "left")
# expect_equal(unitizer:::strtrunc(c("abc", "cab"), 3L), c("abc", 
#     "cab"))
unitizer:::strtrunc(c("abc", "cab"), 3L)
# expect_error(unitizer:::strtrunc(c("abc", "cab"), 2L), "too small")
unitizer:::strtrunc(c("abc", "cab"), 2L)
# test_that("oneline", {
#     dep <- c("res <- data %>% group_by(ID) %>% summarise(date2 = nth(date, ", 
#         "    2), time2 = nth(time, 2), first_date = first(date), last_date = last(date), ", 
#         "    first_time = first(time), last_time = last(time))")
#     expect_equal(unitizer:::one_line(dep), "res <- data %>% group_by(ID) %>% summarise(date2 = nth(date, 2), time2 = nth(time, 2), first_date = first(date), last_date = last(date), first_time = first(time), last_time = last(time))")
#     expect_equal(unitizer:::one_line(dep, 50), "res <- data %>% group_by(ID) %>% summarise(date...")
# })
# - "oneline" ------------------------------------------------------------------


dep <- c("res <- data %>% group_by(ID) %>% summarise(date2 = nth(date, ", 
    "    2), time2 = nth(time, 2), first_date = first(date), last_date = last(date), ", 
    "    first_time = first(time), last_time = last(time))")
# expect_equal(unitizer:::one_line(dep), "res <- data %>% group_by(ID) %>% summarise(date2 = nth(date, 2), time2 = nth(time, 2), first_date = first(date), last_date = last(date), first_time = first(time), last_time = last(time))")
unitizer:::one_line(dep)
# expect_equal(unitizer:::one_line(dep, 50), "res <- data %>% group_by(ID) %>% summarise(date...")
unitizer:::one_line(dep, 50)
# test_that("let_comb_fun", {
#     expect_identical((unitizer:::make_let_combn_fun(letters))(12), 
#         c("a.", "b.", "c.", "d.", "e.", "f.", "g.", "h.", "i.", 
#             "j.", "k.", "l."))
# })
# - "let_comb_fun" -------------------------------------------------------------


# expect_identical((unitizer:::make_let_combn_fun(letters))(12), 
#     c("a.", "b.", "c.", "d.", "e.", "f.", "g.", "h.", "i.", "j.", 
#         "k.", "l."))
(unitizer:::make_let_combn_fun(letters))(12)
# test_that("cc", {
#     expect_equal(unitizer:::cc("a", "b"), "ab")
#     expect_equal(unitizer:::cc(c("a", "b"), "c"), "abc")
#     expect_equal(unitizer:::cc(c("a", "b"), "c", c = " "), "a b c")
# })
# - "cc" -----------------------------------------------------------------------


# expect_equal(unitizer:::cc("a", "b"), "ab")
unitizer:::cc("a", "b")
# expect_equal(unitizer:::cc(c("a", "b"), "c"), "abc")
unitizer:::cc(c("a", "b"), "c")
# expect_equal(unitizer:::cc(c("a", "b"), "c", c = " "), "a b c")
unitizer:::cc(c("a", "b"), "c", c = " ")
# test_that("screen_out", {
#     string <- "once upon a time in a fairy land very far away lived a green dragon"
#     expect_equal(capture.output(unitizer:::screen_out(string, 
#         max.len = c(3L, 2L), width = 13L)), c("once upon a ", 
#         "time in a ", "... truncated 4 lines"))
# })
# - "screen_out" ---------------------------------------------------------------


string <- "once upon a time in a fairy land very far away lived a green dragon"
# expect_equal(capture.output(unitizer:::screen_out(string, max.len = c(3L, 
#     2L), width = 13L)), c("once upon a ", "time in a ", "... truncated 4 lines"))
capture.output(unitizer:::screen_out(string, max.len = c(3L, 
    2L), width = 13L))
# test_that("text_wrap", {
#     expect_error(unitizer:::text_wrap(list(1, 2, 3), 5), "must be")
#     expect_error(unitizer:::text_wrap(letters, 1:3), "multiple")
# })
# - "text_wrap" ----------------------------------------------------------------


# expect_error(unitizer:::text_wrap(list(1, 2, 3), 5), "must be")
unitizer:::text_wrap(list(1, 2, 3), 5)
# expect_error(unitizer:::text_wrap(letters, 1:3), "multiple")
unitizer:::text_wrap(letters, 1:3)
# test_that("capture_output", {
#     capt <- unitizer:::capture_output({
#         cat("hello")
#         cat("goodbye", file = stderr())
#     })
#     expect_equal(capt, structure(list(output = "hello", message = "goodbye"), 
#         class = "captured_output"))
#     expect_equal(sum(grepl("Output|Message", capture.output(print(capt)))), 
#         2L)
# })
# - "capture_output" -----------------------------------------------------------


capt <- unitizer:::capture_output({
    cat("hello")
    cat("goodbye", file = stderr())
})
# expect_equal(capt, structure(list(output = "hello", message = "goodbye"), 
#     class = "captured_output"))
capt
# expect_equal(sum(grepl("Output|Message", capture.output(print(capt)))), 
#     2L)
sum(grepl("Output|Message", capture.output(print(capt))))
# test_that("meta_word_cat", {
#     expect_equal(capture.output(unitizer:::meta_word_cat("hello")), 
#         c("| hello", ""))
#     expect_equal(capture.output(unitizer:::meta_word_cat("hello", 
#         trail.nl = FALSE)), "| hello")
#     expect_equal(capture.output(unitizer:::meta_word_cat("hello\n", 
#         sep = "")), c("| hello", ""))
#     expect_equal(capture.output(unitizer:::meta_word_cat("hello", 
#         "there")), c("| hello", "| there", ""))
#     expect_equal(capture.output(unitizer:::meta_word_cat("hello", 
#         "there", sep = " ")), c("| hello there", ""))
# })
# - "meta_word_cat" ------------------------------------------------------------


# expect_equal(capture.output(unitizer:::meta_word_cat("hello")), 
#     c("| hello", ""))
capture.output(unitizer:::meta_word_cat("hello"))
# expect_equal(capture.output(unitizer:::meta_word_cat("hello", 
#     trail.nl = FALSE)), "| hello")
capture.output(unitizer:::meta_word_cat("hello", trail.nl = FALSE))
# Newline issues
# expect_equal(capture.output(unitizer:::meta_word_cat("hello\n", 
#     sep = "")), c("| hello", ""))
capture.output(unitizer:::meta_word_cat("hello\n", sep = ""))
# expect_equal(capture.output(unitizer:::meta_word_cat("hello", 
#     "there")), c("| hello", "| there", ""))
capture.output(unitizer:::meta_word_cat("hello", "there"))
# expect_equal(capture.output(unitizer:::meta_word_cat("hello", 
#     "there", sep = " ")), c("| hello there", ""))
capture.output(unitizer:::meta_word_cat("hello", "there", sep = " "))
# test_that("meta_word_msg", {
#     expect_message(unitizer:::meta_word_msg("hello"), c("| hello\n"), 
#         fixed = TRUE)
#     txt <- "hello there how are you this wraps"
#     expect_message(unitizer:::meta_word_msg(txt, width = 20), 
#         "| hello there how \n| are you this wraps\n\n", fixed = TRUE)
#     expect_equal(capture.output(unitizer:::word_msg("hello"), 
#         type = "message"), "hello")
# })
# - "meta_word_msg" ------------------------------------------------------------


# expect_message(unitizer:::meta_word_msg("hello"), c("| hello\n"), 
#     fixed = TRUE)
unitizer:::meta_word_msg("hello")
txt <- "hello there how are you this wraps"
# expect_message(unitizer:::meta_word_msg(txt, width = 20), "| hello there how \n| are you this wraps\n\n", 
#     fixed = TRUE)
unitizer:::meta_word_msg(txt, width = 20)
# legacy fun
# expect_equal(capture.output(unitizer:::word_msg("hello"), type = "message"), 
#     "hello")
capture.output(unitizer:::word_msg("hello"), type = "message")
# test_that("desc", {
#     obj1 <- list(a = iris, b = lm(dist ~ speed, cars), 1:10, 
#         matrix(letters, 2))
#     expect_equal(desc(obj1, 80), "list(a=data.frame[150,5], b=lm[12], int[10], chr mat[2,13])")
#     expect_equal(desc(obj1, 40), "list[4]")
#     expect_equal(desc(iris, 80), "data.frame[150,5]")
#     expect_equal(desc(iris, 200), "data.frame(Sepal.Length=num[150], Sepal.Width=num[150], Petal.Length=num[150], Petal.Width=num[150], Species=fct[150])")
#     expect_equal(desc(list(NULL, 1L)), "list(NULL, int[1])")
#     expect_equal(desc(NULL), "NULL")
#     expect_equal(unitizer:::desc(NULL), "NULL")
#     expect_equal(unitizer:::desc(lm(y ~ x, data.frame(y = 1:10, 
#         x = runif(10)))), "lm[12]")
#     expect_equal(unitizer:::desc(new("unitizerItem", call = quote(1 + 
#         1), env = new.env())), "unitizerItem")
#     expect_equal(unitizer:::desc(array(1:27, dim = rep(3, 3))), 
#         "array[3,3,3]")
#     expect_equal(unitizer:::desc(data.frame(a = letters[1:10], 
#         b = 1:10, stringsAsFactors = TRUE)), "data.frame(a=fct[10], b=int[10])")
# })
# - "desc" ---------------------------------------------------------------------


obj1 <- list(a = iris, b = lm(dist ~ speed, cars), 1:10, matrix(letters, 
    2))
# expect_equal(desc(obj1, 80), "list(a=data.frame[150,5], b=lm[12], int[10], chr mat[2,13])")
desc(obj1, 80)
# expect_equal(desc(obj1, 40), "list[4]")
desc(obj1, 40)
# expect_equal(desc(iris, 80), "data.frame[150,5]")
desc(iris, 80)
# expect_equal(desc(iris, 200), "data.frame(Sepal.Length=num[150], Sepal.Width=num[150], Petal.Length=num[150], Petal.Width=num[150], Species=fct[150])")
desc(iris, 200)
# expect_equal(desc(list(NULL, 1L)), "list(NULL, int[1])")
desc(list(NULL, 1L))
# expect_equal(desc(NULL), "NULL")
desc(NULL)
# expect_equal(unitizer:::desc(NULL), "NULL")
unitizer:::desc(NULL)
# expect_equal(unitizer:::desc(lm(y ~ x, data.frame(y = 1:10, x = runif(10)))), 
#     "lm[12]")
unitizer:::desc(lm(y ~ x, data.frame(y = 1:10, x = runif(10))))
# expect_equal(unitizer:::desc(new("unitizerItem", call = quote(1 + 
#     1), env = new.env())), "unitizerItem")
unitizer:::desc(new("unitizerItem", call = quote(1 + 1), env = new.env()))
# expect_equal(unitizer:::desc(array(1:27, dim = rep(3, 3))), "array[3,3,3]")
unitizer:::desc(array(1:27, dim = rep(3, 3)))
# expect_equal(unitizer:::desc(data.frame(a = letters[1:10], b = 1:10, 
#     stringsAsFactors = TRUE)), "data.frame(a=fct[10], b=int[10])")
unitizer:::desc(data.frame(a = letters[1:10], b = 1:10, stringsAsFactors = TRUE))
# test_that("char_to_eng", {
#     expect_equal(unitizer:::char_to_eng(character(), "", ""), 
#         "")
#     expect_equal(unitizer:::char_to_eng(letters[1:4], "", ""), 
#         "a, b, c, and d")
#     expect_equal(unitizer:::char_to_eng(letters[1:2], "", ""), 
#         "a, and b")
#     expect_equal(unitizer:::char_to_eng(letters[1], "", ""), 
#         "a")
#     expect_equal(unitizer:::char_to_eng(letters[1]), "a was")
#     expect_equal(unitizer:::char_to_eng(letters[1:2]), "a, and b were")
# })
# - "char_to_eng" --------------------------------------------------------------


# expect_equal(unitizer:::char_to_eng(character(), "", ""), "")
unitizer:::char_to_eng(character(), "", "")
# expect_equal(unitizer:::char_to_eng(letters[1:4], "", ""), "a, b, c, and d")
unitizer:::char_to_eng(letters[1:4], "", "")
# expect_equal(unitizer:::char_to_eng(letters[1:2], "", ""), "a, and b")
unitizer:::char_to_eng(letters[1:2], "", "")
# expect_equal(unitizer:::char_to_eng(letters[1], "", ""), "a")
unitizer:::char_to_eng(letters[1], "", "")
# expect_equal(unitizer:::char_to_eng(letters[1]), "a was")
unitizer:::char_to_eng(letters[1])
# expect_equal(unitizer:::char_to_eng(letters[1:2]), "a, and b were")
unitizer:::char_to_eng(letters[1:2])
