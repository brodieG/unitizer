source(file.path("_helper", "init.R"))

# - "cap_first" ----------------------------------------------------------------

set.seed(1, "Mersenne-Twister")
words <- replicate(2, paste0(sample(letters, 5), collapse = ""))
WORDS <- toupper(words)

unitizer:::cap_first(c("", letters[1:2], letters[25:26], words, WORDS))

unitizer:::decap_first(c("", letters[1:2], letters[25:26], words, WORDS))

# - "header" -------------------------------------------------------------------

try(unitizer:::header("hello world", letters))
try(unitizer:::header(letters, 1))

# - "word_wrap" ----------------------------------------------------------------

lorem1 <- "Today, with Kiernan on the stand offering confirmation, Howard walked the jury through the enormous amount of data pulled from Ulbricht's computer. Defense lawyers haven't had a chance yet to respond to this evidence—that will likely come tomorrow. The mountain they have to climb looks higher than ever, though. Last week, Ulbricht's lawyer outlined a defense in which Ulbricht walked away from the marketplace he created and was \"lured back.\" But what will explain the dozens of folders of data on this laptop, with data from the upper echelons of Silk Road management—mixed with the most intimate details of Ulbricht's personal life?"
lorem2 <- "/Volumes/FIXED/folder1/folder2/folder.2345/folderabac/file.text.batch"
lorem3 <- "\"untz.state.test\", \"add.smooth\", \"bitmapType\", \"browser\", \"browserNLdisabled\", \"CBoundsCheck\", \"check.bounds\", \"citation.bibtex.max\", \"continue\", \"contrasts\""
range(nchar(head(unitizer:::word_wrap(lorem1, 25L), -1L)))
t.rn <- range(nchar(head(unitizer:::word_wrap(lorem1, 25L, 3L), 
    -1L)))
# for some reason can't get test to produce same thing in windows when
# running all tests vs. single one at the prompt; the > 20 is a cop-out that
# should catch both the expected case (23) and what actually happens when
# you run the tests on windows
# expect_true(min(t.rn) > 20 && max(t.rn) <= 25)
min(t.rn) > 20 && max(t.rn) <= 25

unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L)
unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L, FALSE)
unitizer:::word_wrap(lorem2, 15L, 3L)
unitizer:::word_wrap(lorem2, 15L, 8L)

unitizer:::word_wrap(lorem3, 76L, 8L)
unitizer:::word_wrap("hello sunset \nthere moonrise", width = 12L)

x1 <- c("this is supposed to be a particularly long string\nthat allows us to test the behavior of bullets once we start seeing\nsome wrapping kicking in which was a problem once upon a time")

unitizer:::word_wrap(x1, unlist = FALSE, width = 80L)
com <- "# this is supposed to be a relatively long comment that will get re-flowed"
old.opt <- options(crayon.enabled = FALSE)
unitizer:::word_comment(com, width = 30L)
unitizer:::word_wrap(c("\nhello\nthere", "\nhow"))
# too narrow
no.wrap <- "hello I won't be wrapped"
unitizer:::word_wrap(no.wrap, width = 3)  # warning
options(old.opt)

# - "bullets" ------------------------------------------------------------------

x <- c("there was once a time when the fantastic unicorns could fly", 
    "bugs bunny ate carrots and drank milk while hunting ducks")
xx <- unitizer:::UL(x)

as.character(xx, width = 30L)
print(xx, width = 80L)
yy <- unitizer:::OL(x)
as.character(yy, width = 30L)
# hopefully always C locale collation in tests?
sort(as.character(unitizer:::OL(rep(letters, 2), style = "LETTERS")))
xl <- as.list(x)
y <- unitizer:::UL(c(xl, list(unitizer:::OL(c(xl, list(unitizer:::UL(x))))), 
    "yowza it is raining toads today!"))
as.character(y, width = 30)
try(unitizer:::as.character.bullet(hello, 1:10))
# Extra args to word_wrap
try(as.character(unitizer:::OL(c("hello", "there")), unlist = TRUE))
as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), width = 20L)
as.character(unitizer:::OL("asdfasdfqwerjhdfkasdfasdfasd"), width = 20L, 
    hyphens = FALSE)

# - "substr_const" -------------------------------------------------------------

unitizer:::substr_cons(c("ab", "abcde", "abce"), 4L)
unitizer:::substr_cons(c("ab", "abcde", "abce"), 4L, justify = "right")
unitizer:::substr_cons(c("NEW", "PASS", "FAIL", "DELETED", "Error"), 4L)

# - "str_reduce_unique" --------------------------------------------------------

str1 <- c("abcdef", "abcdefgh", "abcql")
res1 <- c("def", "defgh", "ql")
unitizer:::str_reduce_unique(str1)
unitizer:::str_reduce_unique(str1, from = "right")
str2 <- vapply(strsplit(str1, ""), function(x) paste0(rev(x), 
    collapse = ""), "")
res2 <- vapply(strsplit(res1, ""), function(x) paste0(rev(x), 
    collapse = ""), "")
all.equal(unitizer:::str_reduce_unique(str2, from = "right"), res2)
unitizer:::str_reduce_unique("aaa")
identical(unitizer:::str_reduce_unique(rep("aaa", 5L)), rep("", 5L))

# - "strtrunc" -----------------------------------------------------------------

str1 <- c(paste0(letters, collapse = ""), paste0(LETTERS, collapse = ""))
unitizer:::strtrunc(str1, 10L)
unitizer:::strtrunc(str1, 10L, from = "left")
unitizer:::strtrunc(c("abc", "cab"), 3L)
try(unitizer:::strtrunc(c("abc", "cab"), 2L))

# - "oneline" ------------------------------------------------------------------

dep <- c("res <- data %>% group_by(ID) %>% summarise(date2 = nth(date, ", 
    "    2), time2 = nth(time, 2), first_date = first(date), last_date = last(date), ", 
    "    first_time = first(time), last_time = last(time))")
unitizer:::one_line(dep)
unitizer:::one_line(dep, 50)

# - "let_comb_fun" -------------------------------------------------------------

(unitizer:::make_let_combn_fun(letters))(12)

# - "cc" -----------------------------------------------------------------------

unitizer:::cc("a", "b")
unitizer:::cc(c("a", "b"), "c")
unitizer:::cc(c("a", "b"), "c", c = " ")

# - "screen_out" ---------------------------------------------------------------

string <- "once upon a time in a fairy land very far away lived a green dragon"
unitizer:::screen_out(string, max.len = c(3L, 2L), width = 13L)

# - "text_wrap" ----------------------------------------------------------------

try(unitizer:::text_wrap(list(1, 2, 3), 5))
try(unitizer:::text_wrap(letters, 1:3))

# - "capture_output" -----------------------------------------------------------

capt <- unitizer:::capture_output({
    cat("hello")
    cat("goodbye", file = stderr())
})
capt
sum(grepl("Output|Message", capture.output(print(capt))))

# - "meta_word_cat" ------------------------------------------------------------

unitizer:::meta_word_cat("hello")
capture.output(unitizer:::meta_word_cat("hello", trail.nl = FALSE))
# Newline issues
unitizer:::meta_word_cat("hello\n", sep = "")
unitizer:::meta_word_cat("hello", "there")
unitizer:::meta_word_cat("hello", "there", sep = " ")

# - "meta_word_msg" ------------------------------------------------------------

unitizer:::meta_word_msg("hello")
txt <- "hello there how are you this wraps"
unitizer:::meta_word_msg(txt, width = 20)
# legacy fun
unitizer:::word_msg("hello")

# - "desc" ---------------------------------------------------------------------

obj1 <- list(a = iris, b = lm(dist ~ speed, cars), 1:10, matrix(letters, 
    2))
desc(obj1, 80)
desc(obj1, 40)
desc(iris, 80)
desc(iris, 200)
desc(list(NULL, 1L))
desc(NULL)
unitizer:::desc(NULL)
unitizer:::desc(lm(y ~ x, data.frame(y = 1:10, x = runif(10))))
unitizer:::desc(new("unitizerItem", call = quote(1 + 1), env = new.env()))
unitizer:::desc(array(1:27, dim = rep(3, 3)))
unitizer:::desc(data.frame(a = letters[1:10], b = 1:10, stringsAsFactors = TRUE))

# - "char_to_eng" --------------------------------------------------------------

unitizer:::char_to_eng(character(), "", "")
unitizer:::char_to_eng(letters[1:4], "", "")
unitizer:::char_to_eng(letters[1:2], "", "")
unitizer:::char_to_eng(letters[1], "", "")
unitizer:::char_to_eng(letters[1])
unitizer:::char_to_eng(letters[1:2])

