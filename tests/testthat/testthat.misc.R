  library(testthat)
library(unitizer)

test_that("Text wrapping", {
  var <- "humpty dumpty sat on a truck and had a big dump"
  expect_true(all(nchar(unlist(unitizer:::text_wrap(var, 10))) <= 10))
  var2 <- rep(var, 4)
  expect_true(
    all(nchar(wrp <- unlist(unitizer:::text_wrap(var2, c(20, 15)))) <= 20) &&
    length(wrp) == 14
  )
} )
test_that("Headers", {
  # these basically require visual inspection

  print(unitizer:::H1("hello world"))
  print(unitizer:::H2("hello world"))
  print(unitizer:::H3("hello world"))

  expect_error(print(unitizer:::H1(rep_len("hello world", 10)))) # cause an error
  print(unitizer:::H1(paste0(rep_len("hello world", 10), collapse=" ")))
  print(unitizer:::H2(paste0(rep_len("hello world", 10), collapse=" ")))

  "No margin"
  print(unitizer:::H2("No margin"), margin="none")
  "No margin"
} )
test_that("Sweet'n short descriptions work",{
  expect_match(
    unitizer:::desc(lm(y ~ x, data.frame(y=1:10, x=runif(10)))),
    "list lm \\[12,4;28\\] \\{coefficients:num\\(2\\);"
  )
  expect_equal(unitizer:::desc(new("unitizerItem", call=quote(1+1), env=new.env())), "S4 unitizerItem")
  expect_equal(unitizer:::desc(array(1:27, dim=rep(3, 3))), "integer array [3,3,3]")
  expect_equal(unitizer:::desc(data.frame(a=letters[1:10], b=1:10)), "list data.frame [10,{a:fct;b:int}]")
} )
test_that("Valid Names convert names to valid", {
  expect_equal(unitizer:::valid_names("hello"), "hello")
  expect_equal(unitizer:::valid_names(".hello"), ".hello")
  expect_equal(unitizer:::valid_names("1hello"), "`1hello`")
  expect_equal(unitizer:::valid_names("hello kitty"), "`hello kitty`")
  expect_equal(unitizer:::valid_names("h3llo"), "`h3llo`")
  expect_equal(unitizer:::valid_names("h_llo"), "h_llo")
  expect_equal(unitizer:::valid_names("$hot"), "`$hot`")
  expect_equal(unitizer:::valid_names("HELLO"), "HELLO")
} )
test_that("strtrunc", {
  expect_equal(unitizer:::strtrunc("hollywood is for starlets", 5), "ho...")
  expect_error(unitizer:::strtrunc(5, "hollywood is for starlets"))
} )
test_that("environment name tools", {
  env1 <- new.env(parent=globalenv())
  env2 <- new.env(parent=env1)
  env3 <- new.env(parent=env2)
  env4 <- new.env(parent=env3)

  expect_true(is.character(ename <- unitizer:::env_name(env3)) && identical(length(ename), 1L))
  expect_true(
    is.character(envanc <- unitizer:::env_ancestry(env4)) &&
    identical(length(envanc), 5L) &&
    identical(envanc[[5L]], "R_GlobalEnv")
  )
} )
test_that("deparse peek", {
  expr1 <- quote(1 + 1 + 3)
  expr2 <- quote(
    for(i in 1:100) {
      loop.val <- sample(1:1000, 200, replace=TRUE)
      loop.val <- loop.val * 200 / 3000 * mean(runif(20000))
  } )
  expect_equal("1 + 1 + 3", unitizer:::deparse_peek(expr1, 20L))
  expect_error(unitizer:::deparse_peek(expr1, 3L))
  expect_equal("1 ...", unitizer:::deparse_peek(expr1, 5L))
  expect_equal(
    "for (i in 1:100) {    loop.val <- sam...",
    unitizer:::deparse_peek(expr2, 40L)
  )
} )
test_that("deparse fun", {
  expect_identical(unitizer:::deparse_fun(quote(fun)), "fun")
  expect_identical(unitizer:::deparse_fun(quote(function(x) NULL)), NA_character_)
  expect_identical(unitizer:::deparse_fun("hello"), character(0L))
} )
test_that("(Un)ordered Lists", {
  vec <- c(
    "hello htere how are you blah blah blah blah blah",
    "this is helpful you know",
    "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
    letters[1:10]
  )
  expect_equal(
    c(" 1. hello htere how are you blah blah blah blah blah", " 2. this is helpful you know", " 3. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut la", "    bore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ", "    nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate vel", "    it esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, ",  "    sunt in culpa qui officia deserunt mollit anim id est laborum.", " 4. a", " 5. b", " 6. c", " 7. d", " 8. e", " 9. f", "10. g", "11. h", "12. i", "13. j"),
    print(unitizer:::OL(vec), 100)
  )
  expect_equal(
    c("- hello htere how are you blah blah blah blah blah", "- this is helpful you know", "- Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labo", "  re et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi", "   ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit ess", "  e cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in ",  "  culpa qui officia deserunt mollit anim id est laborum.", "- a", "- b", "- c", "- d", "- e", "- f", "- g", "- h", "- i", "- j"),
    print(unitizer:::UL(vec), 100)
  )
} )
# test_that("Messing with traceback", {
#   warning("Missing traceback tests")
#   # Main problem with this is that there may not be a good way to cause a trace
#   # back to register while not also stopping execution of this file, so not
#   # sure if this can be tested
# } )

test_that("Compare Conditions", {
  lst1 <- new("conditionList", .items=list(
      simpleWarning("warning", quote(yo + yo)),
      simpleWarning("warning2", quote(yo2 + yo)),
      simpleWarning("warning3", quote(yo3 + yo)),
      simpleError("error1", quote(make_an_error()))
  ) )
  lst2 <- new("conditionList", .items=list(
      simpleWarning("warning", quote(yo + yo)),
      simpleWarning("warning2", quote(yo2 + yo)),
      simpleError("error1", quote(make_an_error()))
  ) )
  expect_true(all.equal(lst1, lst1))
  expect_equal(
    "`target` (a.k.a `.ref`) and `current` (a.k.a `.new`) do not have the same number of conditions (4 vs 3)",
    all.equal(lst1, lst2)
  )
  expect_equal(
    c("There is 1 condition mismatch; showing first mismatch at condition #3", "Condition type mismatch, `target` (a.k.a. `.ref`) is 'Error', but `current` (a.k.a. `.new`) is 'Warning'"),
    all.equal(lst2, lst1[1L:3L])
  )
  expect_equal(
    c("There are 2 condition mismatches; showing first mismatch at condition #1", "Warning condition messages do not match"),
    all.equal(lst2, lst1[2L:4L])
  )
  attr(lst1[[3L]], "printed") <- TRUE

  expect_equal(
    c("There is 1 condition mismatch; showing first mismatch at condition #3", "Condition type mismatch, `target` (a.k.a. `.ref`) is 'Error', but `current` (a.k.a. `.new`) is 'Warning'", "Condition mismatch may involve print/show methods; carefully review conditions with `.NEW$conditions` and `.REF$conditions` as just typing `.ref` or `.new` at the prompt will invoke print/show methods, which themselves may be the cause of the mismatch."),
    all.equal(lst2, lst1[1L:3L])
  )
  attr(lst1[[3L]], "printed") <- NULL
  lst1[[2L]] <- simpleWarning("warning2", quote(yo2 + yoyo))
  expect_equal(
    c("There is 1 condition mismatch; showing first mismatch at condition #2", "Warning condition calls do not match"),
    all.equal(lst2, lst1[c(1L:2L, 4L)])
  )
} )
test_that("Compare Functions With Traces", {
  fun.a <- base::library
  expect_true(identical(fun.a, base::library))
  trace(library, where=.BaseNamespaceEnv)
  expect_false(identical(fun.a, base::library))
  expect_true(unitizer:::identical_fun(fun.a, base::library))
  expect_false(unitizer:::identical_fun(base::library, fun.a))
  untrace(library, where=.BaseNamespaceEnv)
  expect_error(unitizer:::identical_fun(1, base::library))
  expect_error(unitizer:::identical_fun(base::library, 1))
} )
test_that("word_cat", {
  str <- "Humpty dumpty sat on a wall and took a big fall.  All the kings horses and men couldn't put humpty dumpty together again"
  expect_equal(
    c("Humpty dumpty sat ", "on a wall and took ", "a big fall.  All ", "the kings horses ", "and men couldn't ", "put humpty dumpty ", "together again"),
    capture.output(unitizer:::word_cat(str, fill=20))
  )
  expect_equal(str, capture.output(unitizer:::word_cat(str, fill=20, sep=" ")))  # sep forces this to be treated as cat
  expect_error(unitizer:::word_cat(stop("boom"), fill=20, sep=" "), ": boom")
  expect_error(unitizer:::word_cat(stop("boom"), fill=20), "Problem evaluating `\\.\\.\\.`")
  str2 <- rep("goodbye goodbye")
  str1 <- rep("hello hello hello", 2)
  expect_equal(
    c("hello hello ", "hello hello ", "hello hello ", "goodbye ", "goodbye"),
    capture.output(unitizer:::word_cat(str1, str2, fill=15))
  )
  # Make sure default works

  width <- getOption("width")
  options(width=20)
  expect_equal(
    c("Humpty dumpty sat ", "on a wall and took ", "a big fall.  All ", "the kings horses ", "and men couldn't ", "put humpty dumpty ", "together again"),
    capture.output(unitizer:::word_cat(str))
  )
  options(width=width)
})
