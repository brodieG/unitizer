library(testthat)
library(testor)

test_that("Text wrapping", {
  var <- "humpty dumpty sat on a truck and had a big dump"
  expect_true(all(nchar(unlist(testor:::text_wrap(var, 10))) <= 10))
  var2 <- rep(var, 4)
  expect_true(
    all(nchar(wrp <- unlist(testor:::text_wrap(var2, c(20, 15)))) <= 20) &&
    length(wrp) == 14
  )
} )
test_that("Headers", {
  # these basically require visual inspection

  print(testor:::H1("hello world"))
  print(testor:::H2("hello world"))
  print(testor:::H3("hello world"))

  expect_error(print(testor:::H1(rep_len("hello world", 10)))) # cause an error
  print(testor:::H1(paste0(rep_len("hello world", 10), collapse=" ")))
  print(testor:::H2(paste0(rep_len("hello world", 10), collapse=" ")))

  "No margin"
  print(testor:::H2("No margin"), margin="none")
  "No margin"
} )
test_that("Sweet'n short descriptions work",{
  expect_match(
    testor:::desc(lm(y ~ x, data.frame(y=1:10, x=runif(10)))), 
    "list lm \\[12,4;28\\] \\{coefficients:num\\(2\\);"
  )
  expect_equal(testor:::desc(new("testorItem", call=quote(1+1))), "S4 testorItem")
  expect_equal(testor:::desc(array(1:27, dim=rep(3, 3))), "integer array [3,3,3]")
  expect_equal(testor:::desc(data.frame(a=letters[1:10], b=1:10)), "list data.frame [10,{a:fct;b:int}]")
} )
test_that("Valid Names convert names to valid", {
  expect_equal(testor:::valid_names("hello"), "hello")
  expect_equal(testor:::valid_names(".hello"), ".hello")
  expect_equal(testor:::valid_names("1hello"), "`1hello`")
  expect_equal(testor:::valid_names("hello kitty"), "`hello kitty`")
  expect_equal(testor:::valid_names("h3llo"), "`h3llo`")
  expect_equal(testor:::valid_names("h_llo"), "h_llo")
  expect_equal(testor:::valid_names("$hot"), "`$hot`") 
  expect_equal(testor:::valid_names("HELLO"), "HELLO") 
} )
test_that("strtrunc", {
  expect_equal(testor:::strtrunc("hollywood is for starlets", 5), "ho...")
  expect_error(testor:::strtrunc(5, "hollywood is for starlets"))
} )
test_that("environment name tools", {
  env1 <- new.env(parent=globalenv())
  env2 <- new.env(parent=env1)
  env3 <- new.env(parent=env2)
  env4 <- new.env(parent=env3)

  expect_true(is.character(ename <- testor:::env_name(env3)) && identical(length(ename), 1L))
  expect_true(
    is.character(envanc <- testor:::env_ancestry(env4)) && 
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
  expect_equal("1 + 1 + 3", testor:::deparse_peek(expr1, 20L))
  expect_error(testor:::deparse_peek(expr1, 3L))
  expect_equal("1 + 1...", testor:::deparse_peek(expr1, 5L))
  expect_equal(
    "for (i in 1:100) {    loop.val <- sample...",
    testor:::deparse_peek(expr2, 40L)
  )
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
    print(testor:::OL(vec), 100)
  )
  expect_equal(
    c("- hello htere how are you blah blah blah blah blah", "- this is helpful you know", "- Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labo", "  re et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi", "   ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit ess", "  e cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in ",  "  culpa qui officia deserunt mollit anim id est laborum.", "- a", "- b", "- c", "- d", "- e", "- f", "- g", "- h", "- i", "- j"),
    print(testor:::UL(vec), 100)
  )
} )
test_that("Messing with traceback", {
  warning("Missing traceback tests")
  # Main problem with this is that there may not be a good way to cause a trace
  # back to register while not also stopping execution of this file, so not
  # sure if this can be tested
} )

