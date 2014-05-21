library(testthat)
library(testor)

test_that("Text wrapping", {
  var <- "humpty dumpty sat on a truck and had a big dump"
  expect_true(all(nchar(testor:::text_wrap(var, 10)) <= 10))
  var2 <- rep(var, 4)
  expect_true(all(nchar(wrp <- testor:::text_wrap(var2, c(20, 15))) <= 20) && length(wrp) == 14)
} )
test_that("Headers", {
  # these basically require visual inspection

  print(H1("hello world"))
  print(H2("hello world"))
  print(H3("hello world"))

  expect_error(print(H1(rep_len("hello world", 10)))) # cause an error
  print(H1(paste0(rep_len("hello world", 10), collapse=" ")))
  print(H2(paste0(rep_len("hello world", 10), collapse=" ")))

  "No margin"
  print(H2("No margin"), margin="none")
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
  testor:::deparse_peek(expr1, 20L)
  testor:::deparse_peek(expr1, 3L)
  testor:::deparse_peek(expr1, 5L)
  testor:::deparse_peek(expr2, 40L)
} )
