library(testthat)
library(unitizer)
context("Misc")

if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "misc", sprintf("%s.rds", x))

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

  old.opt <- options(width=80L)
  on.exit(old.opt)
  expect_equal_to_reference(
    capture.output(print(unitizer:::H1("hello world"))),
    rdsf(100)
  )
  expect_equal_to_reference(
    capture.output(print(unitizer:::H2("hello world"))),
    rdsf(200)
  )
  expect_equal_to_reference(
    capture.output(print(unitizer:::H3("hello world"))),
    rdsf(300)
  )
  expect_error(print(unitizer:::H1(rep_len("hello world", 10)))) # cause an error

  h.w.long <- paste0(rep_len("hello world", 10), collapse=" ")
  expect_equal_to_reference(
    capture.output(print(unitizer:::H1(h.w.long))), rdsf(400)
  )
  expect_equal_to_reference(
    capture.output(print(unitizer:::H2(h.w.long))), rdsf(500)
  )
  expect_equal_to_reference(
    capture.output(print(unitizer:::H2("No margin"), margin="none")), rdsf(600)
  )
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
test_that("deparse_prompt", {
  expect_identical(
    unitizer:::deparse_prompt(quote(if(TRUE) {25} else {42})),
    c("> if (TRUE) {", "+     25", "+ } else {", "+     42", "+ }" )
  )
})
test_that("deparse_mixed", {
  b <- setNames(1:3, letters[1:3])
  x <- quote(1 + b)
  x[[3]] <- b
  expect_equal(unitizer:::deparse_mixed(x), "quote(1 + 1:3)")
  y <- quote(1 + 3 + b)
  y[[3]] <- b
  expect_equal(unitizer:::deparse_mixed(y), "quote(1 + 3 + 1:3)")
})

test_that("(Un)ordered Lists", {
  vec <- c(
    "hello htere how are you blah blah blah blah blah",
    "this is helpful you know",
    "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
    letters[1:10]
  )
  expect_equal(
    as.character(unitizer:::OL(vec), width=100L),
    c(" 1. hello htere how are you blah blah blah blah blah", " 2. this is helpful you know", " 3. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut ", "    labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco ", "    laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in ", "    voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat ", "    non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",  " 4. a", " 5. b", " 6. c", " 7. d", " 8. e", " 9. f", "10. g", "11. h", "12. i", "13. j")
  )
  expect_equal(
    as.character(unitizer:::UL(vec), width=20L),
    c("- hello htere how ", "  are you blah blah ", "  blah blah blah", "- this is helpful ", "  you know", "- Lorem ipsum dolor ", "  sit amet, consec-", "  tetur adipisicing ", "  elit, sed do ", "  eiusmod tempor ", "  incididunt ut ", "  labore et dolore ", "  magna aliqua. Ut ", "  enim ad minim ", "  veniam, quis ", "  nostrud exer-", "  citation ullamco ", "  laboris nisi ut ", "  aliquip ex ea ", "  commodo consequat.", "  Duis aute irure ", "  dolor in reprehen-", "  derit in voluptate", "  velit esse cillum ",  "  dolore eu fugiat ", "  nulla pariatur. ", "  Excepteur sint ", "  occaecat cupidatat", "  non proident, sunt", "  in culpa qui ", "  officia deserunt ", "  mollit anim id est", "  laborum.", "- a", "- b", "- c", "- d", "- e", "- f", "- g", "- h", "- i", "- j")
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
    "Condition count mismatch; expected 4 (got 3)",
    all.equal(lst1, lst2)
  )
  expect_equal(
    "There is one condition mismatch at index [[3]]",
    all.equal(lst2, lst1[1L:3L])
  )
  expect_equal(
    "There are 2 condition mismatches, first one at index [[1]]",
    all.equal(lst2, lst1[2L:4L])
  )
  attr(lst1[[3L]], "unitizer.printed") <- TRUE
  expect_equal(
    "There is one condition mismatch at index [[3]]",
    all.equal(lst2, lst1[1L:3L])
  )
  expect_equal(
    c("Condition type mismatch, `target` is 'Error', but `current` is 'Warning'", "Condition mismatch may involve print/show methods; carefully review conditions with `.NEW$conditions` and `.REF$conditions` as just typing `.ref` or `.new` at the prompt will invoke print/show methods, which themselves may be the cause of the mismatch"),
    all.equal(lst2[[3]], lst1[[3]])
  )
  attr(lst1[[3L]], "unitizer.printed") <- NULL
  lst1[[2L]] <- simpleWarning("warning2", quote(yo2 + yoyo))
  expect_equal(
    "There is one condition mismatch at index [[2]]",
    all.equal(lst2, lst1[c(1L:2L, 4L)])
  )
  # single condition display with a more complex condition

  large.cond <- simpleWarning(
    paste0(collapse="\n",
      c(
        "This is a complicated warning:",
        as.character(
          unitizer:::UL(c("one warning", "two warning", "three warning"))
      ) )
    ),
    quote(make_a_warning())
  )
  lst3 <- new("conditionList", .items=list(large.cond))
  show1 <- capture.output(show(lst3))
  expect_equal_to_reference(
    show1,
    file.path("helper", "refobjs", "misc_cndlistshow1.rds")
  )
  attr(lst3[[1L]], "unitizer.printed") <- TRUE
  lst3[[2L]] <- simpleWarning("warning2", quote(yo2 + yoyo))
  show2 <- capture.output(show(lst3))
  expect_equal_to_reference(
    show2,
    file.path("helper", "refobjs", "misc_cndlistshow2.rds")
  )
  # empty condition
  expect_equal(capture.output(show(lst3[0])), "Empty condition list")
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
  expect_true(unitizer:::identical_fun(base::print, base::print))
  # make sure all.equal dispatches properly out of namespace
  expect_equal(
    evalq(
      all.equal(
        new("conditionList", .items=list(
            simpleWarning("warning", quote(yo + yo)),
            simpleWarning("warning2", quote(yo2 + yo)),
            simpleWarning("warning3", quote(yo3 + yo)),
            simpleError("error1", quote(make_an_error()))
        ) ),
        new("conditionList", .items=list(
            simpleWarning("warning", quote(yo + yo)),
            simpleWarning("warning2", quote(yo2 + yo)),
            simpleError("error1", quote(make_an_error()))
      ) ) ),
      envir=getNamespace("stats")
    ),
    "Condition count mismatch; expected 4 (got 3)"
  )
} )
test_that("word_cat", {
  str <- "Humpty dumpty sat on a wall and took a big fall.  All the kings horses and men couldn't put humpty dumpty together again"
  expect_equal(
    capture.output(unitizer:::word_cat(str, width=20L)),
    c("Humpty dumpty sat on", "a wall and took a ", "big fall.  All the ", "kings horses and men", "couldn't put humpty ", "dumpty together ", "again")
  )
  expect_error(unitizer:::word_cat(stop("boom"), width=20L, sep=" "), "boom")
  str2 <- rep("goodbye goodbye")
  str1 <- rep("hello hello hello", 2)
  expect_equal(
    c("hello hello ", "hello hello ", "hello hello ", "goodbye ", "goodbye"),
    capture.output(unitizer:::word_cat(str1, str2, width=14L))
  )
  # Make sure default works

  old.width <- options(width=20L)
  on.exit(options(old.width))
  expect_equal(
    capture.output(unitizer:::word_cat(str)),
    c("Humpty dumpty sat on", "a wall and took a ", "big fall.  All the ", "kings horses and men", "couldn't put humpty ", "dumpty together ", "again")
  )
})
test_that("relativize_path", {
  base <- file.path(system.file(package="unitizer"), "expkg")
  wd <- file.path(base, "infer")
  p1 <- file.path(wd, "R")
  p2 <- file.path(base, "unitizerdummypkg1")

  expect_equal(unitizer:::relativize_path(p1, wd), "R")
  expect_equal(unitizer:::relativize_path(p2, wd), "../unitizerdummypkg1")
  expect_equal(
    unitizer:::relativize_path(c(p1, p2), wd),
    c("R", "../unitizerdummypkg1")
  )
  expect_equal(
    unitizer:::relativize_path(c(p1, p2), wd),
    c("R", "../unitizerdummypkg1")
  )
  expect_equal(
    unitizer:::relativize_path(c(p1, p2, file.path("notarealpath", "foo")), wd),
    c("R", "../unitizerdummypkg1", file.path("notarealpath", "foo"))
  )
  expect_equal(
    unitizer:::relativize_path("/a/b/c/d/e/x.txt"),
    "/a/b/c/d/e/x.txt"
  )
  expect_equal(
    unitizer:::relativize_path("/a/b/c/d/e/x.txt", only.if.shorter=FALSE),
    do.call(
      file.path,
      c(
        as.list(
          rep(
            "..",
            length(
              unlist(strsplit(getwd(), .Platform$file.sep, fixed=TRUE))
            ) - 1L
        ) ),
        list("a/b/c/d/e/x.txt")
  ) ) )

})
test_that("path_clean", {
  expect_error(unitizer:::path_clean(list()), "must be character")
  expect_equal(unitizer:::path_clean(file.path("a", "", "b", "c")), file.path("a", "b", "c"))
})
test_that("unitizer:::merge_lists", {
  expect_equal(
    unitizer:::merge_lists(list(a=1, b=2), list(c=3)),
    list(a=1, b=2, c=3)
  )
  expect_equal(
    unitizer:::merge_lists(list(a=1, b=2, c=3), list(d=5, c=5)),
    list(a=1, b=2, c=5, d=5)
  )
  expect_equal(
    unitizer:::merge_lists(list(a=1, b=2, c=3), list(a=NULL, d=5, c=5)),
    list(a=NULL, b=2, c=5, d=5)
  )
})
test_that("is", {
  f <- tempfile()
  cat("hello\n", file=f)
  fc <- file(f, "r")
  expect_true(unitizer:::is.valid_con(fc))
  expect_true(unitizer:::is.valid_con(fc, f))
  expect_error(unitizer:::is.valid_con(fc, 1:5))
  expect_match(unitizer:::is.valid_con(fc, "tada"), "file name does not match")
  expect_true(unitizer:::is.open_con(fc))
  close(fc)
  unlink(f)
})
test_that("filename to storeid", {
  expect_equal(filename_to_storeid("tests.R"), "tests.unitizer")
  expect_warning(filename_to_storeid("tests.rock"), "Unable to translate")
})
test_that("pretty_path", {
  # not supposed to exist
  expect_warning(res <- unitizer:::pretty_path('xadfasdfxcfasdfasd'), NA)
  skip('fails CRAN')
  expect_identical(res, 'xadfasdfxcfasdfasd')
  expect_identical(unitizer:::pretty_path(normalizePath('.')), '.')
  expect_identical(
    unitizer:::pretty_path(
      file.path(system.file(package="stats"), "DESCRIPTION")
    ),
    "package:stats/DESCRIPTION"
  )
})
test_that("quit", {
  # for some reason cover tests run via travis can't handle the with_mock,
  # so we just use truly-quit=FALSE
  # with_mock(
  #   quit=function(...) stop("quit!\n"), {
  #     unitizer:::read_line_set_vals("y")
  #     expect_error(capture.output(unitizer:::unitizer_quit()), "quit!")
  #     unitizer:::read_line_set_vals("n")
  #     capture.output(uq2 <- unitizer:::unitizer_quit())
  #     expect_equal(uq2, NULL)
  #     unitizer:::read_line_set_vals(c("q", "q", "q", "q", "q", "q"))
  #     expect_error(capture.output(unitizer:::unitizer_quit()), "quit!")
  #   }
  # )
  unitizer:::read_line_set_vals("y")
  capture.output(q.res.1 <- unitizer:::unitizer_quit(truly.quit=FALSE))
  expect_true(q.res.1)
  unitizer:::read_line_set_vals("n")
  capture.output(q.res.2 <- unitizer:::unitizer_quit(truly.quit=FALSE))
  expect_false(q.res.2)
  unitizer:::read_line_set_vals(c("q", "q", "q", "q", "q", "q"))
  expect_message(
    capture.output(q.res.3 <- unitizer:::unitizer_quit(truly.quit=FALSE)),
    "Sorry"
  )
  expect_true(q.res.3)
  unitizer:::read_line_set_vals(NULL)
})
test_that("mock_item", {
  expect_is(mock_item(), "unitizerItem")
})
test_that("diff conditionList", {
  cond1 <- new(
    "conditionList",
    .items=list(
      simpleWarning("hello", call=quote(fun())),
      simpleWarning("goodbye", call=quote(fun()))
  ) )
  expect_is(diffobj::diffObj(cond1, cond1), "Diff")
})

test_that("Condition object structure", {
  # We're assuming a particular structure for the condition object in
  # `faux_prompt` and `unitizer_prompt` so we put in a test here to make sure it
  # doesn't change

  cond <- simpleError('hello')
  expect_true(is.list(cond))
  expect_true(identical(names(cond), c('message', 'call')))
  expect_true(identical(class(cond), c('simpleError', 'error', 'condition')))
})
