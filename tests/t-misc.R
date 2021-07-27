source(file.path("_helper", "init.R"))
source(file.path("aammrtf", "ref.R")); make_ref_obj_funs("refobjs")

# - "Text wrapping" ------------------------------------------------------------

var <- "humpty dumpty sat on a truck and had a big dump"
# expect_true(all(nchar(unlist(unitizer:::text_wrap(var, 10))) <=
writeLines(unlist(unitizer:::text_wrap(var, 10)))
all(nchar(unlist(unitizer:::text_wrap(var, 10))) <= 10)

var2 <- rep(var, 4)
# expect_true(all(nchar(wrp <- unlist(unitizer:::text_wrap(var2,
#     c(20, 15)))) <= 20) && length(wrp) == 14)
writeLines(unlist(unitizer:::text_wrap(var2, c(20, 15))))
all(nchar(wrp <- unlist(unitizer:::text_wrap(var2, c(20, 15)))) <=
    20) && length(wrp) == 14

# - "Headers" ------------------------------------------------------------------

# these basically require visual inspection

unitizer:::H1("hello world")
unitizer:::H2("hello world")
unitizer:::H3("hello world")

# cause an error
try(print(unitizer:::H1(rep_len("hello world", 10))))

h.w.long <- paste0(rep_len("hello world", 10), collapse = " ")
unitizer:::H1(h.w.long)
unitizer:::H2(h.w.long)
print(unitizer:::H2("No margin"), margin = "none") # no extra line below

# - "Valid Names convert names to valid" ---------------------------------------

# expect_equal(unitizer:::valid_names("hello"), "hello")
unitizer:::valid_names("hello")
# expect_equal(unitizer:::valid_names(".hello"), ".hello")
unitizer:::valid_names(".hello")
# expect_equal(unitizer:::valid_names("1hello"), "`1hello`")
unitizer:::valid_names("1hello")
# expect_equal(unitizer:::valid_names("hello kitty"), "`hello kitty`")
unitizer:::valid_names("hello kitty")
# expect_equal(unitizer:::valid_names("h3llo"), "`h3llo`")
unitizer:::valid_names("h3llo")
# expect_equal(unitizer:::valid_names("h_llo"), "h_llo")
unitizer:::valid_names("h_llo")
# expect_equal(unitizer:::valid_names("$hot"), "`$hot`")
unitizer:::valid_names("$hot")
# expect_equal(unitizer:::valid_names("HELLO"), "HELLO")
unitizer:::valid_names("HELLO")

# - "strtrunc" -----------------------------------------------------------------

# expect_equal(unitizer:::strtrunc("hollywood is for starlets",
#     5), "ho...")
unitizer:::strtrunc("hollywood is for starlets", 5)
# expect_error(unitizer:::strtrunc(5, "hollywood is for starlets"))
try(unitizer:::strtrunc(5, "hollywood is for starlets"))

# - "environment name tools" ---------------------------------------------------

env1 <- new.env(parent = globalenv())
env2 <- new.env(parent = env1)
env3 <- new.env(parent = env2)
env4 <- new.env(parent = env3)
# expect_true(is.character(ename <- unitizer:::env_name(env3)) &&
#     identical(length(ename), 1L))
is.character(ename <- unitizer:::env_name(env3)) && identical(length(ename), 1L)
# expect_true(is.character(envanc <- unitizer:::env_ancestry(env4)) &&
#     identical(length(envanc), 5L) && identical(envanc[[5L]],
#     "R_GlobalEnv"))
is.character(envanc <- unitizer:::env_ancestry(env4)) &&
  identical(length(envanc), 5L) && identical(envanc[[5L]], "R_GlobalEnv")

# - "deparse peek" -------------------------------------------------------------

expr1 <- quote(1 + 1 + 3)
expr2 <- quote(for (i in 1:100) {
    loop.val <- sample(1:1000, 200, replace = TRUE)
    loop.val <- loop.val * 200/3000 * mean(runif(20000))
})
# expect_equal("1 + 1 + 3", unitizer:::deparse_peek(expr1, 20L))
unitizer:::deparse_peek(expr1, 20L)

# expect_error(unitizer:::deparse_peek(expr1, 3L))
try(unitizer:::deparse_peek(expr1, 3L))
# expect_equal("1 ...", unitizer:::deparse_peek(expr1, 5L))
unitizer:::deparse_peek(expr1, 5L)

# expect_equal("for (i in 1:100) {    loop.val <- sam...", unitizer:::deparse_peek(expr2,
#     40L))
unitizer:::deparse_peek(expr2, 40L)

# - "deparse fun" --------------------------------------------------------------

# expect_identical(unitizer:::deparse_fun(quote(fun)), "fun")
unitizer:::deparse_fun(quote(fun))
# expect_identical(unitizer:::deparse_fun(quote(function(x) NULL)),
#     NA_character_)
unitizer:::deparse_fun(quote(function(x) NULL))
# expect_identical(unitizer:::deparse_fun("hello"), character(0L))
unitizer:::deparse_fun("hello")

# - "deparse_prompt" -----------------------------------------------------------

# expect_identical(unitizer:::deparse_prompt(quote(if (TRUE) {
#     25
# } else {
#     42
# })), c("> if (TRUE) {", "+     25", "+ } else {", "+     42",
#     "+ }"))
unitizer:::deparse_prompt(quote(if (TRUE) {
    25
} else {
    42
}))

# - "deparse_mixed" ------------------------------------------------------------

b <- setNames(1:3, letters[1:3])
x <- quote(1 + b)
x[[3]] <- b
# expect_equal(unitizer:::deparse_mixed(x), "quote(1 + 1:3)")
unitizer:::deparse_mixed(x)
y <- quote(1 + 3 + b)
y[[3]] <- b
# expect_equal(unitizer:::deparse_mixed(y), "quote(1 + 3 + 1:3)")
unitizer:::deparse_mixed(y)

# - "(Un)ordered Lists" --------------------------------------------------------

vec <- c("hello htere how are you blah blah blah blah blah",
    "this is helpful you know", "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
    letters[1:10])

# expect_equal(as.character(unitizer:::OL(vec), width = 100L),
#     c(" 1. hello htere how are you blah blah blah blah blah",
#         " 2. this is helpful you know", " 3. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut ",
#         "    labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco ",
#         "    laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in ",
#         "    voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat ",
#         "    non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
#         " 4. a", " 5. b", " 6. c", " 7. d", " 8. e", " 9. f",
#         "10. g", "11. h", "12. i", "13. j"))
writeLines(as.character(unitizer:::OL(vec), width = 100L))

# expect_equal(as.character(unitizer:::UL(vec), width = 20L), c("- hello htere how ",
#     "  are you blah blah ", "  blah blah blah", "- this is helpful ",
#     "  you know", "- Lorem ipsum dolor ", "  sit amet, consec-",
#     "  tetur adipisicing ", "  elit, sed do ", "  eiusmod tempor ",
#     "  incididunt ut ", "  labore et dolore ", "  magna aliqua. Ut ",
#     "  enim ad minim ", "  veniam, quis ", "  nostrud exer-",
#     "  citation ullamco ", "  laboris nisi ut ", "  aliquip ex ea ",
#     "  commodo consequat.", "  Duis aute irure ", "  dolor in reprehen-",
#     "  derit in voluptate", "  velit esse cillum ", "  dolore eu fugiat ",
#     "  nulla pariatur. ", "  Excepteur sint ", "  occaecat cupidatat",
#     "  non proident, sunt", "  in culpa qui ", "  officia deserunt ",
#     "  mollit anim id est", "  laborum.", "- a", "- b", "- c",
#     "- d", "- e", "- f", "- g", "- h", "- i", "- j"))

writeLines(as.character(unitizer:::UL(vec), width = 20L))

# test_that("Messing with traceback", {
#   warning("Missing traceback tests")
#   # Main problem with this is that there may not be a good way to cause a trace
#   # back to register while not also stopping execution of this file, so not
#   # sure if this can be tested
# } )

# - "Compare Conditions" -------------------------------------------------------

lst1 <- new("conditionList", .items = list(simpleWarning("warning",
    quote(yo + yo)), simpleWarning("warning2", quote(yo2 + yo)),
    simpleWarning("warning3", quote(yo3 + yo)), simpleError("error1",
        quote(make_an_error()))))
lst2 <- new("conditionList", .items = list(simpleWarning("warning",
    quote(yo + yo)), simpleWarning("warning2", quote(yo2 + yo)),
    simpleError("error1", quote(make_an_error()))))

all.equal(lst1, lst1)
# expect_equal("Condition count mismatch; expected 4 (got 3)",
#     all.equal(lst1, lst2))
all.equal(lst1, lst2)#
# expect_equal("There is one condition mismatch at index [[3]]",
#     all.equal(lst2, lst1[1L:3L]))
all.equal(lst2, lst1[1L:3L])
# expect_equal("There are 2 condition mismatches, first one at index [[1]]",
#     all.equal(lst2, lst1[2L:4L]))
all.equal(lst2, lst1[2L:4L])
attr(lst1[[3L]], "unitizer.printed") <- TRUE
# expect_equal("There is one condition mismatch at index [[3]]",
#     all.equal(lst2, lst1[1L:3L]))
all.equal(lst2, lst1[1L:3L])
# expect_equal(c("Condition type mismatch, `target` is 'Error', but `current` is 'Warning'",
#     "Condition mismatch may involve print/show methods; carefully review conditions with `.NEW$conditions` and `.REF$conditions` as just typing `.ref` or `.new` at the prompt will invoke print/show methods, which themselves may be the cause of the mismatch"),
#     all.equal(lst2[[3]], lst1[[3]]))
all.equal(lst2[[3]], lst1[[3]])

attr(lst1[[3L]], "unitizer.printed") <- NULL
lst1[[2L]] <- simpleWarning("warning2", quote(yo2 + yoyo))
# expect_equal("There is one condition mismatch at index [[2]]",
#     all.equal(lst2, lst1[c(1L:2L, 4L)]))
all.equal(lst2, lst1[c(1L:2L, 4L)])

# single condition display with a more complex condition
large.cond <- simpleWarning(paste0(collapse = "\n", c("This is a complicated warning:",
    as.character(unitizer:::UL(c("one warning", "two warning",
        "three warning"))))), quote(make_a_warning()))
lst3 <- new("conditionList", .items = list(large.cond))
show1 <- capture.output(show(lst3))
all.equal(show1, rds("misc_cndlistshow1"))

attr(lst3[[1L]], "unitizer.printed") <- TRUE
lst3[[2L]] <- simpleWarning("warning2", quote(yo2 + yoyo))
lst3

# empty condition
lst3[0]

# - "Compare Functions With Traces" --------------------------------------------

fun.a <- base::library
identical(fun.a, base::library)
trace(library, where = .BaseNamespaceEnv)
identical(fun.a, base::library)  # FALSE
unitizer:::identical_fun(fun.a, base::library)
unitizer:::identical_fun(base::library, fun.a)  # FALSE
untrace(library, where = .BaseNamespaceEnv)
# expect_error(unitizer:::identical_fun(1, base::library))
try(unitizer:::identical_fun(1, base::library))
# expect_error(unitizer:::identical_fun(base::library, 1))
try(unitizer:::identical_fun(base::library, 1))
unitizer:::identical_fun(base::print, base::print)
# make sure all.equal dispatches properly out of namespace

# expect_equal(evalq(all.equal(new("conditionList", .items = list(simpleWarning("warning",
#     quote(yo + yo)), simpleWarning("warning2", quote(yo2 + yo)),
#     simpleWarning("warning3", quote(yo3 + yo)), simpleError("error1",
#         quote(make_an_error())))), new("conditionList", .items = list(simpleWarning("warning",
#     quote(yo + yo)), simpleWarning("warning2", quote(yo2 + yo)),
#     simpleError("error1", quote(make_an_error()))))), envir = getNamespace("stats")),
#     "Condition count mismatch; expected 4 (got 3)")
evalq(all.equal(new("conditionList", .items = list(simpleWarning("warning",
    quote(yo + yo)), simpleWarning("warning2", quote(yo2 + yo)),
    simpleWarning("warning3", quote(yo3 + yo)), simpleError("error1",
        quote(make_an_error())))), new("conditionList", .items = list(simpleWarning("warning",
    quote(yo + yo)), simpleWarning("warning2", quote(yo2 + yo)),
    simpleError("error1", quote(make_an_error()))))), envir = getNamespace("stats"))

# - "word_cat" -----------------------------------------------------------------

str <- "Humpty dumpty sat on a wall and took a big fall.  All the kings horses and men couldn't put humpty dumpty together again"
# expect_equal(capture.output(unitizer:::word_cat(str, width = 20L)),
#     c("Humpty dumpty sat on", "a wall and took a ", "big fall.  All the ",
#         "kings horses and men", "couldn't put humpty ", "dumpty together ",
#         "again"))
unitizer:::word_cat(str, width = 20L)
# expect_error(unitizer:::word_cat(stop("boom"), width = 20L, sep = " "),
#     "boom")
try(unitizer:::word_cat(stop("boom"), width = 20L, sep = " "))
str2 <- rep("goodbye goodbye")
str1 <- rep("hello hello hello", 2)
# expect_equal(c("hello hello ", "hello hello ", "hello hello ",
#     "goodbye ", "goodbye"), capture.output())
unitizer:::word_cat(str1, str2, width = 14L)

# Make sure default works
old.width <- options(width = 20L)
# expect_equal(capture.output(unitizer:::word_cat(str)), c("Humpty dumpty sat on",
#     "a wall and took a ", "big fall.  All the ", "kings horses and men",
#     "couldn't put humpty ", "dumpty together ", "again"))
unitizer:::word_cat(str)
options(old.width)

# - "relativize_path" ----------------------------------------------------------

base <- file.path(system.file(package = "unitizer"), "expkg")
wd <- file.path(base, "infer")
p1 <- file.path(wd, "R")
p2 <- file.path(base, "unitizerdummypkg1")
# expect_equal(unitizer:::relativize_path(p1, wd), "R")
unitizer:::relativize_path(p1, wd)
# expect_equal(unitizer:::relativize_path(p2, wd), "../unitizerdummypkg1")
unitizer:::relativize_path(p2, wd)
# expect_equal(unitizer:::relativize_path(c(p1, p2), wd), c("R",
#     "../unitizerdummypkg1"))
unitizer:::relativize_path(c(p1, p2), wd)
# expect_equal(unitizer:::relativize_path(c(p1, p2), wd), c("R",
#     "../unitizerdummypkg1"))
unitizer:::relativize_path(c(p1, p2), wd)
# expect_equal(unitizer:::relativize_path(c(p1, p2, file.path("notarealpath",
#     "foo")), wd), c("R", "../unitizerdummypkg1", file.path("notarealpath",
#     "foo")))
unitizer:::relativize_path(
  c(p1, p2, file.path("notarealpath", "foo")), wd
)
# expect_equal(unitizer:::relativize_path("/a/b/c/d/e/x.txt"),
#     "/a/b/c/d/e/x.txt")
unitizer:::relativize_path("/a/b/c/d/e/x.txt")
all.equal(
  unitizer:::relativize_path("/a/b/c/d/e/x.txt", only.if.shorter = FALSE),
  do.call(
    file.path,
    c(
      as.list(
        rep(
          "..",
          length(unlist(strsplit(getwd(), .Platform$file.sep, fixed = TRUE))) -
          1L
      ) ),
      list("a/b/c/d/e/x.txt")
) ) )

# - "path_clean" ---------------------------------------------------------------

try(unitizer:::path_clean(list()))
unitizer:::path_clean(file.path("a", "", "b", "c"))

# - "unitizer:::merge_lists" ---------------------------------------------------

unitizer:::merge_lists(list(a = 1, b = 2), list(c = 3))
unitizer:::merge_lists(list(a = 1, b = 2, c = 3), list(d = 5, c = 5))
unitizer:::merge_lists(list(a = 1, b = 2, c = 3), list(a = NULL, d = 5, c = 5))

# - "filename to storeid" ------------------------------------------------------

filename_to_storeid("tests.R")
filename_to_storeid("tests.rock")

# - "pretty_path" --------------------------------------------------------------
# not supposed to exist
res <- unitizer:::pretty_path("xadfasdfxcfasdfasd")  # warn

if(FALSE) {
  # "fails CRAN"
  # expect_identical(res, "xadfasdfxcfasdfasd")
  res
  unitizer:::pretty_path(normalizePath("."))
  unitizer:::pretty_path(file.path(system.file(package = "stats"),
    "DESCRIPTION"))
}
# - "quit" ---------------------------------------------------------------------

# for some reason cover tests run via travis can't handle the with_mock,
# so we just use truly-quit=FALSE; UPDATE (mabye du to compiler?)
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
capture.output(q.res.1 <- unitizer:::unitizer_quit(truly.quit = FALSE))
q.res.1
unitizer:::read_line_set_vals("n")
capture.output(q.res.2 <- unitizer:::unitizer_quit(truly.quit = FALSE))
q.res.2  # FALSE
unitizer:::read_line_set_vals(c("q", "q", "q", "q", "q", "q"))
capture.output(q.res.3 <- unitizer:::unitizer_quit(truly.quit = FALSE))
q.res.3
unitizer:::read_line_set_vals(NULL)

# - "mock_item" ----------------------------------------------------------------

is(mock_item(), "unitizerItem")

# - "diff conditionList" -------------------------------------------------------

cond1 <- new("conditionList", .items = list(simpleWarning("hello",
    call = quote(fun())), simpleWarning("goodbye", call = quote(fun()))))
is(diffobj::diffObj(cond1, cond1), "Diff")

# - "Condition object structure" -----------------------------------------------

# We're assuming a particular structure for the condition object in
# `faux_prompt` and `unitizer_prompt` so we put in a test here to make sure it
# doesn't change
cond <- simpleError("hello")
is.list(cond)
identical(names(cond), c("message", "call"))
identical(class(cond), c("simpleError", "error", "condition"))

# - "options" ------------------------------------------------------------------

# not great tests...

old.opts <- options()
new.opts <- unitizer:::options_zero()

all(names(new.opts) %in% names(old.opts))
length(new.opts) <= length(old.opts)
options(old.opts)

