library(testthat)
library(unitizer)
context("Parse")

local( {
  txt <- "# This is an early comment

    hello <- 25

    # multi
    # line
    # comment

    matrix(1:9, 3)  # and another!

    unitizer_sect(\"here is a section\", {
      # test that were not crazy

      1 + 1 == 2   # TRUE hopefully

      # Still not crazy

      2 * 2 == 2 ^ 2
      # Tada
    } )
    sample(1:10)

    # and this comment belongs to whom?

    runif(20)
    print(\"woo\")  # and I?
    "
  all <- unitizer:::parse_dat_get(text=txt)
  prs <- all$expr
  dat <- all$dat
  dat$parent <- pmax(0L, dat$parent)
  dat.split <- dat.split.2 <- par.ids.3 <- NULL
  if.text <- "if # IFFY\n(x > 3 # ifcond\n){ hello\n #whome to attach?\n} else #final\ngoodbye"

  test_that("Top Level Parents Identified Correctly", {
    expect_equal(info="Identified top level parents?",
      par.ids <- with(dat, unitizer:::top_level_parse_parents(id, parent)),
      c(0L, 0L, 12L, 12L, 12L, 12L, 12L, 0L, 0L, 0L, 0L, 45L, 45L,  45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 0L, 0L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 0L, 128L, 128L, 128L, 128L, 128L,  128L, 128L, 128L, 128L, 128L, 0L, 0L, 147L, 147L, 147L, 147L,  147L, 147L, 0L, 159L, 159L, 159L, 159L, 159L, 159L, 0L)
    )
    dat.split <<- split(dat, par.ids)
    expect_equal(info="Identified sub-level top level parents correctly",
      par.ids.2 <- with(dat.split$`112`, unitizer:::top_level_parse_parents(id, parent, 112L)),
      c(54L, 112L, 112L, 57L, 112L, 112L, 112L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 112L)
    )
    dat.split.2 <<- split(dat.split$`112`, par.ids.2)
    expect_equal(info="Parent relationships in `unitizer_sect` piece.",
      par.ids.3 <<- with(dat.split.2$`108`, unitizer:::top_level_parse_parents(id, parent, 108L)),
      c(108L, 108L, 108L, 76L, 76L, 76L, 76L, 76L, 76L, 76L, 76L, 76L,  108L, 108L, 108L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L,  100L, 100L, 100L, 100L, 100L, 108L, 108L)
    )
  } )
  test_that("Comments Are Assigned", {
    expect_equal(info="Did we assign comments correctly to topmost level?",
      lapply(unitizer:::comments_assign(prs, dat.split$`0`), attr, "comment"),
      list("# This is an early comment", c("# multi", "# line", "# comment",  "# and another!"), NULL, NULL, "# and this comment belongs to whom?",      "# and I?")
    )
    expect_equal(info="No comments here so no changes should occur",
      unitizer:::comments_assign(prs[[3]], dat.split.2$`112`),
      prs[[3]]
    )
    expect_equal(info="Comments in `unitizer_sect` body assigned correctly",
      lapply(unitizer:::comments_assign(prs[[3]][[3]], split(dat.split.2$`108`, par.ids.3)$`108`), attr, "comment"),
      list(NULL, c("# test that were not crazy", "# TRUE hopefully" ), "# Still not crazy")
    )
  } )
  test_that("Ancestry Descend", {
    x <- unitizer:::parse_dat_get(text="1 + 1; fun(x, fun(y + z))")$dat
    expect_equal(
      structure(c(7L, 6L, 34L, 2L, 3L, 5L, 12L, 11L, 15L, 14L, 30L, 31L, 1L, 4L, 10L, 13L, 20L, 19L, 27L, 25L, 18L, 23L, 22L, 26L, 21L, 24L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L), .Dim = c(26L, 2L), .Dimnames = list(NULL, c("children", "level"))),
      unitizer:::ancestry_descend(x$id, x$parent, 0)
    )
  })
  test_that("Clean up Parse Data", {
    dat <- unitizer:::parse_dat_get(text="{function(x) NULL;; #comment\n}")$dat
    dat <- transform(dat, parent=ifelse(parent < 0, 0L, parent))  # set negative ids to be top level parents

    expect_equal(info="Ancestry Descend",
      structure(c(21L, 1L, 18L, 16L, 19L, 15L, 14L, 11L, 9L, 2L, 3L,  4L, 5L, 8L, 7L, 0L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 4L,  4L, 4L, 5L), .Dim = c(15L, 2L), .Dimnames = list(NULL, c("children",  "level"))),
      dat.anc <- unitizer:::ancestry_descend(dat$id, dat$parent, 0L)
    )
    expect_equal(info="Excise `exprlist`",
      c("expr", "'{'", "expr", "FUNCTION", "'('", "SYMBOL_FORMALS",  "')'", "NULL_CONST", "expr", "COMMENT", "'}'"),
      unitizer:::prsdat_fix_exprlist(dat, dat.anc)$token
    )
    dat.1 <- unitizer:::parse_dat_get(text="{1 ; ; ;2;}")$dat
    dat.1 <- transform(dat.1, parent=ifelse(parent < 0, 0L, parent))  # set negative ids to be top level parents

    expect_equal(info="Another `exprlist` test",
      list(c(0L, 18L, 3L, 18L, 12L, 18L, 18L), c("expr", "'{'", "NUM_CONST",  "expr", "NUM_CONST", "expr", "'}'")),
      unname(as.list(unitizer:::prsdat_fix_exprlist(dat.1, unitizer:::ancestry_descend(dat.1$id, dat.1$parent, 0L))[c("parent", "token")]))
    )
    dat.2 <- unitizer:::parse_dat_get(text="{NULL; yowza; #comment\nhello\n}")$dat
    dat.2 <- transform(dat.2, parent=ifelse(parent < 0, 0L, parent))  # set negative ids to be top level parents

    expect_equal(info="Yet another `exprlist`",
      list(c(0L, 22L, 3L, 22L, 9L, 22L, 22L, 17L, 22L, 22L), c("expr",  "'{'", "NULL_CONST", "expr", "SYMBOL", "expr", "COMMENT", "SYMBOL",  "expr", "'}'")),
      unname(as.list(unitizer:::prsdat_fix_exprlist(dat.2, unitizer:::ancestry_descend(dat.2$id, dat.2$parent, 0L))[c("parent", "token")]))
    )
    expect_equal(info="`for` cleanup",
      structure(list(id = c(1L, 3L, 5L, 7L, 27L, 9L, 24L, 10L, 11L, 12L, 14L, 13L, 16L, 17L, 18L, 20L, 21L, 22L), parent = c(30L, 30L, 7L, 30L, 30L, 27L, 27L, 24L, 24L, 14L, 24L, 24L, 17L, 24L, 24L, 21L, 24L, 27L), token = c("FOR", "SYMBOL", "SYMBOL", "expr", "expr", "'{'", "expr", "IF", "'('", "SYMBOL", "expr", "')'", "BREAK", "expr", "ELSE", "NEXT", "expr", "'}'")), .Names = c("id", "parent", "token")),
      as.list(unitizer:::prsdat_fix_for(unitizer:::parse_dat_get(text="for(i in x) {if(x) break else next}")$dat[-1L, ]))[c("id", "parent", "token")]
    )
    expect_equal(info="`if` cleanup",
      list(c(1L, 2L, 13L, 5L, 7L, 6L, 8L, 9L, 10L, 26L, 15L, 16L, 18L, 21L, 24L, 29L, 31L, 33L), c("IF", "COMMENT", "expr", "SYMBOL", "expr", "GT", "NUM_CONST", "expr", "COMMENT", "expr", "'{'", "SYMBOL", "expr", "COMMENT", "'}'", "COMMENT", "SYMBOL", "expr")),
      unname(as.list(unitizer:::prsdat_fix_if(unitizer:::parse_dat_get(text=if.text)$dat[-1,])[c("id", "token")]))
    )
  } )
  test_that("Full Parse Works Properly", {
    expect_equal(info="Full Comment Parse",
      list(NULL, list("# This is an early comment", list(NULL), list(NULL), list(NULL)), list(c("# multi", "# line", "# comment", "# and another!"), list(NULL), list(NULL, list(NULL), list(NULL), list(NULL)), list(NULL)), list(NULL, list(NULL), list(NULL), list(NULL, list(NULL), list(c("# test that were not crazy", "# TRUE hopefully"), list(NULL), list(NULL, list(NULL), list(NULL), list(NULL)), list(NULL)), list("# Still not crazy", list(NULL), list(NULL, list(NULL), list(NULL), list(NULL)), list(NULL,      list(NULL), list(NULL), list(NULL))))), list(NULL, list(NULL), list(NULL, list(NULL), list(NULL), list(NULL))), list("# and this comment belongs to whom?", list(NULL), list(NULL)), list("# and I?", list(NULL), list(NULL))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text=txt))
    )
    expect_equal(info="EQ_SUB and SYMBOL_SUB test",
      list(NULL, structure(list(NULL, list(NULL), list("# the data", list(NULL), list(NULL), list(NULL)), class = list(c("# the label", "#the equal sign", "# the class"))), .Names = c("", "", "", "class"))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="structure(1:3, # the data\nclass # the label\n=#the equal sign\n'hello' # the class\n)"))
    )
    expect_equal(info="Function with `exprlist`",
      list(NULL, list(NULL, list("#first arg"), structure(list(NULL, x = list(NULL), y = list(NULL)), .Names = c("", "x", "y")), list("#second arg with default", list(NULL), list("# first comment", list(NULL), list(NULL), list(NULL)), list("#second comment"), list("#lastcomment ", list(NULL), list(NULL), list(NULL))), list(NULL, list(NULL), list(NULL), list(NULL), list(NULL), list(NULL), list(NULL), list(NULL), list(NULL)))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="function(x #first arg\n, y=25 #second arg with default\n) {x + y; # first comment\n; yo #second comment\n x / y; #lastcomment \n;}"))
    )
    expect_equal(info="`for` loop",
      list(NULL, list(NULL, list(NULL), list("#in counter"), list("#incounter again", list(NULL), list(NULL), list(NULL)), list(NULL, list(NULL), list("# first comment", list(NULL), list(NULL), list(NULL)), list(NULL), list("#second comment"), list(NULL, list(NULL), list(NULL), list(NULL)), list("#lastcomment ")))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="for(i #in counter\nin 1:10#incounter again\n) {x + y; # first comment\n; next; yo #second comment\n x / y; break; #lastcomment \n;}"))
    )
    expect_equal(info="`if` statement",
      list(NULL, list(NULL, list("# IFFY"), list("# ifcond", list(NULL), list(NULL), list(NULL)), list(NULL, list(NULL), list(NULL)), list("#final"))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text=if.text))
    )
    expect_equal(info="formula",
      list(NULL, list("# hello", list(NULL), list(NULL), list(NULL)), list("#yowza", list("#bust a move"), list(NULL))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text=". + x # hello\n#yowza\n~#bust a move\ny"))
    )
    expect_equal(info="`repeat`",
      list(NULL, list(NULL, list("#first"), list(NULL, list(NULL), list("#comm", list(NULL), list(NULL)), list(NULL)))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="repeat #first\n{runif(10); #comm\nbreak;}"))
    )
    expect_equal(info="S4 slot",
      list(NULL, list(NULL, list(NULL), list(NULL, list("#comment"), list(NULL), list(NULL)), list(NULL))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="test@#comment\nhello <- 3"))
    )
    expect_equal(info="`while`",
      list(NULL, list(NULL, list(NULL), list("# a comment", list(NULL), list(NULL), list(NULL)), list(NULL, list(NULL), list(NULL), list(NULL)))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="while(x > 5 # a comment\n) { hello; goodbye } #yay"))
    )

    txt2 <- "library(functools)
      fun <- function(a=1, bravo, card=25, ..., xar=list(\"aurochs\", 1), z) {}

      # Need to add tests:
      # - with complex objects? (did I mean in the definition? Or the call??)
      (NULL)
      # These should be identical to match.call()

      body(fun) <- parse(text=\"{print(match_call()); print(match.call())}\")

      calls <- c(
        'fun(54, \"hello\", \"wowo\", \"blergh\", 8, 9)',
        'fun(54, \"hello\", \"wowo\", \"blergh\", a=8, z=9)',
        'fun(54, \"hello\", z=\"wowo\", \"blergh\", 8, 9)',
        'fun(54, \"hello\", z=\"wowo\", x=\"blergh\", 8, 9)',
        'fun(54, c=\"hello\", z=\"wowo\", xar=3, 8, 9)'
      )
      invisible(lapply(calls, function(x){cat(\"-- New Call --\", x, sep=\"\n\"); eval(parse(text=x))}))
    "
    test.comp <- unitizer:::comm_extract(unitizer:::parse_with_comments(text=txt2))
    expect_equal(info="A more complex test",
      list(c("# Need to add tests:", "# - with complex objects? (did I mean in the definition? Or the call??)"), "# These should be identical to match.call()"),
      lapply(test.comp[4:5], `[[`, 1)
    )
    expect_equal(info="Added SYMBOL_PACKAGE token",
      list(NULL, list(c("# a comment before", "#a comment after"))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="# a comment before\nunitizer:::browse()  #a comment after"))
    )
    expect_equal(info="Added SYMBOL_PACKAGE token v2",
      list(NULL, list(c("# a comment before", "#a comment after"))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text="# a comment before\nunitizer::browse()  #a comment after"))
    )
    # LBB used to break stuff

    txt3 <- "# This is an early comment
      hello <- 25
      # multi
      hello[[1]]  # and another!"
    expect_equal(info="LBB test",
      list(NULL, list("# This is an early comment", list(NULL), list(NULL), list(NULL)), list(c("# multi", "# and another!"), list(NULL), list(NULL), list(NULL))),
      unitizer:::comm_extract(unitizer:::parse_with_comments(text=txt3))
    )
  } )
  txt3 <- "# Calls to `library` and assignments are not normally considered tests, so
  # you will not be prompted to review them

  library(utzflm)
  x <- 1:100
  y <- x ^ 2
  res <- fastlm(x, y)

  res                     # first reviewable expression
  get_slope(res)
  get_rsq(res)

  fastlm(x, head(y))      # This should cause an error; press Y to add to store"
  expr <- unitizer:::parse_with_comments(text=txt3)
  my.unitizer <- new("unitizer", id=1, zero.env=new.env())
  capture.output(my.unitizer <- my.unitizer + expr)
  test_that("Weird missing comment on `res` works", {
    expect_identical(
      list(c("# Calls to `library` and assignments are not normally considered tests, so", "# you will not be prompted to review them"), character(0), character(0), character(0), "# first reviewable expression", character(0), character(0), "# This should cause an error; press Y to add to store"),
      lapply(unitizer:::as.list(my.unitizer@items.new), slot, "comment")
    )
  } )
  test_that("exprlist excission with negative par ids", {
    txt <- "# For random tests\n\nunitizer_sect(\"blah\", {\n  identity(1);\n})\n";
    prs.dat <- unitizer:::parse_dat_get(text=txt)$dat

    prs.dat <- transform(prs.dat, parent=ifelse(parent < 0, 0L, parent))  # set negative ids to be top level parents
    ancestry <- with(prs.dat, unitizer:::ancestry_descend(id, parent, 0L))

    x <- unitizer:::prsdat_fix_exprlist(prs.dat, ancestry)

    expect_identical(
      structure(c(1L, 36L, 6L, 8L, 7L, 9L, 11L, 10L, 32L, 14L, 24L, 16L, 18L, 17L, 19L, 20L, 21L, 30L, 33L, 0L, 0L, 8L, 36L, 36L, 11L, 36L, 36L, 36L, 32L, 32L, 18L, 24L, 24L, 20L, 24L, 24L, 32L, 36L), .Dim = c(19L, 2L)),
      unname(as.matrix(x[, 5:6]))
    )
  } )
  test_that("empty symbols handled okay", {
    txt <- "mtcars[1:10,]\n";  # the empty second argument to `[` caused problems before
    unitizer:::parse_with_comments(text=txt)  # shouldn't cause error
  })
  test_that("uncommenting works", {
    expect_identical(
      quote(library(utzflm)),
      unitizer:::uncomment(expr[[1]])
    )
    expect_equal(info="don't blow away function arg names",
      quote(function(a, b) NULL),
      unitizer:::uncomment(quote(function(a, b) NULL))
    )
    # Recover comments and uncomment

    txt <- ".alike(  # FALSE, match.call disabled
    quote(fun(b=fun2(x, y), 1, 3)),  # first sub.call
    quote(fun(NULL, fun2(a, b), 1)), # second sub.call
    alike_settings(lang.mode=1))"

    exp <- unitizer:::parse_with_comments(text=txt)
    candc <- unitizer:::comm_and_call_extract(exp)
    expect_identical(
      quote(.alike(quote(fun(b = fun2(x, y), 1, 3)), quote(fun(NULL, fun2(a, b), 1)), alike_settings(lang.mode = 1))),
      candc$call[[1L]]
    )
    expect_identical(
      c("# FALSE, match.call disabled", "# first sub.call", "# second sub.call"),
      candc$comments
    )
  } )
  test_that("failing parses produce proper errors", {
    txt <- "this is a + syntax error that cannot be parsed"
    expect_error(
      capture.output(unitizer:::parse_tests(text=txt), type="message"),
      "Unable to parse test file"
    )
    f <- tempfile()
    on.exit(unlink(f))
    cat(txt, "\n", sep="", file=f)
    expect_error(
      capture.output(unitizer:::parse_tests(f), type="message"),
      "Unable to parse test file"
    )
    # try in normal mode (just fall back to normal parse)

    expect_error(
      unitizer:::parse_tests(text=txt, comment=FALSE),
      "unexpected symbol"
    )
    expect_error(
      unitizer:::parse_tests(f, comment=FALSE), "unexpected symbol"
    )
  })
} )
