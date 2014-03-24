library(testthat)
library(testor)

local( {
  prs <- parse("tests/testthat/parse.data/file1.R")
  dat <- getParseData(prs)
  dat$parent <- pmax(0L, dat$parent)
  dat.split <- dat.split.2 <- par.ids.3 <- NULL

  test_that("Top Level Parents Identified Correctly", {
    expect_equal(info="Identified top level parents?",
      par.ids <- with(dat, testor:::top_level_parse_parents(id, parent)),
      c(0L, 0L, 12L, 12L, 12L, 12L, 12L, 0L, 0L, 0L, 0L, 45L, 45L,  45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 0L, 0L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 0L, 128L, 128L, 128L, 128L, 128L,  128L, 128L, 128L, 128L, 128L, 0L, 0L, 147L, 147L, 147L, 147L,  147L, 147L, 0L, 159L, 159L, 159L, 159L, 159L, 159L, 0L)
    )
    dat.split <<- split(dat, par.ids)
    expect_equal(info="Identified sub-level top level parents correctly",
      par.ids.2 <- with(dat.split$`112`, testor:::top_level_parse_parents(id, parent, 112L)), 
      c(54L, 112L, 112L, 57L, 112L, 112L, 112L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 112L)
    )
    dat.split.2 <<- split(dat.split$`112`, par.ids.2)
    expect_equal(info="Parent relationships in `testor_sect` piece.",
      par.ids.3 <<- with(dat.split.2$`108`, testor:::top_level_parse_parents(id, parent, 108L)),
      c(108L, 108L, 108L, 76L, 76L, 76L, 76L, 76L, 76L, 76L, 76L, 76L,  108L, 108L, 108L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L,  100L, 100L, 100L, 100L, 100L, 108L, 108L)
    )
  } )
  test_that("Comments Are Assigned", {
    expect_equal(info="Did we assign comments correctly to topmost level?",
      lapply(testor:::comments_assign(prs, dat.split$`0`), attr, "comment"),
      list("# This is an early comment", c("# multi", "# line", "# comment",  "# and another!"), NULL, NULL, "# and this comment belongs to whom?",      "# and I?")
    )
    expect_equal(info="No comments here so no changes should occur",
      testor:::comments_assign(prs[[3]], dat.split.2$`112`),
      prs[[3]]
    )
    expect_equal(info="Comments in `testor_sect` body assigned correctly",
      lapply(testor:::comments_assign(prs[[3]][[3]], split(dat.split.2$`108`, par.ids.3)$`108`), attr, "comment"),
      list(NULL, c("# test that were not crazy", "# TRUE hopefully" ), "# Still not crazy")
    )
  } )
  test_that("Clean up Parse Data", {
    dat <- getParseData(parse(text="{function(x) NULL;; #comment\n}"))
    expect_equal(info="Ancestry Descend",
      structure(c(21L, 1L, 18L, 16L, 19L, 15L, 14L, 11L, 9L, 2L, 3L,  4L, 5L, 8L, 7L, 0L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 4L,  4L, 4L, 5L), .Dim = c(15L, 2L), .Dimnames = list(NULL, c("children",  "level"))),
      testor:::ancestry_descend(dat$id, dat$parent, 0L)
    )
    expect_equal(info="Excise `exprlist`",
      c("expr", "'{'", "expr", "FUNCTION", "'('", "SYMBOL_FORMALS",  "')'", "NULL_CONST", "expr", "COMMENT", "'}'"),
      testor:::exprlist_remove(dat)$token
    )
    expect_equal(info="Another `exprlist` test",
      list(c(0L, 18L, 3L, 18L, 12L, 18L, 18L), c("expr", "'{'", "NUM_CONST",  "expr", "NUM_CONST", "expr", "'}'")),
      unname(as.list(testor:::exprlist_remove(getParseData(parse(text="{1 ; ; ;2;}")))[c("parent", "token")]))
    )
    expect_equal(info="Yet another `exprlist`"
      list(c(0L, 22L, 3L, 22L, 9L, 22L, 22L, 17L, 22L, 22L), c("expr",  "'{'", "NULL_CONST", "expr", "SYMBOL", "expr", "COMMENT", "SYMBOL",  "expr", "'}'")),
      unname(as.list(testor:::exprlist_remove(getParseData(parse(text="{NULL; yowza; #comment\nhello\n}")))[c("parent", "token")]))
    )
    expect_equal(info="`for` cleanup",
      structure(list(id = c(1L, 3L, 5L, 7L, 27L, 9L, 24L, 10L, 11L, 12L, 14L, 13L, 16L, 17L, 18L, 20L, 21L, 22L), parent = c(30L, 30L, 7L, 30L, 30L, 27L, 27L, 24L, 24L, 14L, 24L, 24L, 17L, 24L, 24L, 21L, 24L, 27L), token = c("FOR", "SYMBOL", "SYMBOL", "expr", "expr", "'{'", "expr", "IF", "'('", "SYMBOL", "expr", "')'", "BREAK", "expr", "ELSE", "NEXT", "expr", "'}'")), .Names = c("id", "parent", "token")),  
      as.list(testor:::prsdat_fix_for(getParseData(parse(text="for(i in x) {if(x) break else next}"))[-1L, ]))[c("id", "parent", "token")]
    )
  } )
  test_that("Full Parse Works Properly", {
    expect_equal(info="Full Comment Parse",
      list(NULL, 
        list("# This is an early comment", list(NULL), list(NULL), list(NULL)), 
        list(c("# multi", "# line", "# comment", "# and another!"), list(NULL), list(NULL, list(NULL), list(NULL), list(NULL)), list(NULL)), 
        list(NULL, list(NULL), list(NULL), 
          list(NULL, list(NULL), 
            list(c("# test that were not crazy", "# TRUE hopefully"), list(NULL), list(NULL, list(NULL), list(NULL), list(NULL)), list(NULL)), 
            list("# Still not crazy", list(NULL), list(NULL, list(NULL), list(NULL), list(NULL)), list(NULL, list(NULL), list(NULL), list(NULL))
        ) ) ),
        list(NULL, list(NULL), list(NULL, list(NULL), list(NULL), list(NULL))), 
        list("# and this comment belongs to whom?", list(NULL), list(NULL)), 
        list("# and I?", list(NULL), list(NULL))
      ),
      comm_extract(testor:::parse_data_assign(prs))
    )
    expect_equal(info="EQ_SUB and SYMBOL_SUB test",
      structure(list(NULL, "# the data", class = c("# the label", "#the equal sign",  "# the class")), .Names = c("", "", "class")),
      lapply(testor:::parse_data_assign(parse(text="structure(1:3, # the data\nclass # the label\n=#the equal sign\n'hello' # the class\n)"))[[1]], attr, "comment")
    )
    expect_equal(info="Function with `exprlist`",
      list(NULL, list(NULL, list("#first arg"), structure(list(NULL, x = list(NULL), y = list(NULL)), .Names = c("", "x", "y")), list("#second arg with default", list(NULL), list("# first comment", list(NULL), list(NULL), list(NULL)), list("#second comment"), list("#lastcomment ", list(NULL), list(NULL), list(NULL))), list(NULL, list(NULL), list(NULL), list(NULL), list(NULL), list(NULL), list(NULL), list(NULL), list(NULL)))),
      testor:::comm_extract(testor:::parse_data_assign(text="function(x #first arg\n, y=25 #second arg with default\n) {x + y; # first comment\n; yo #second comment\n x / y; #lastcomment \n;}"))
    )
  } )
} )


