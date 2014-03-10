library(testthat)
library(testor)

prs <- parse("tests/testthat/parse.data/file1.R")
dat <- getParseData(prs)
dat$parent <- pmax(0L, dat$parent)

expect_equal(info="Identified top level parents?",
  par.ids <- with(dat, testor:::top_level_parse_parents(id, parent)),
  c(0L, 0L, 12L, 12L, 12L, 12L, 12L, 0L, 0L, 0L, 0L, 45L, 45L,  45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 0L, 0L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 0L, 128L, 128L, 128L, 128L, 128L,  128L, 128L, 128L, 128L, 128L, 0L, 0L, 147L, 147L, 147L, 147L,  147L, 147L, 0L, 159L, 159L, 159L, 159L, 159L, 159L, 0L)
)
dat.split <- split(dat, par.ids)

expect_equal(info="Did we assign comments correctly to topmost level?",
  lapply(testor:::comments_assign(prs, dat.split$`0`), attr, "comment"),
  list("# This is an early comment", c("# multi", "# line", "# comment",  "# and another!"), NULL, NULL, "# and this comment belongs to whom?",      "# and I?")
)

expect_equal(info="Identified sub-level top level parents correctly",
  par.ids.2 <- with(dat.split$`112`, testor:::top_level_parse_parents(id, parent, 112L)), 
  c(54L, 112L, 112L, 57L, 112L, 112L, 112L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 112L)
)
dat.split.2 <- split(dat.split$`112`, par.ids.2)

expect_equal(info="No comments here so no changes should occur",
  testor:::comments_assign(prs[[3]], dat.split.2$`112`),
  prs[[3]]
)
expect_equal(info="Parent relationships in `testor_sect` piece.",
  par.ids.3 <- with(dat.split.2$`108`, testor:::top_level_parse_parents(id, parent, 108L)),
  c(108L, 108L, 108L, 76L, 76L, 76L, 76L, 76L, 76L, 76L, 76L, 76L,  108L, 108L, 108L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L,  100L, 100L, 100L, 100L, 100L, 108L, 108L)
)
expect_equal(info="Comments in `testor_sect` body assigned correctly",
  testor:::comments_assign(prs[[3]][[3]], split(dat.split.2$`108`, par.ids.3)$`108`),
  list(NULL, c("# test that were not crazy", "# TRUE hopefully" ), "# Still not crazy")
)


