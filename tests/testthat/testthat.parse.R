library(testthat)
library(testor)

prs <- parse("tests/testthat/parse.data/file1.R")
dat <- getParseData(prs)

expect_equal(
  par.ids <- with(dat, testor:::top_level_parse_parents(id, parent)),
  c(12L, 0L, 12L, 12L, 12L, 12L, 12L, 45L, 45L, 45L, 0L, 45L, 45L,  45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 45L, 112L,  0L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L, 112L,  112L, 112L, 112L, 112L, 112L, 112L, 0L, 128L, 128L, 128L, 128L,  128L, 128L, 128L, 128L, 128L, 128L, 147L, 0L, 147L, 147L, 147L,  147L, 147L, 147L, 0L, 159L, 159L, 159L, 159L, 159L, 159L, 0L)
)

dat.split <- split(dat, par.ids)
expect_equal(
  par.ids.2 <- with(dat.split$`112`, testor:::top_level_parse_parents(id, parent, 112L)), 
  c(112L, 54L, 112L, 112L, 57L, 112L, 112L, 112L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L, 108L,  108L, 108L, 108L, 108L, 108L, 112L)
)
dat.split.2 <- split(dat.split$`112`, par.ids.2)

# Reset negative parents to zero due to the odd treatment of top level comments

dat.new <- transform(dat, parent=ifelse(parent < 0, 0L, parent))
par.ids <- with(dat.new, testor:::top_level_parse_parents(id, parent))
dat.new.split <- split(dat.new, par.ids)
par.ids.2 <- with(dat.new.split$`112`, testor:::top_level_parse_parents(id, parent, 112L))
dat.new.split.2 <- split(dat.new.split$`112`, par.ids.2)
