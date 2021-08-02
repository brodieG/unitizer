source(file.path("_helper", "init.R"))

txt <- "# This is an early comment\n\n  hello <- 25\n\n  # multi\n  # line\n  # comment\n\n  matrix(1:9, 3)  # and another!\n\n  unitizer_sect(\"here is a section\", {\n    # test that were not crazy\n\n    1 + 1 == 2   # TRUE hopefully\n\n    # Still not crazy\n\n    2 * 2 == 2 ^ 2\n    # Tada\n  } )\n  sample(1:10)\n\n  # and this comment belongs to whom?\n\n  runif(20)\n  print(\"woo\")  # and I?\n  "
all <- unitizer:::parse_dat_get(text = txt)
prs <- all$expr
dat <- all$dat
dat$parent <- pmax(0L, dat$parent)
# With R4.0 some of the ids started changing
normalize_id <- function(dat) {
    idu <- sort(unique(dat[["id"]]))
    id <- with(dat, match(id, idu))
    parent <- with(dat, ifelse(parent == 0L, 0L, match(parent, 
        idu)))
    dat[["id"]] <- id
    dat[["parent"]] <- parent
    dat
}
dat <- normalize_id(dat)
dat.split <- dat.split.2 <- par.ids.3 <- NULL
if.text <- "if # IFFY\n(x > 3 # ifcond\n){ hello\n #whome to attach?\n} else #final\ngoodbye"

# - "Top Level Parents Identified Correctly" -----------------------------------

# "Identified top level parents?"
par.ids <- with(dat, unitizer:::top_level_parse_parents(id, parent))
par.ids
dat.split <- split(dat, par.ids)

# "Identified sub-level top level parents correctly"
par.ids.2 <- with(dat.split$`64`, unitizer:::top_level_parse_parents(id, 
    parent, 64L))
par.ids.2
dat.split.2 <- split(dat.split$`64`, par.ids.2)

# "Parent relationships in `unitizer_sect` piece."

par.ids.3 <- with(dat.split.2$`62`, unitizer:::top_level_parse_parents(id, 
    parent, 62L))
par.ids.3

# - "Comments Are Assigned" ----------------------------------------------------

# "Did we assign comments correctly to topmost level?"
lapply(unitizer:::comments_assign(prs, dat.split$`0`), attr, "comment")

# "No comments here so no changes should occur"
all.equal(unitizer:::comments_assign(prs[[3]], dat.split.2$`64`), prs[[3]])

# "Comments in `unitizer_sect` body assigned correctly"
lapply(unitizer:::comments_assign(prs[[3]][[3]], split(dat.split.2$`62`, 
    par.ids.3)$`62`), attr, "comment")

# - "Ancestry Descend" ---------------------------------------------------------

x <- unitizer:::parse_dat_get(text = "1 + 1; fun(x, fun(y + z))")$dat
x <- normalize_id(x)

unitizer:::ancestry_descend(x$id, x$parent, 0)

# - "Clean up Parse Data" ------------------------------------------------------

dat <- unitizer:::parse_dat_get(text = "{function(x) NULL;; #comment\n}")$dat
# set negative ids to be top level parents
dat <- transform(dat, parent = ifelse(parent < 0, 0L, parent))
dat <- normalize_id(dat)

# "Ancestry Descend"
dat.anc <- unitizer:::ancestry_descend(dat$id, dat$parent, 0L)
dat.anc

# "Excise `exprlist`"
unitizer:::prsdat_fix_exprlist(dat, dat.anc)$token

dat.1 <- unitizer:::parse_dat_get(text = "{1 ; ; ;2;}")$dat
# set negative ids to be top level parents
dat.1 <- transform(dat.1, parent = ifelse(parent < 0, 0L, parent))
dat.1 <- normalize_id(dat.1)

# "Another `exprlist` test"
unname(
  as.list(
    unitizer:::prsdat_fix_exprlist(
      dat.1, 
      unitizer:::ancestry_descend(dat.1$id, dat.1$parent, 0L)
    )[c("parent", "token")]
) )
dat.2 <- unitizer:::parse_dat_get(text = "{NULL; yowza; #comment\nhello\n}")$dat
# set negative ids to be top level parents
dat.2 <- transform(dat.2, parent = ifelse(parent < 0, 0L, parent))
dat.2 <- normalize_id(dat.2)

# "Yet another `exprlist`"
unname(
  as.list(
    unitizer:::prsdat_fix_exprlist(
      dat.2, unitizer:::ancestry_descend(dat.2$id, dat.2$parent, 0L)
    )[c("parent", "token")]
) )

dat.2a <- normalize_id(
  unitizer:::parse_dat_get(text = "for(i in x) {if(x) break else next}")$dat
)
# "`for` cleanup"

as.list(unitizer:::prsdat_fix_for(dat.2a[-1L, ]))

dat.3 <- normalize_id(unitizer:::parse_dat_get(text = if.text)$dat)

# "`if` cleanup"

unname(as.list(unitizer:::prsdat_fix_if(dat.3[-1, ])[c("id", "token")]))

# - "Full Parse Works Properly" ------------------------------------------------

# "Full Comment Parse"
unitizer:::comm_extract(unitizer:::parse_with_comments(text = txt))

# "EQ_SUB and SYMBOL_SUB test"
unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = "structure(1:3, # the data\nclass # the label\n=#the equal sign\n'hello' # the class\n)"
) )

# "Function with `exprlist`"

unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = "function(x #first arg\n, y=25 #second arg with default\n) {x + y; # first comment\n; yo #second comment\n x / y; #lastcomment \n;}"
) )

# "`for` loop"
unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = "for(i #in counter\nin 1:10#incounter again\n) {x + y; # first comment\n; next; yo #second comment\n x / y; break; #lastcomment \n;}"
) )

# "`if` statement"
unitizer:::comm_extract(unitizer:::parse_with_comments(text = if.text))

# "formula"
unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = ". + x # hello\n#yowza\n~#bust a move\ny"
) )

# "`repeat`"
unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = "repeat #first\n{runif(10); #comm\nbreak;}"
) )

# "S4 slot"
unitizer:::comm_extract(
  unitizer:::parse_with_comments(text = "test@#comment\nhello <- 3")
)

#  "`while`"
unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = "while(x > 5 # a comment\n) { hello; goodbye } #yay"
) )

txt2 <- "library(functools)\n    fun <- function(a=1, bravo, card=25, ..., xar=list(\"aurochs\", 1), z) {}\n\n    # Need to add tests:\n    # - with complex objects? (did I mean in the definition? Or the call??)\n    (NULL)\n    # These should be identical to match.call()\n\n    body(fun) <- parse(text=\"{print(match_call()); print(match.call())}\")\n\n    calls <- c(\n      'fun(54, \"hello\", \"wowo\", \"blergh\", 8, 9)',\n      'fun(54, \"hello\", \"wowo\", \"blergh\", a=8, z=9)',\n      'fun(54, \"hello\", z=\"wowo\", \"blergh\", 8, 9)',\n      'fun(54, \"hello\", z=\"wowo\", x=\"blergh\", 8, 9)',\n      'fun(54, c=\"hello\", z=\"wowo\", xar=3, 8, 9)'\n    )\n    invisible(lapply(calls, function(x){cat(\"-- New Call --\", x, sep=\"\n\"); eval(parse(text=x))}))\n  "
test.comp <- unitizer:::comm_extract(unitizer:::parse_with_comments(text = txt2))

# "A more complex test"
lapply(test.comp[4:5], `[[`, 1)

# "Added SYMBOL_PACKAGE token"
unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = "# a comment before\nunitizer:::browse()  #a comment after"
) )
# "Added SYMBOL_PACKAGE token v2" 
unitizer:::comm_extract(
  unitizer:::parse_with_comments(
    text = "# a comment before\nunitizer::browse()  #a comment after"
) )
# LBB used to break stuff
txt3 <- "# This is an early comment\n    hello <- 25\n    # multi\n    hello[[1]]  # and another!"
# "LBB test"
unitizer:::comm_extract(unitizer:::parse_with_comments(text = txt3))

# - "Weird missing comment on `res` works" -------------------------------------

txt3 <- "# Calls to `library` and assignments are not normally considered tests, so\n# you will not be prompted to review them\n\nlibrary(utzflm)\nx <- 1:100\ny <- x ^ 2\nres <- fastlm(x, y)\n\nres                     # first reviewable expression\nget_slope(res)\nget_rsq(res)\n\nfastlm(x, head(y))      # This should cause an error; press Y to add to store"
expr <- unitizer:::parse_with_comments(text = txt3)
my.unitizer <- new("unitizer", id = 1, zero.env = new.env())
capture.output(my.unitizer <- my.unitizer + expr)

lapply(unitizer:::as.list(my.unitizer@items.new), slot, "comment")

# - "exprlist excission with negative par ids" ---------------------------------

txt <- "# For random tests\n\nunitizer_sect(\"blah\", {\n  identity(1);\n})\n"
prs.dat <- unitizer:::parse_dat_get(text = txt)$dat
# set negative ids to be top level parents
prs.dat <- transform(prs.dat, parent = ifelse(parent < 0, 0L, 
    parent))
prs.dat <- normalize_id(prs.dat)
ancestry <- with(prs.dat, unitizer:::ancestry_descend(id, parent, 
    0L))
x <- unitizer:::prsdat_fix_exprlist(prs.dat, ancestry)
unname(as.matrix(x[, 5:6]))

# - "empty symbols handled okay" -----------------------------------------------

# the empty second argument to `[` caused problems before
txt <- "mtcars[1:10,]\n"
# shouldn't cause error
unitizer:::parse_with_comments(text = txt)

# - "uncommenting works" -------------------------------------------------------

unitizer:::uncomment(expr[[1]])

# "don't blow away function arg names"
unitizer:::uncomment(quote(function(a, b) NULL))
#
# Recover comments and uncomment
txt <- ".alike(  # FALSE, match.call disabled\n  quote(fun(b=fun2(x, y), 1, 3)),  # first sub.call\n  quote(fun(NULL, fun2(a, b), 1)), # second sub.call\n  alike_settings(lang.mode=1))"
exp <- unitizer:::parse_with_comments(text = txt)
candc <- unitizer:::comm_and_call_extract(exp)

candc$call[[1L]]

candc$comments

# - "failing parses produce proper errors" -------------------------------------

txt <- "this is a + syntax error that cannot be parsed"
try(capture.output(unitizer:::parse_tests(text = txt), type = "message"))
f <- tempfile()
on.exit(unlink(f))
cat(txt, "\n", sep = "", file = f)
try(capture.output(unitizer:::parse_tests(f), type = "message"))
# try in normal mode (just fall back to normal parse)
try(unitizer:::parse_tests(text = txt, comments = FALSE))

any(
  grepl(
    "unexpected symbol",
    capture.output(try(unitizer:::parse_tests(f, comments = FALSE)), type='message'),
) )

# - "NULL, constants, and new tokens" ------------------------------------------

txt <- c("a = 2", "# ho how", "b = 3", "", "b + a  # oh here", 
    "", "b + # oh there", "a   # bear", "", "NULL")
unitizer:::comm_extract(unitizer:::parse_with_comments(text = txt))
with.const <- unitizer:::parse_with_comments(text = "3 # comment on const")
unitizer:::symb_mark_rem(with.const[[1]])

