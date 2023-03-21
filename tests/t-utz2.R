# Overflow tests from testthat.unitize.R

source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))

library(unitizer)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "unreviewed variations" ----------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Test unreviewed
# Accept one and go to unreviewed
# Accept one more and browse and go to unreviewed
# Accept two remaining and confirm no unreviewed
# No unreviewed tests
unitizer:::read_line_set_vals(
  c("Y", "Q", "U", "Y", "B", "U", "Y", "Y", "U", "B", "U", "Q")
)
out <- unitizer:::capture_output(unitize(FLM.TEST.FILE, interactive.mode = TRUE))
unitizer:::clean_eval_exp(out)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Re-eval" ------------------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Test re-eval
# Re-eval and jump back to file 1
# Quit from file 1 and back to main menu
# Accept one test in file 2 and quit
# Go to file 3, accept one, and Re-eval all

unitizer:::read_line_set_vals(
  c("1", "Y", "R", "Y", "Q", "2", "Y", "Y", "3", "Y", "RR", "Y", "Q", "Q")
)
untz1 <- unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
print(untz1)
# remove temp file names and display
invisible(lapply(untz1, function(x) {print(x); cat('\n')}))

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Section Extra" ------------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Make sure that deleted items from a section are still marked from that
# section upgrade to version two to use the files that are set up for that
# there; notice update_fastlm_*extra*
# Re-set by dropping unitizers

unlink(list.dirs(FLM.TEST.DIR, recursive = FALSE), recursive = TRUE)
unitizer:::update_fastlm_extra(FLM)
inst_pak(FLM)
test.file.1 <- file.path(FLM.TEST.DIR, "unitizer.fastlm.R")
test.file.2 <- file.path(FLM.TEST.DIR, "unitizer.fastlm2.R")
test.store <- file.path(FLM.TEST.DIR, "store2.unitizer")
# First auto accept all initial tests, and then re-run with second version to
# make sure deleted tests are where we think they should be
out.1 <-
  unitizer:::capture_output(unitize(test.file.1, test.store, auto.accept = "new"))
unitizer:::read_line_set_vals(c("B", "Q"))
out.2 <- unitizer:::capture_output(
  untz.2 <- unitize(test.file.2, test.store, interactive.mode = TRUE)
)
attributes(untz.2) <- NULL
untz.2

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "warning when comp funs produce output" ------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Sections with comp funs that output to stdout/stderr
temp.loc <- tempfile()
dir.create(temp.loc)
file.copy(file.path(START.DIR, "_helper", "unitizers", "sects.R"), temp.loc)
f.sec <- file.path(temp.loc, "sects.R")
odir <- setwd(temp.loc)
out <- unitizer:::capture_output(
  unitize(f.sec, auto.accept = "new", interactive.mode = FALSE
) )

unitize(f.sec, interactive.mode = FALSE)
setwd(odir)
unlink(temp.loc, recursive = TRUE)

# - "Corner Case Files" --------------------------------------------------------

# Corner case files
# empty
temp.empty <- paste0(tempfile(), "-empty.R")
cat("\n", file = temp.empty)
empty.capt <- unitizer:::capture_output(unitize(temp.empty, force = TRUE))
# File, but does not end in .R
temp.bad <- paste0(tempfile())

cat("\n", file = temp.bad)
badname.capt <- unitizer:::capture_output(try(unitize(temp.bad)))
any(grepl("`get_unitizer` error", out$message))

any(grepl("Empty unitizer", empty.capt$output))
any(grepl("No valid unitizers available", badname.capt$message))
unlink(c(temp.empty, temp.bad))

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Re-eval change" -----------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# re-eval reeval with a modified file
temp.reeval.base <- paste0(tempfile(), "-reeval")
temp.reeval <- paste0(temp.reeval.base, ".R")
temp.reeval.utz <- paste0(temp.reeval.base, ".unitizer")
cat("1 + 1\n2 + 2\n", file = temp.reeval)
# force re-review
unitizer:::read_line_set_vals(c("Y", "P", "Y", "cat(\"1 + 1\n2 + 3\n\", file=temp.reeval)",
    "R", "Y", "Q"))
# state = environment() so we can access variables from this local
reeval.capt <- unitizer:::capture_output(unitize(temp.reeval,
    state = environment(), interactive.mode = TRUE))
unlink(c(temp.reeval, temp.reeval.utz), recursive = TRUE)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Condition fail" -----------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Fail test with conditions
temp.cond.base <- paste0(tempfile(), "-cond")
temp.cond <- paste0(tempfile(), ".R")
temp.cond.utz <- paste0(tempfile(), ".unitizer")
cond.message <- "hello world"
cat("warning(cond.message)", file = temp.cond)
unitizer:::read_line_set_vals(c("Y", "Y"))
# state = environment() so we can access variables from this local
unitizer:::capture_output(
  unitize(temp.cond, state = environment(), interactive.mode = TRUE)
)
cond.message <- "goodbye world"
unitizer:::read_line_set_vals("Q")
cond.capt <-
unitizer:::capture_output(
  unitize(temp.cond, state = environment(), interactive.mode = TRUE)
)
sum(grepl("Conditions mismatch", cond.capt$output)) == 1L
unlink(c(temp.cond, temp.cond.utz), recursive = TRUE)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Force" --------------------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#
# Toggle force update, in order for this to work we need to create a situation
# where in a `unitize_dir`, one file passes, the other doesn't, and we review
# the file that passes.  Otherwise the only other way to do it is to set force
# from the beginning, but that ruins the toggle effect.  One possible issue
# here is that we don't have a great way to check the file actually changed.

temp.forceup.base.dir <- tempfile()
dir.create(temp.forceup.base.dir)
temp.forceup.1 <- file.path(temp.forceup.base.dir, "force1.R")
temp.forceup.2 <- file.path(temp.forceup.base.dir, "force2.R")
cat("force.var\n", file = temp.forceup.1)
cat("4 + 1\n", file = temp.forceup.2)
force.var <- 1
unitizer:::capture_output({
  unitize(temp.forceup.1, auto.accept = "new", state = environment())
  unitize(temp.forceup.2, auto.accept = "new", state = environment())
})
force.var <- 2
unitizer:::read_line_set_vals(c("2", "1", "Y", "O", "Q", "Q"))
force.capt <- unitizer:::capture_output(unitize_dir(temp.forceup.base.dir,
    state = environment(), interactive.mode = TRUE))
unlink(temp.forceup.base.dir, recursive = TRUE)
sum(grepl("Toggling force update mode ON", force.capt$message)) ==
    1L
sum(grepl("You are about to .* with re-evaluated", force.capt$message)) ==
    1L

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Compare Funs" -------------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Bad comparison functions
temp.bad.comp <- paste0(tempfile(), ".R")
cat("\n  unitizer_sect(\n    'bad comp', {1 + 1; 2 + 2},\n    compare=function(x, y) list('failed', 'comparison')\n  )\n",
    file = temp.bad.comp)
unitizer:::capture_output(unitize(temp.bad.comp, auto.accept = "new"))
unitizer:::read_line_set_vals(c("Q"))
bad.comp.capt <- unitizer:::capture_output(unitize(temp.bad.comp,
    interactive.mode = TRUE))
unlink(temp.bad.comp)

sum(grepl("Unable to compare value", bad.comp.capt$message)) == 1L
sum(grepl("Corrupted", bad.comp.capt$output)) >= 1L

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "bad map" ------------------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Bad store mapping functions

try(unitize_dir(FLM.TEST.DIR, store.ids = function(x) stop("Bad store map fun")))

unitizer:::read_line_set_vals(NULL)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Multiple Bookmarks" -------------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Issue 245: In review-all mode should not move to next unitizer until the
# review bookmark is cleared.

temp.dir <- tempfile()
dir.create(temp.dir)
writeLines("'hello'\n'world'", file.path(temp.dir, "a.R"))
writeLines("2 + 1\n3 + 2", file.path(temp.dir, "b.R"))
writeLines("pi\n2 * pi\nsqrt(pi)", file.path(temp.dir, "c.R"))
unitizer:::read_line_set_vals(
  c("A","N","N","Y","Y","R","Y","Q","Y","R","Y","Y","Y","Y","Q")
)
unitize_dir(temp.dir, interactive.mode=TRUE)

# Make sure re-eval all clears all bookmarks
unlink(
  list.files(temp.dir, full.names=TRUE, pattern="\\.unitizer$"),
  recursive=TRUE
)
unitizer:::read_line_set_vals(c("A","Q","Y","RR","Y","Q"))
unitize_dir(temp.dir, interactive.mode=TRUE)

unitizer:::read_line_set_vals(NULL)
unlink(temp.dir, recursive=TRUE)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Non-Standard Conditions" --------------------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Issue 272: some conditions don't produce any output, but for `unitizer` we
# consider normally non-test expressions that produce conditions as tests.  So
# we need a mechanism for clarifying what happened.

temp.dir <- tempfile()
temp.file <- file.path(temp.dir, 'a.R')
dir.create(temp.dir)

unitizer:::read_line_set_vals("Q")
writeLines(
  c(
    "cond <- simpleCondition('hello')",
    "class(cond) <- c('svgchop_unsupported', 'svgchop', class(cond))",
    "invisible(signalCondition(cond))"
  ),
  temp.file
)
unitize(temp.file, interactive.mode=TRUE)

unitizer:::read_line_set_vals(NULL)
unlink(temp.dir, recursive=TRUE)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Output Transcript in non-Interactive" -------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Issue 286: don't capture output in non-interactive.
# We need `try` because we're testing what happens when the unitizer fails.

temp.dir <- tempfile()
temp.file <- file.path(temp.dir, 'a.R')
dir.create(temp.dir)
writeLines('warning("boom")', temp.file)  # can't use error b/c try below
old.opt <- options(unitizer.transcript=NULL, unitizer.show.progress=TRUE)
try(unitize(temp.file))
options(old.opt)
unlink(temp.dir, recursive=TRUE)

# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
# - "Display All Tests in non-Interactive" -------------------------------------
# /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Issue 293: don't stop after first failing test
# We need `try` because we're testing what happens when the unitizer fails.

temp.dir <- tempfile()
temp.file.a <- file.path(temp.dir, 'a.R')
temp.file.b <- file.path(temp.dir, 'b.R')
dir.create(temp.dir)
writeLines(c("1 + 1", "warning('hello')"), temp.file.a)
writeLines(c("2 + 1", "warning('goodbye')"), temp.file.b)
try(unitize_dir(temp.dir, transcript=FALSE))
options(old.opt)
unlink(temp.dir, recursive=TRUE)

