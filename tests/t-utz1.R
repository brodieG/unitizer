source(file.path("_helper", "init.R"))
source(file.path("aammrtf", "ref.R")); make_ref_obj_funs("refobjs")
source(file.path("_helper", "pkgs.R"))

# - "custom history file" ------------------------------------------------------

# Random history file
unitizer:::read_line_set_vals(c("1 + 1", "Y", "Y", "Y", "Y", "N"))
hist.file <- tempfile()
invisible(
  unitizer:::capture_output(
    unitize(FLM.TEST.FILE, interactive.mode = TRUE, history = hist.file)
) )
hist.dat <- readLines(hist.file)
unlink(hist.file)

# History only works in interactive mode
if (interactive()) {
  identical(hist.dat, c("## <unitizer> (original history will be restored on exit)",
    "library(utzflm, lib.loc = getOption(\"unitizer.tmp.lib.loc\"))",
    "dat <- data.frame(x = 1:100, y = (1:100)^2)", "res <- fastlm(dat$x, dat$y)",
    "res", "1 + 1", "get_slope(res)", "get_rsq(res)", "fastlm(1:100, 1:10)"))
} else {
  identical(hist.dat, character())
}
# - "bad history" --------------------------------------------------------------

bad.hist <- try(unitize(FLM.TEST.FILE, history = list()), silent = TRUE)
inherits(bad.hist, "try-error")
conditionMessage(attr(bad.hist, "condition"))

# - "bad seed" -----------------------------------------------------------------

# gsub needed b/c of inconsistent error calls in 3.3.2 and 3.4
old.opt <- options(unitizer.seed = "bad.seed")
txtopt1 <- unitizer:::capture_output(try(unitize(FLM.TEST.FILE)))
options(unitizer.seed = list("bad.seed"))
txtopt2 <- unitizer:::capture_output(try(unitize(FLM.TEST.FILE)))
# set.seed gained an argument c.a. R3.6 that caused error mismatch
txtopt2$message[grepl("\\(function \\(seed", txtopt2$message,
    ignore.case = TRUE)] <- ""
options(old.opt)


unitizer:::clean_eval_exp(txtopt1)
# supplied seed not valid int
# unexpectedly exited; not clear why all stderr is not being captured by
# capture_output...
txtopt2

# - "create dir" ---------------------------------------------------------------

# Unitizers in different directories that don't exist; also test using a
# function to generate those directories
get_store_id <- function(x) {
    file <- basename(x)
    dir <- dirname(dirname(x))
    file.path(dir, "unitizer2", sub("(.*)\\.R", "\\1.unitizer",
        file))
}
unitizer:::read_line_set_vals(c("N"))
txt1 <- unitizer:::capture_output(
  untz1 <- try(unitize_dir(FLM.TEST.DIR, get_store_id, interactive.mode = TRUE))
)
unitizer:::read_line_set_vals(c("Y", "Q"))
txt2 <- unitizer:::capture_output(untz2 <- unitize_dir(FLM.TEST.DIR,
    get_store_id, interactive.mode = TRUE))

inherits(untz1, "try-error")
inherits(untz2, "unitizer_results")

# Some of the text must be ablated
rem_txt <- function(x) {
    crd <- grep("Create directory\\?", x)
    if (!length(crd))
        stop("Logic Error: this must be a create directory test")
    x[-(2L:(crd[[1L]] - 1L))]
}
txt1$output <- rem_txt(txt1$output)
txt2$output <- rem_txt(txt2$output)

# must create the following directory
# cannot proceed w/o creating directories

txt1
txt2

# - print / dir ----------------------------------------------------------------

# quit from all at once
unitizer:::read_line_set_vals(c("A", "QQ", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)

# Now test `unitize_dir`; we are testing all different combination of whether
# a unitizer is accepted and updated
# Review all
# Accept all
# Quit
# Quit
# Re-evalute
# Review remaining
# Accept all
# Quit from review
# Quit completely

unitizer:::read_line_set_vals(c("A", "Y", "Y", "Y", "Y", "Y",
    "Q", "Q", "R", "A", "Y", "Y", "Q", "Q"))
untz3a <- unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
untz3a.get.all <- vapply(get_unitizer(untz3a), class, character(1L))
identical(untz3a.get.all, c("unitizer", "unitizer", "logical"))
print(untz3a)
untz3a.first <- untz3a[[1L]]
print(untz3a.first)

identical(class(untz3a), "unitizer_results")
identical(
  lapply(untz3a, class),
  replicate(3L, c("unitizer_result", "data.frame"), simplify = FALSE)
)

untz3a.cpy <- untz3a
# need to drop temp file attributes for tests
for (i in seq_along(untz3a.cpy)) {
    attr(untz3a.cpy[[i]], "test.file") <- basename(attr(untz3a.cpy[[i]],
        "test.file"))
    attr(untz3a.cpy[[i]], "store.id") <- basename(attr(untz3a.cpy[[i]],
        "store.id"))
}
all.equal(untz3a.cpy, rds("unitize_res1"))

# dummy class for errors
untz3a.first.bad <- untz3a.first
setClass("uhtsdfoqiuerhzb", slots=c(a='integer'))
attr(untz3a.first.bad, "store.id") <- new("uhtsdfoqiuerhzb")
print(untz3a.first.bad)

# this is a bit contrived as it isn't possible to directly create an empty
# unitize dir result
untz3a.empty <- untz3a[0]
class(untz3a.empty) <- class(untz3a)
print(untz3a.empty)

# Now accept the last remaining tests
# unlink(list.files(test.dir, pattern="\\.unitizer$", full.names=TRUE),
# recursive=TRUE)
# Invalid input
# Review third unitizer
# Accept all
# Re-eval and exit (again, not clear this is right thing to do)
unitizer:::read_line_set_vals(c("3000", "3", "Y", "Y", "Y", "Y",
    "R"))
untz3b <- unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
print(untz3b)
identical(
  vapply(get_unitizer(untz3b), class, character(1L)), rep("unitizer", 3L)
)
# - "namespace conflict" -------------------------------------------------------

# Namespace conflicts; unfortunately if either `covr` or `data.table` are
# loaded this may not work quite right.  Also as of `covr` 2.2.2 it seems that
# the R session `covr` launches now seems to load the covr namespace.  The
# logic here ensures covr namespace is always loaded for this tests, if
# possible.  So we omit the line were what namespaces could not be unloaded are
# mentioned.

unitizer:::read_line_set_vals("Y")
ns.conf1 <- unitizer:::capture_output(
  unitize_dir(FLM.TEST.DIR, state = "pristine", interactive.mode = TRUE)
)
ns.conf1$message <- ns.conf1$message[-3]
ns.conf1

unitizer:::read_line_set_vals("N")
ns.conf2 <- unitizer:::capture_output(
  unitize_dir(FLM.TEST.DIR, state = "pristine", interactive.mode = TRUE)
)
ns.conf2$message <- ns.conf2$message[-3]
ns.conf2

# Non-interactive; also testing what happens when we run a test with errors
# inside a try block

try(unitize_dir(FLM.TEST.DIR, state = "pristine", interactive.mode = FALSE))
ns.conf3 <- unitizer:::capture_output(
  try(
    unitize(
      file.path(FLM.TEST.DIR, "fastlm2.R"), state = "pristine",
      interactive.mode = FALSE
) ) )
ns.conf3$message <- ns.conf3$message[-grep('unloaded', ns.conf3$message)]
ns.conf3

# - "Removing Tests" -----------------------------------------------------------

# Removing tests; del2 has the same tests as del1, but with some removed
extra.dir <- file.path(FLM.TEST.DIR, "..", "extra")
unitize(file.path(extra.dir, "del1.R"), auto.accept = "new", interactive.mode = FALSE)
unitizer:::read_line_set_vals(c("Y", "YY", "Y", "Y"))
unitize(
  file.path(extra.dir, "del2.R"),
  store.id = file.path(extra.dir, "del1.unitizer"),
  interactive.mode = TRUE
)
# - "navigate" -----------------------------------------------------------------

# Update `fastlm` to cause unitizers to fail, and go through the errors
update_fastlm(FLM, version = "0.1.1")
inst_pak(FLM)
# Try navigating through the unitizer
unitizer:::read_line_set_vals(c("P", "B", "3", "N", "U", "N",
    "N", "B", "U", "Q"))
untz7a <- unitize(FLM.TEST.FILE, interactive.mode = TRUE)
attr(untz7a, "test.file") <- basename(attr(untz7a, "test.file"))
attr(untz7a, "store.id") <- basename(attr(untz7a, "store.id"))
path <- attr(untz7a, "test.file")
path
(path.norm <- unitizer:::normalize_path(path, mustWork=FALSE, exists=TRUE))
(rel.path <- unitizer:::relativize_path(path.norm, wd=NULL, only.if.shorter=TRUE, exists=TRUE))
(pkg.dir <- unitizer:::get_package_dir(path.norm, exists=TRUE))
untz7a

# - "review dir" ---------------------------------------------------------------

# list help, review first item, but do nothing
unitizer:::read_line_set_vals(c("H", "1", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
# incorrect selection
unitizer:::read_line_set_vals(c("H", "4", "1", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
# simulate slow unitizer review
old.opt <- options(unitizer.prompt.b4.quit.time = 0)
unitizer:::read_line_set_vals(c("H", "1", "Q", "Q", "Q", "Y"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
options(old.opt)
# Failures in non-interactive mode (note, can't run on the actual "fastlm.R"
# file b/c we need to do this under a `try`):
try(
  unitize_dir(FLM.TEST.DIR, pattern = "unitize|fastlm2", interactive.mode = FALSE)
)
# review all that need review, but don't do anything
unitizer:::read_line_set_vals(c("A", "Q", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
# review all, but don't do anything
unitizer:::read_line_set_vals(c("AA", "Q", "Q", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
# review one, and Re-eval despite no change
unitizer:::read_line_set_vals(c("1", "R", "Y", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
unitizer:::read_line_set_vals(c("1", "RR", "Y", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
# Test force eval
# first run, force update and accept
# second run, R from dir summary doesn't set bookmarks
unitizer:::read_line_set_vals(c("1", "O", "Q", "Y", "R", "1",
    "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
# Variations on YY, YYY, and YYY
unitizer:::read_line_set_vals(c("1", "YY", "Y", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
unitizer:::read_line_set_vals(c("1", "YYY", "Y", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
unitizer:::read_line_set_vals(c("1", "YYYY", "Y", "Q", "Q"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)
# review all, accepting all changes, and reevaluting everything; note that this
# means we're accepting tests that are not correct
unitizer:::read_line_set_vals(c("A", "Y", "Y", "Y", "Y", "Y", "Y", "RR"))
unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE)

# - "multi-sect" ---------------------------------------------------------------

# Upgrade again, and try with deleted tests and other things
update_fastlm(FLM, version = "0.1.2")
inst_pak(FLM)
unitizer:::read_line_set_vals(c("3", "ref(res)", "Y", "Y", "B",
    "1", "B", "U", "Y", "RR", "Y", "Q"))
txt20 <- unitizer:::capture_output(unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE))
txt20$output <- gsub("^<\\w+: .*?>", "", txt20$output)
txt20

# - "Load Fail" ----------------------------------------------------------------

# Purposefully mess up one of the unitizers to see if the load fail stuff works
saveRDS(list(1, 2, 3), file.path(FLM.TEST.DIR, "fastlm1.unitizer",
    "data.rds"))
try(unitize_dir(FLM.TEST.DIR, interactive.mode = TRUE))
unitizer:::read_line_set_vals(NULL)
