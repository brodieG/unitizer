# devtools::install(); library(testthat); library(unitizer); setwd("tests/testthat")

if(!file_test("-d", file.path("helper", "refobjs")))
  stop("Make sure wd is set to tests/testthat")

library(unitizer)
library(testthat)
(.unitizer.fastlm <- copy_fastlm_to_tmpdir())    # package directory
devtools::install(.unitizer.fastlm, quiet=TRUE)  # install first version
.unitizer.test.file <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm.R")
.unitizer.test.store <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm.unitizer")
test.dir <- file.path(.unitizer.fastlm, "tests", "unitizer")

# Random history file

old.width <- options(width=80L)

unitizer:::read_line_set_vals(c("1 + 1", "Y", "Y", "Y", "Y", "N"))
hist.file <- tempfile()
unitize(.unitizer.test.file, interactive.mode=TRUE, history=hist.file)
hist.dat <- readLines(hist.file)
unlink(hist.file)

test_that("custom history file", {
  expect_identical(
    hist.dat,
    c("## <unitizer> (original history will be restored on exit)", "library(unitizer.fastlm)", "dat <- data.frame(x = 1:100, y = (1:100)^2)", "res <- fastlm(dat$x, dat$y)", "res", "1 + 1", "get_slope(res)", "get_rsq(res)", "fastlm(1:100, 1:10)")
  )
})
# Bad seed

old.opt <- options(unitizer.seed="bad.seed")
txtopt1 <- unitizer:::capture_output(try(unitize(.unitizer.test.file)))
options(unitizer.seed=list("bad.seed"))
txtopt2 <- unitizer:::capture_output(try(unitize(.unitizer.test.file)))
options(old.opt)

test_that("bad seed", {
  expect_equal_to_reference(
    txtopt1, file.path("helper", "refobjs", "unitize_txtop1.rds")
  )
  # supplied seed not valid int
  # unexpectedly exited
  expect_equal_to_reference(
    txtopt2, file.path("helper", "refobjs", "unitize_txtop2.rds")
  )
})

# Unitizers in different directories that don't exist; also test using a
# function to generate those directories

get_store_id <- function(x) {
  file <- basename(x)
  dir <- dirname(dirname(x))
  file.path(dir, "unitizer2", sub("(.*)\\.R", "\\1.unitizer", file))
}
unitizer:::read_line_set_vals(c("N"))
txt1 <- unitizer:::capture_output(
  untz1 <- try(unitize_dir(test.dir, get_store_id, interactive.mode=TRUE))
)
unitizer:::read_line_set_vals(c("Y", "Q"))
txt2 <- unitizer:::capture_output(
  untz2 <- unitize_dir(test.dir, get_store_id, interactive.mode=TRUE)
)
# Some of the text must be ablated

rem_txt <- function(x) {
  crd <- grep("Create directory\\?", x)
  if(!length(crd)) stop("Logic Error: this must be a create directory test")
  x[-(2L:(crd[[1L]] - 1L))]
}
txt1$output <- rem_txt(txt1$output)
txt2$output <- rem_txt(txt2$output)

test_that("create dir", {
  # must create the following directory
  # cannot proceed w/o creating directories

  expect_equal_to_reference(
    txt1, file.path("helper", "refobjs", "unitize_txtcd1.rds")
  )
  expect_equal_to_reference(
    txt2, file.path("helper", "refobjs", "unitize_txtcd2.rds")
  )
})
# Now test `unitize_dir`; we are testing all different combination of whether
# a unitizer is accepted and updated

unitizer:::read_line_set_vals(
  c(
    "A",                            # Review all
    "Y", "Y", "Y", "Y", "Y",        # Accept all
    "Q",                            # Quit
    "Q",                            # Quit
    "R",                            # Re-evalute
    "A",                            # Review remaining
    "Y", "Y",                       # Accept all
    "Q",                            # Quit from review
    "Q"                             # Quit completely
  )
)
txt3a <- unitizer:::capture_output(
  untz3a <- unitize_dir(test.dir, interactive.mode=TRUE)
)
untz3a.get.all <- vapply(get_unitizer(untz3a), class, character(1L))
untz3a.cpy <- untz3a
for(i in seq_along(untz3a.cpy)) {  # need to drop temp file attributes for tests
  attr(untz3a.cpy[[i]], "test.file") <- basename(attr(untz3a.cpy[[i]], "test.file"))
  attr(untz3a.cpy[[i]], "store.id") <- basename(attr(untz3a.cpy[[i]], "store.id"))
}
untz3a.all <- capture.output(print(untz3a))
untz3a.first <- capture.output(print(untz3a[[1L]]))

# Now accept the last remaining tests
# unlink(list.files(test.dir, pattern="\\.unitizer$", full.names=TRUE), recursive=TRUE)

unitizer:::read_line_set_vals(
  c(
    "3",                            # Review third unitizer
    "Y", "Y", "Y", "Y",             # Accept all
    "R"                             # Re-eval and exit (again, not clear this is right thing to do)
  )
)
txt3b <- unitizer:::capture_output(
  untz3b <- unitize_dir(test.dir, interactive.mode=TRUE)
)
untz3b.all <- capture.output(print(untz3b))
untz3b.get.all <- vapply(get_unitizer(untz3b), class, character(1L))

test_that("unitize_dir", {
  expect_equal_to_reference(
    txt3a, file.path("helper", "refobjs", "unitize_txtdir.rds")
  )
  expect_identical(
    class(untz3a), "unitizer_results"
  )
  expect_identical(
    lapply(untz3a, class), replicate(3L, c("unitizer_result", "data.frame"), simplify=FALSE)
  )
  expect_equal_to_reference(untz3a.all, file.path("helper", "refobjs", "unitize_resprint1.rds"))
  expect_equal_to_reference(untz3a.first, file.path("helper", "refobjs", "unitize_resprint2.rds"))

  expect_equal_to_reference(untz3a.cpy, file.path("helper", "refobjs", "unitize_res1.rds"))
  expect_equal(untz3a.get.all, c("unitizer", "unitizer", "logical"))

  expect_equal_to_reference(untz3a.all, file.path("helper", "refobjs", "unitize_resprint3.rds"))
  expect_equal(untz3b.get.all, c("unitizer", "unitizer", "unitizer"))
})
# Namespace conflicts; unfortunately if either `covr` or `data.table` are loaded
# this may not work quite right

old.keep.ns <- options(unitizer.namespace.keep=c("testthat"))
unitizer:::read_line_set_vals("Y")
txt4 <- unitizer:::capture_output(unitize_dir(test.dir, state="pristine", interactive.mode=TRUE))
unitizer:::read_line_set_vals("N")
txt5 <- unitizer:::capture_output(unitize_dir(test.dir, state="pristine", interactive.mode=TRUE))

# Non-interactive; also testing what happens when we run a test with errors
# inside a try block (txt6)

txt6 <- unitizer:::capture_output(
  try(unitize_dir(test.dir, state="pristine", interactive.mode=FALSE))
)
txt7 <- unitizer:::capture_output(
  try(unitize(file.path(test.dir, "fastlm2.R"), state="pristine", interactive.mode=FALSE))
)
options(old.keep.ns)

test_that("namespace conflict", {
  expect_equal_to_reference(
    txt4, file.path("helper", "refobjs", "unitize_nsconf1.rds")
  )
  expect_equal_to_reference(
    txt5, file.path("helper", "refobjs", "unitize_nsconf2.rds")
  )
  expect_equal_to_reference(
    txt6, file.path("helper", "refobjs", "unitize_errintry.rds")
  )
  expect_equal_to_reference(
    txt7, file.path("helper", "refobjs", "unitize_nsconf3.rds")
  )
})
# Update `fastlm` to cause unitizers to fail, and go through the errors

update_fastlm(.unitizer.fastlm, version="0.1.1")
devtools::install(.unitizer.fastlm)

# Try navigating through the unitizer

unitizer:::read_line_set_vals(c("P", "B", "3", "N", "U", "N", "N", "B", "U", "Q"))
txt7a <- unitizer:::capture_output(
  untz7a <- unitize(.unitizer.test.file, interactive.mode=TRUE)
)
attr(untz7a, "test.file") <- basename(attr(untz7a, "test.file"))
attr(untz7a, "store.id") <- basename(attr(untz7a, "store.id"))

test_that("navigate", {
  expect_equal_to_reference(
    txt7a, file.path("helper", "refobjs", "unitize_nav1.rds")
  )
  expect_equal_to_reference(
    untz7a, file.path("helper", "refobjs", "unitize_res7a.rds")
  )
})
# list help, review first item, but do nothing

unitizer:::read_line_set_vals(c("H", "1", "Q", "Q"))
txt8 <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

# incorrect selection

unitizer:::read_line_set_vals(c("H", "4", "1", "Q", "Q"))
txt8a <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

# simulate slow unitizer review

old.opt <- options(unitizer.prompt.b4.quit.time=0)
unitizer:::read_line_set_vals(c("H", "1", "Q", "Q", "Q", "Y"))
txt8b <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))
options(old.opt)

# Failures in non-interactive mode (note, can't run on the actual "fastlm.R"
# file b/c we need to do this under a `try`):

txt8c <- unitizer:::capture_output(
  try(unitize_dir(test.dir, pattern="unitize|fastlm2", interactive.mode=FALSE))
)
# review all that need review, but don't do anything

unitizer:::read_line_set_vals(c("A", "Q", "Q", "Q"))
txt9 <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

# review all, but don't do anything

unitizer:::read_line_set_vals(c("AA", "Q", "Q", "Q", "Q"))
txt10 <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

# review one, and Re-eval despite no change

unitizer:::read_line_set_vals(c("1", "R", "Y", "Q"))
txt12 <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

unitizer:::read_line_set_vals(c("1", "RR", "Y", "Q"))
txt12a <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

# Variations on YY, YYY, and YYY

unitizer:::read_line_set_vals(c("1", "YY", "Y", "Q", "Q"))
txt13 <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

unitizer:::read_line_set_vals(c("1", "YYY", "Y", "Q", "Q"))
txt13a <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

unitizer:::read_line_set_vals(c("1", "YYYY", "Y", "Q", "Q"))
txt13b <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

# review all, accepting all changes, and reevaluting everything; note that this
# means we're accepting tests that are not correct

unitizer:::read_line_set_vals(c("A", "Y", "Y", "Y", "Y", "Y", "Y", "RR"))
txt11 <- unitizer:::capture_output(
  untz11 <- unitize_dir(test.dir, interactive.mode=TRUE)
)

test_that("review dir", {
  expect_equal_to_reference(
    txt8, file.path("helper", "refobjs", "unitize_revdir1.rds")
  )
  expect_equal_to_reference(
    txt8a, file.path("helper", "refobjs", "unitize_revdir1a.rds")
  )
  expect_equal_to_reference(
    txt8b, file.path("helper", "refobjs", "unitize_revdir1b.rds")
  )
  expect_equal_to_reference(
    txt8c, file.path("helper", "refobjs", "unitize_revdir1c.rds")
  )
  expect_equal_to_reference(
    txt9, file.path("helper", "refobjs", "unitize_revdir2.rds")
  )
  expect_equal_to_reference(
    txt10, file.path("helper", "refobjs", "unitize_revdir3.rds")
  )
  expect_equal_to_reference(
    txt11, file.path("helper", "refobjs", "unitize_revdir4.rds")
  )
  expect_equal_to_reference(
    txt12, file.path("helper", "refobjs", "unitize_reeval1.rds")
  )
  expect_equal_to_reference(
    txt12a, file.path("helper", "refobjs", "unitize_reeval2.rds")
  )
  expect_equal_to_reference(
    txt13, file.path("helper", "refobjs", "unitize_multiaccept1.rds")
  )
  expect_equal_to_reference(
    txt13a, file.path("helper", "refobjs", "unitize_multiaccept2.rds")
  )
  expect_equal_to_reference(
    txt13b, file.path("helper", "refobjs", "unitize_multiaccept3.rds")
  )
})
# Upgrade again, and try with deleted tests and other things

update_fastlm(.unitizer.fastlm, version="0.1.2")
devtools::install(.unitizer.fastlm)

unitizer:::read_line_set_vals(c("3", "ref(res)", "Y", "Y", "B", "1", "B", "U", "Y", "RR", "Y", "Q"))
txt20 <- unitizer:::capture_output(unitize_dir(test.dir, interactive.mode=TRUE))

test_that("multi-sect", {
  txt20$output <- gsub("^<\\w+: .*?>", "", txt20$output)
  txt20.rds <- readRDS(file.path("helper", "refobjs", "unitize_multisect1.rds"))
  txt20.rds$output <- gsub("^<\\w+: .*?>", "", txt20.rds$output)
  expect_identical(txt20, txt20.rds)
})

unitizer_cleanup_demo()
unitizer:::read_line_set_vals(NULL)
options(old.width)
