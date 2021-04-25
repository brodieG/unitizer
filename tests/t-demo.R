source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))
source(file.path("_helper", "objects.R"))
source(file.path("_helper", "mock.R"))
list2env(make_file_funs("refobjs"), environment())

setwd(FLM)   # For whole test file to avoid temp file display

# Mostly makes sure the demo steps work, but since it is a convenient way of
# generating a unitizer with actual errors and so forth, we use it to test a few
# other things as well in the context of those unitizers

unlink(list.dirs(FLM.TEST.DIR, recursive = FALSE), recursive = TRUE)
#
# test_that("copy fastlm dir works", {
#     expect_identical(sort(list.files(".")), sort(c("DESCRIPTION",
#         "man", "NAMESPACE", "R", "tests", "utzflm.Rcheck")))
#     expect_identical(readLines(file.path(".", "DESCRIPTION"))[[5L]],
#         "Version: 0.1.0")
#     update_fastlm(".", version = "0.1.1")
#     expect_identical(readLines(file.path(".", "DESCRIPTION"))[[5L]],
#         "Version: 0.1.1")
#     update_fastlm(".", version = "0.1.2")
#     expect_identical(readLines(file.path(".", "DESCRIPTION"))[[5L]],
#         "Version: 0.1.2")
# })
# - "copy fastlm dir works" ----------------------------------------------------

sort(tolower(list.files(".")))
readLines(file.path(".", "DESCRIPTION"))[[5L]]  # v0.1.0
update_fastlm(".", version = "0.1.1")
readLines(file.path(".", "DESCRIPTION"))[[5L]]
update_fastlm(".", version = "0.1.2")
readLines(file.path(".", "DESCRIPTION"))[[5L]]

# - "show_file" ----------------------------------------------------------------

f <- tempfile()
cat("this is a\ntest code\nfile\n", file = f)
file.show <- capture.output(show_file(f))
file.show[[1L]]
start.file <- grep("+---+-----------+", file.show, fixed = TRUE)
length(start.file)  # 2
writeLines(file.show[start.file[[1L]]:start.file[[2L]]])
unlink(f)

# Run actual demo bits; note we want to force `interactive.mode=TRUE` so that
# `read_line_vals` values are used as user input; note that until we fix
# / rationalize how sinking behaves within unitizer when the standard streams
# come in sunk, we won't be able to fully test everything, since for example
# the display of the captured stdout just won't happen.

# - "demo create worked" -------------------------------------------------------

# use this options for rest of script

old.opt <- options(
  unitizer.color = FALSE, width = 80L, crayon.enabled = TRUE,
  diffobj.term.colors = 8
)
# options(unitizer.disable.capt=c(output=TRUE, message=FALSE))

# In tests, initial version of package should be 0.1.0; the test store
# does not exist so it doesn't get overwritten with subsequent updates
# Note the initial install happens in the test running script

unitizer:::update_fastlm(".", version = "0.1.0")
install.packages(
  ".", repos = NULL, type = "src", quiet = TRUE, lib = TMP.LIB
)
unitizer:::read_line_set_vals(c("Y", "Y", "Y", "Y", "Y"))
txt1 <- unitizer:::capture_output(untz <- unitize(FLM.TEST.FILE,
    interactive.mode = TRUE))
# Re-running doesn't change unitizer
txt2 <- unitizer:::capture_output(untz2 <- unitize(FLM.TEST.FILE,
    interactive.mode = TRUE))
# Rejecting failed tests does not change unitizer
update_fastlm(".", version = "0.1.1")
install.packages(
  ".", repos = NULL, type = "src", quiet = TRUE, lib = TMP.LIB
)
unitizer:::read_line_set_vals(c("N", "N", "Y"))
txt3 <- unitizer:::capture_output(untz3 <- unitize(FLM.TEST.FILE,
    interactive.mode = TRUE))
untz.clean <- lapply(list(untz, untz2, untz3), function(x) {
    attr(x, "test.file") <- basename(attr(x, "test.file"))
    attr(x, "store.id") <- basename(attr(x, "store.id"))
    x
})

# test_that("demo create worked", {
#     expect_is(untz, "unitizer_result")
#     expect_equal_to_reference(untz.clean[[3]], file.path("helper",
#         "refobjs", "demo_res1.rds"))
#     expect_equal_to_reference(untz.clean[[1]], file.path("helper",
#         "refobjs", "demo_res2.rds"))
#     expect_equal_to_reference(untz.clean[[2]], file.path("helper",
#         "refobjs", "demo_res3.rds"))
#     expect_match(paste0(txt1$output, collapse = ""), "\\+-+\\+| unitizer for: tests/unitizer/fastlm\\.R.*Pass Fail  New  1\\. <untitled>     -    -    4.*= Finalize Unitizer.*- Adding 4 out of 4 new tests")
#     expect_match(paste0(txt1$message, collapse = ""), "Error in fastlm\\(1:100, 1:10\\).*You will IRREVERSIBLY modify.*unitizer updated")
#     expect_match(paste0(txt2$message, collapse = ""), "| 4/4 tests passed; nothing to review.",
#         fixed = TRUE)
# })


is(untz, "unitizer_result")
all.equal(untz.clean[[3]], rds("demo_res1"))
all.equal(untz.clean[[1]], rds("demo_res2"))
all.equal(untz.clean[[2]], rds("demo_res3"))

writeLines(txt1$output)
writeLines(txt1$message)
writeLines(txt2$message)

# - "demo review" --------------------------------------------------------------

# review is always in interactive mode
unitizer:::read_line_set_vals(c("5", "Q"))
review(FLM.TEST.STORE)

# - "use.diff" -----------------------------------------------------------------

# Use this opportunity to make sure `use.diff=FALSE` works as intended
unitizer:::read_line_set_vals("Q")
unitize(FLM.TEST.FILE, interactive.mode = TRUE, use.diff = FALSE)
unitizer:::read_line_set_vals(c(".DIFF$state", "Q"))

unitize(FLM.TEST.FILE, interactive.mode = TRUE, use.diff = FALSE)

# - "failing diff" -------------------------------------------------------------

unitizer:::read_line_set_vals("Q")
mock(diffobj::diffObj, quote(stop("A failing diff.")))
unitize(FLM.TEST.FILE, interactive.mode = TRUE)
unmock(diffobj::diffObj)

# - "multi-accept" -------------------------------------------------------------

# Test what happens if we back out of a multi-accept

unitizer:::read_line_set_vals(c("YY", "N", "Q"))
unitize(FLM.TEST.FILE, interactive.mode = TRUE)

# - "multi-input" --------------------------------------------------------------

# Or if we request to go to unreviewed when there are none
unitizer:::read_line_set_vals(c("YY", "Y", "B", "U", "Q"))
unitize(FLM.TEST.FILE, interactive.mode = TRUE)

# - "warn in parse" ------------------------------------------------------------

# Make sure parse warnings are issued
unitizer:::read_line_set_vals(c("-2147483648L", "Q"))
txt8 <- unitizer:::capture_output(unitize(FLM.TEST.FILE,
    interactive.mode = TRUE))

# expect_true(any(grepl("qualified with L", txt8$message)))
any(grepl("qualified with L", txt8$message))
# test_that("demo changes", {
#     expect_match(paste0(txt5$output, collapse = ""), "Pass Fail <untitled>     2    2")
#     expect_match(paste0(txt5$message, collapse = ""), "| You will IRREVERSIBLY modify 'unitizer/fastlm1.unitizer' by:| - Replacing 2 out of 2 failed tests| unitizer updated.")
# })
# - "demo changes" -------------------------------------------------------------
#
# Now actually accept the changes
unitizer:::read_line_set_vals(c("Y", "Y", "Y"))
untz5 <- unitize(FLM.TEST.FILE, interactive.mode = TRUE)
unitizer:::read_line_set_vals(NULL)
options(old.opt)

# - "get_package_dir" ----------------------------------------------------------

# These were in t-get but we moved them here to avoid re-loading pkgs.R

unitizer:::get_package_dir(f)  # empty
test.dir.1 <- file.path(".", "utzflm.Rcheck", "utzflm", "R")
identical(
  unitizer:::get_package_dir(test.dir.1),
  normalizePath(dirname(test.dir.1), winslash = "/")
)
test.dir.2 <- file.path(".", "utzflm.Rcheck")
identical(
  unitizer:::get_package_dir(file.path(test.dir.2, "tests", "tests.R")),
  normalizePath(file.path(test.dir.2, "utzflm"), winslash = "/")
)
