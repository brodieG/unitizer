source(file.path("aammrtf", "mock.R"))
source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))

# Mostly makes sure the demo steps work, but since it is a convenient way of
# generating a unitizer with actual errors and so forth, we use it to test a few
# other things as well in the context of those unitizers

# - "in_pkg" -------------------------------------------------------------------

base.dir <- file.path(FLM, "tests", "extra")
in.pkg.file <- file.path(base.dir, "inpkg.R")

unitizer:::read_line_set_vals(c("Q"))
txt1 <- unitizer:::capture_output(unitize(in.pkg.file, interactive.mode = TRUE))
# `sub` needed due to inconsistencies in R 3.4 and 3.3 for top level error
# messages
txt1$message <- sub("^Error.*:", "", txt1$message)
txt1
unitizer:::read_line_set_vals(c("Q"))
unitize(in.pkg.file, state = in_pkg(), interactive.mode = TRUE)
unitizer:::read_line_set_vals(c("Q"))
try(unitize(in.pkg.file, state = in_pkg("ASDFASDFA"), interactive.mode = TRUE))

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

# In tests, initial version of package should be 0.1.0; the test store
# does not exist so it doesn't get overwritten with subsequent updates
# Note the initial install happens in the test running script

unitizer:::update_fastlm(".", version = "0.1.0")
inst_pak(".")
unitizer:::read_line_set_vals(c("Y", "Y", "Y", "Y", "Y"))
untz <- unitize(FLM.TEST.FILE, interactive.mode = TRUE)
is(untz, "unitizer_result")
print(untz)
# Re-running doesn't change unitizer
untz2 <- unitize(FLM.TEST.FILE, interactive.mode = TRUE)
print(untz2)
# Rejecting failed tests does not change unitizer
update_fastlm(".", version = "0.1.1")
inst_pak(".")

unitizer:::read_line_set_vals(c("N", "N", "Y"))
unitizer:::capture_output(
  untz3 <- unitize(FLM.TEST.FILE, interactive.mode = TRUE)
)
print(untz3)

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

any(grepl("qualified with L", txt8$message))

# - "demo changes" -------------------------------------------------------------
#
# Now actually accept the changes
unitizer:::read_line_set_vals(c("Y", "Y", "Y"))
untz5 <- unitize(FLM.TEST.FILE, interactive.mode = TRUE)
unitizer:::read_line_set_vals(NULL)

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
