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
unitizer:::read_line_set_vals(c("Y", "Y"))
untz <- unitize(FLM.TEST.FILE, interactive.mode = TRUE)
untz
