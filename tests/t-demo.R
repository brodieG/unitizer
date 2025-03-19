source(file.path("aammrtf", "mock.R"))
source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))

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
