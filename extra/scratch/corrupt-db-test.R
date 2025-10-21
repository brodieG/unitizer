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
