# Test out state tracking, etc

library(unitizer)
unitize_dir("tests/interactive/unitizer/", pattern="^state")

# Missing var should show up with a value instead of missing

unitizer:::word_msg("Missing vars should now show up")

xorb <- "once upon a time"
unitize_dir(
  "tests/interactive/unitizer/", pattern="^state",
  state=new("unitizerStatePristine", par.env=.GlobalEnv)
)
# Should get state warnings below

unitizer:::word_msg(
  "Running state1 with state0.unitizer with default reproducible",
  "make sure you don't accept changes\n"
)

unitize(
  "tests/interactive/unitizer/state1.R",
  "tests/interactive/unitizer/state0.unitizer"
)

unitizer:::word_msg(
  "Running state1 with state0.unitizer with no state tracking",
  "make sure you don't accept changes\n"
)

unitize(
  "tests/interactive/unitizer/state1.R",
  "tests/interactive/unitizer/state0.unitizer",
  state="off"
)
rm(xorb)

