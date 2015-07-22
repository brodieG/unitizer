# Test out state

library(unitizer)
unitize_dir("tests/interactive/unitizer/", pattern="^state")

# Should get state warnings below

message("Running state1 with state0.unitizer")

unitize(
  "tests/interactive/unitizer/state1.R",
  "tests/interactive/unitizer/state0.unitizer"
)

