source(file.path("_helper", "init.R"))

# - "Ensure we get warning if we try to run in handlers" -----------------------

try(unitize("_helper/unitizers/trivial.R"))
tryCatch(unitize("_helper/unitizers/trivial.R"))
withRestarts(unitize("_helper/unitizers/trivial.R"))

# need to figure out why running this without `try` in covr causes cover to
# fail with
# Error in aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...) :
#   no rows to aggregate

# - "Ensure we get error if we try to do something stupid..." ------------------

try(
  withRestarts(
    unitize("_helper/unitizers/trivial.R"), 
    unitizerInteractiveFail = function() NULL
  )
)

