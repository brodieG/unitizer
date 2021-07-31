source(file.path("_helper", "init.R"))

# Most tests involving global are scattered all over the place, just putting a
# few extra ones that are very specifically about global here
#
# - "Singleton Implementation Working" -----------------------------------------

invisible(unitizer:::unitizerGlobal$new()) # warn
glob.first <- unitizer:::unitizerGlobal$new(set.global = TRUE)
try(unitizer:::unitizerGlobal$new(set.global = TRUE))
try(unitizer:::unitizerGlobal$new())
glob.first$release()

# - "Dummy Display" ------------------------------------------------------------

show(new("unitizerDummy"))  # not recorded

# - "Disable / Enable" ---------------------------------------------------------

suppressWarnings(glob <- unitizer:::unitizerGlobal$new())
glob$disable()
glob$enable(c(search.path = 2L))  # warn state setting

