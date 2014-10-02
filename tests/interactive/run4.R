library(unitizer)

# Tests behavior of errors that occur on ignored tests.  Currently we display
# them the first time around, but don't display them thereafter; this matches
# up with the rest of unitizer behavior

unitize("tests/interactive/unitizer/ignoreerror.R")
unitize("tests/interactive/unitizer/ignoreerror2.R", "tests/interactive/unitizer/ignoreerror.unitizer")
