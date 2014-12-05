# Note, this assumes the WD is set to "tests" (i.e. as if run by R CMD check)

library(unitizer)

set.seed(1)
unitize("noninteractive/tests2.R")
unitize("noninteractive/sectionsRCMDCHECK.R")
