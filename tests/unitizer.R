# Note, this assumes the WD is set to "tests" (i.e. as if run by R CMD check)

library(unitizer)

set.seed(1)
unitize("interactive/unitizer/tests2.R")
unitize("interactive/unitizer/sectionsRCMDCHECK.R")
