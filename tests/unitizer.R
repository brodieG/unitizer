# Note, this assumes the WD is set to "tests" (i.e. as if run by R CMD check)

library(unitizer)

unitize("noninteractive/tests2.R")
unitize("noninteractive/sectionsRCMDCHECK.R")
