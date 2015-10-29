# Note, this assumes the WD is set to "tests" (i.e. as if run by R CMD check)

library(unitizer)
library(testthat)

unitize("unitizer/tests2.R")
unitize("unitizer/sectionsRCMDCHECK.R")
unitize("unitizer/translate.R")
