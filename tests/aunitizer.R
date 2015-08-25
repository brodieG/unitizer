# Note, this assumes the WD is set to "tests" (i.e. as if run by R CMD check)

library(unitizer)
library(testthat)

set.seed(1)
unitize("unitizer/tests2.R", state="off")
unitize("unitizer/sectionsRCMDCHECK.R", state="off")
unitize("unitizer/translate.R", state="off")
