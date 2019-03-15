# Note, this assumes the WD is set to "tests" (i.e. as if run by R CMD check)

library(unitizer)
if(!require(testthat)) stop("testthat required for tests")

RNGversion("3.5.2");
unitize("unitizer/tests2.R")
unitize("unitizer/sectionsRCMDCHECK.R")
unitize("unitizer/translate.R")
RNGversion(as.character(getRversion()))
