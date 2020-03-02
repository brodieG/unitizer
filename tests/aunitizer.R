# Note, this assumes the WD is set to "tests" (i.e. as if run by R CMD check)

library(unitizer)
if(!require(testthat)) stop("testthat required for tests")

local({
  suppressWarnings(RNGversion("3.5.2"))
  old.opt <- options(stringsAsFactors=TRUE)
  on.exit({options(old.opt); RNGversion(as.character(getRversion()))})
  unitize("unitizer/tests2.R")
  unitize("unitizer/sectionsRCMDCHECK.R")
  unitize("unitizer/translate.R")
})
