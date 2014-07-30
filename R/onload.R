.onLoad <- function(libname, pkgname) {
  options(unitizer.show.output=FALSE)         # Will display output/msg to stdout/stderr in addition to capturing it
  options(unitizer.disable.capt=FALSE)        # Will prevent capture
  options(unitizer.test.out.lines=c(50, 15))  # How many lines to display when showing test values, or truncate to if exceeds
  options(unitizer.test.msg.lines=c(10, 3))   # How many lines to display when showing test errors, or truncate to if exceeds
}
.onUnload <- function(libpath) {
  options(unitizer.show.output=NULL) 
  options(unitizer.disable.capt=NULL)  
  options(unitizer.test.out.lines=NULL)
  options(unitizer.test.msg.lines=NULL)  
}

