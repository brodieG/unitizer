.onLoad <- function(libname, pkgname) {
  options(unitizer.show.output=FALSE)         # Will display output/msg to stdout/stderr in addition to capturing it
  options(unitizer.disable.capt=FALSE)        # Will prevent capture
  options(unitizer.test.out.lines=c(50, 15))  # How many lines to display when showing test values, or truncate to if exceeds
  options(unitizer.test.fail.out.lines=c(10, 5))   # How many lines to display when showing failed objects (note banner means one more line than this displayed)
  options(unitizer.test.msg.lines=c(10, 3))        # How many lines to display when showing test errors, or truncate to if exceeds
  options(unitizer.prompt.b4.quit.time=10)    # If unitizer runs in fewer seconds than this and has no reviewed items, `Q` will quit directly without prompting for review
  options(unitizer.search.path.clean=TRUE)    # Whether to run in clean search path by default
  options(unitizer.max.capture.chars=200000)  # Maximum number of characters we allow capture of per test
}
.onUnload <- function(libpath) {
  options(unitizer.show.output=NULL)
  options(unitizer.disable.capt=NULL)
  options(unitizer.test.out.lines=NULL)
  options(unitizer.test.fail.out.lines=NULL)   # How many lines to display when showing failed objects (note banner means one more line than this displayed)
  options(unitizer.test.msg.lines=NULL)
  options(unitizer.prompt.b4.quit.time=NULL)
  options(unitizer.search.path.clean=NULL)
  options(unitizer.max.capture.chars=NULL)
}

