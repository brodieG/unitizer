.onLoad <- function(libname, pkgname) {
  setOptIfNot("unitizer.show.output", FALSE)              # Will display output/msg to stdout/stderr in addition to capturing it
  setOptIfNot("unitizer.disable.capt", FALSE)             # Will prevent capture
  setOptIfNot("unitizer.test.out.lines", c(50, 15))       # How many lines to display when showing test values, or truncate to if exceeds
  setOptIfNot("unitizer.test.fail.out.lines", c(10, 5))   # How many lines to display when showing failed objects (note banner means one more line than this displayed)
  setOptIfNot("unitizer.test.msg.lines", c(10, 3))        # How many lines to display when showing test errors, or truncate to if exceeds
  setOptIfNot("unitizer.prompt.b4.quit.time", 10)         # If unitizer runs in fewer seconds than this and has no reviewed items, `Q` will quit directly without prompting for review
  setOptIfNot("unitizer.search.path.clean", TRUE)         # Whether to run in clean search path by default
  setOptIfNot("unitizer.max.capture.chars", 200000)       # Maximum number of characters we allow capture of per test
  setOptIfNot(                                            # what objects to keep on search path when initializing unitizer
    "unitizer.search.path.keep", c("tools:rstudio", "package:unitizer")
  )
}
.onUnload <- function(libpath) {
}

