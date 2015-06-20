.onLoad <- function(libname, pkgname) {
  untz.opts <- list(
    unitizer.show.output=FALSE,              # Will display output/msg to stdout/stderr in addition to capturing it
    unitizer.disable.capt=FALSE,             # Will prevent capture
    unitizer.test.out.lines=c(50, 15),       # How many lines to display when showing test values, or truncate to if exceeds
    unitizer.test.fail.out.lines=c(10, 5),   # How many lines to display when showing failed objects (note banner means one more line than this displayed)
    unitizer.test.msg.lines=c(10, 3),        # How many lines to display when showing test errors, or truncate to if exceeds
    unitizer.prompt.b4.quit.time=10,         # If unitizer runs in fewer seconds than this and has no reviewed items, `Q` will quit directly without prompting for review
    unitizer.max.capture.chars=200000L,      # Maximum number of characters we allow capture of per test
    unitizer.history.file=tempfile(),        # File to use for `unitizer` history
    unitizer.search.path.keep=c("tools:rstudio", "package:unitizer"),  # what objects to keep on search path when initializing unitizer
    unitizer.reproducible.state=c(
      search.path=2L, options=2L, random.seed=2L, working.directory=2L
    ),
    unitizer.opts.base=.unitizer.opts.base,  # what to set options to when running in reproducible state
    unitizer.opts.asis=.unitizer.opts.sys,   # system dependent options that should not be changed
    unitizer.seed=                           # random seed to use by default, "Wichman-Hill" because default seed is massive
        list(seed=42L, kind="Wichmann-Hill")
  )
  options(untz.opts[setdiff(names(untz.opts), names(options()))])
}

.onUnload <- function(libpath) {
}

