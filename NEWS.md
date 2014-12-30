unitizer 0.6.3
--------------------------------------------------------------------------------

Bugfixes:

* stderr now show in `review` mode (issue #43)
* package startup messages suppressed (issue #23)
* small demo bug

unitizer 0.6.2
--------------------------------------------------------------------------------

Bugfixes:

* Better whitespace wrapping in terminal mode (partially addresses #38)
* Can now drop all items in review mode (issue #37)
* Workaround an R parse bug (issue #41)
* `traceback()` now works for `stop(simpleError(...))` type stops

Behavior changes:

* History is only subbed out if you need to type input (issue #40)

unitizer 0.6.1
--------------------------------------------------------------------------------

Minor release, no substantive changes.

Bugfixes:

* Loading a `unitizer` no longer automatically modifies it through `upgrade`
* `upgrade` cleaned up and has tests now
* calling functions in form `pkg::fun` or `pkg:::fun` no longer causes problems
  when checking for ignoredness

Behavior changes:

* `get` no longer warns if `unitizer` ids don't match

unitizer 0.6.0
--------------------------------------------------------------------------------

New Features:

* Added a demo (`demo(unitizer)`)
* Broke up and updated vignettes
* `unitize_dir` allows you to run all tests in a directory (issue #24)
* `review` allows you to review and drop tests from an existing `unitizer` store
  (issue #21)
* Test navigation mechanism improved (issue #26)
    * Typing R at the unitizer prompt now allows you to review all tests
    * You can skip ahead too
* `unitize(..., force.update=TRUE)` will overwrite unitizer even if there were
  no changes recorded (issue #19)

Behavior changes:

* `unitize` now runs with `search.path.clean=TRUE` by default

Bugfixes:

* Comparison function warnings not captured (issue #14)
* Search path restoration error messages fixed (issue #22)
* Navigation regressions fixed (issue #30)

Other:

Summary titles cleaned up, interative prompts made clearer, package reload warn
conflicts quieted (d2fe594c747, #23)

unitizer 0.5.0
--------------------------------------------------------------------------------

New Features:

* Can now run tests in clean environment (i.e. objects from .GlobalEnv will not
  be visible) (issue #13)
* Can now run tests with clean search path (i.e. only the basic R libraries are
  loaded) (also issue #13), use `unitize(..., search.path.clean=TRUE)`
* New vignette "Reproducible Tests" discusses the above features

Bugfixes:

* Expressions printed as tests evaluated now truncated corretly (issue #4)
* Incorrect displaying/hiding of ignored tests in some circumstances fixed

Other Improvements:

* Summary no longer includes "removed" tests in matrix, since those are section-
  less
* Other minor clean-up of the interactive environment prompting

unitizer 0.4.3
--------------------------------------------------------------------------------

Many interactive use bug fixes:

* LBB now parsed properly (issue #5)
* Non interactive parse (issue #11)
* Review and Back behavior consistent now in interactive use (issue #3)
* Other interactive use cleanup (issues #6, 12, 10, 9)
* Vignette now done properly


unitizer 0.4.2
--------------------------------------------------------------------------------

* Fixed setOldClass conflicts with RJSONIO (issue #1)
* Fixed run_ls not finding base env under certain circumstances (issue #2)
* Fixed conditionLists looping issue introduced when fixing issue #1
