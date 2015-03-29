# Known Bugs

See git repo **[issues](https://github.com/brodieG/unitizer/issues)**.

# Internal Notes

This section contains developer notes for things that are unresolved, need
fixing, or hare-brained ideas for features.  Read at your own risk.

## Optim

* A lot of the overhead is related to the display and capture of text.  One
  possibility would be disabling the default printing and letting the user
  print if there are substantive issues (in particular, do we really need to
  print stuff for passed tests? maybe, if we need the reference stored stuff)
* using `run_ls` on new tests is super inefficient and is likely the next
  obvious point to optimize, but would take a little work in an area of the code
  that is a bit finicky
* `all.equal` adds a little overhead, but not a huge ammount
* `append` also adds some overhead


## Capture

* Should we really allow execution in interactive mode with stderr() and
  stdout() captured?  seems weird.
* implement capture handling at condition handler level and integrate output
  and condition streams so that we can do the replay? right now conditions come
  out of order when displaying test results
* should we capture actual stdout() vs stdout caused by a visible
  value separately?  probably yes, makes way more sense for the
  show.unitizeritem method
* need a better mechanism for handling the options changes for error and
  warning?  do we really want to change the error options? or allow warning=2?
* debugging partially implemented by disabling captures, but really,
  that's not real debugging.  unfortunately, because we capture std.err
  and we can't tee that, there is no good debugging to implement.
* should we capture all options in case user changes some that we don't expect
  and don't know what to do about?

## Browse

* should we keep objects that user creates while browsing across tests?
  probably, but we don't right now.
* accept all (hidden?) option
  * Right now comes up most when removing tests and wanting them all deleted
    w/o having to review every single one
* Deleted tests getting attributed to `Total` while showing a column of zeros is
  confusing; maybe should be it's own line
* Current behavior of automatically storing new non-tests and discarding
  reference non tests can be potentially confusing to someone examining the
  environment and not realizing the object they are looking at is not the same
  that was used in the evaluation of the reference test; need to think about
  now to handle this (throw warning whenever such objects are accessed?)
* Sometimes we want to replace a test with a variation on it, with the expectation
  that the result is unchanged; how do we provide a mechanism for the user to
  do this?  Some system to browse all the tests and extract the objects there-in?
* Should ESC be treated the same way as `Q`?  Right now causes an unexpected exit
  with loss of all work.
* Adding a browser inside `browse` source code, and then Q from browser() env
  causes an exit with loss of history; this is likely purely internal and not
  something a user would run into, but might be worth addressing to simplify
  debugging.  This seems to happen if we hit more than one browser() while
  evaluating.

## Order of Tests

* what do we do with default section that has values scattered throughout?
    * In particular, how do we handle this in review menu?
* And how to make it clear that tests are ordered first by test outcome, and as
  a result a display order will not be the same as the order in the file?
  Document clearly?
* removed tests coming last?  Trying to match them back to a section is too
  onerous, and besides, likely wouldn't have them in the same spot anyway.


## Internal

* There is heavy usage of `parent.env<-`, what do we do about the note in R docs
  stating this might be removed in the future? EDIT: most likely we should not be
  affected as devs seem mostly concerned about changing the search path at
  run time, which we do not do.
* need to think through intialize methods so hopefully we can avoid instantiating
  with .items argument for unitizerlist inheritors since that's a bit weird
* generally, need to add more methods so we're not messing with slots
  directly
* why are unitizer classes available even though they are not exported????
  do they have to be in order to support reading the stored objects?
  related: why is it that it seems that the generics are exported
  after documentation, but any subsequent re-installs apperas to blow them
  away?
* need to check whether there are non-standard re-starts configured?
  or do we just not support that? probably don't support it.
* when assigning comments, should change comment resetting so we don't
  need to go through the entire parse tree every time.
* short comments on short expressions should be appended to end of
  expression rather than to start?
* should `library` be ignored?  How do we reconcile that with the addition of the
  new clean path functions?  In theory `library` should be returning the same
  thing when using the clean paths.
* mechanism for submitting comments in language interferes with cases where we
  don't care about comments (e.g. when showing the review listing, or when
  matching stored calls to the newly parsed ones).  Having to blow away comments
  each time seems inefficient...

## Conditions

* Implement getCond() to supplement getConds()?  Or maybe add note to
  print.getConds() that makes it clear how to get the full message for a given
  condition?
* not sure that condition comparison function should automatically loop
  through lists. this complicates the function substantially, though it
  does have the advantage of allowing the user to handle the cases were
  condition lists have different number of entries; maybe there is an option
  to provide a list version vs. normal version? and if there is a mismatch
  in condition lengths, then actively tell the user they can use the
  list version?
* Can we reproduce the standard warning buffering behavior in R?  Right now
  need to run in `options(warn=1)` due to weird stuff that happens when trying
  to run in mode 0 while capturing output.
* Loosely related, how do we distinguish between a condition that looks like a
  warning / error but isn't actually generated by `warning` or `stop`?
* Logic of when to display errors when reviewing failed tests that cause errors?
  Right now seems like we'll always get an error shown if both ref and new have
  errors, but that isn't the same as normal values.  Obviously when ref doesn't
  cause an error, need to show as otherwise confusing.
* errors happening on compare don't set a traceback; should they?
* internally call matching is done on deparsed result: could this actually cause
  problems if deparsing rules change?  Need a better solution (`as.list` of
  expression, or some such? Or just compare calls with `identical`? alternate
  solutions seems potentially expensive)

## Misc

* provide option to only run new tests?  Makes incremental work on a large
  file easier so not everything has to be re-run
* backup of the unitizer .rds file should be created
* provide facilities to upate unitizer id when unitizers are moved, though
  typically the unitizer should always be in the same relative location
  to the script that runs it.

## S4

There is fairly extensive use of S4 internally.  In some cases it definitely
make sense, in other potentially less so.  In particular, we've leaned towards
making functionst that use S4 objects methods, even if there is only one
possible dispatch to contend with.  We also haven't been super consistent, as
in some cases we do not use S4 methods.  Need to rationalize all of this at
some point.

## Handling Passed Tests

Need to do this in order to implement a review capability for unitizers that
have been stored.

* Modify as.character.unitizerBrowse so we have the option of reviewing all
  tests, not just those that have been reviewed
* create a unitizerBrowseSubSectionPass
* Need to preserve reference sections, this is the single most important part.
  Looks like we need to, in the browse process, keep track of the section that
  each item was in.
* Main issue seems to be whether we can re-use browserPrep, which seems
  challenging since we would have completely different logic depending on
  whether it is being called from `unitize` or from `review`.
* Actually browserPrep is not the main issue.  The main issue is whether we can
  use `browse`, as that's where the PITA stuff happens.  This means we have to
  pull out the `browsePrep` stuff from `browse` and move it to `unitize` so we
  can have a different `browsePrep`for tests that we're reviewing.
* Do we need a new `unitizer` class? `unitizer`, which is for the live review
  of new and old tests, and `unitizerStore` which is the version that gets
  stored?  This resolves the problem with browserPrep.
* Or alternatively, do we spoof a version of a normal unitizer?  Move all the
  tests back to `items.new`, and then add a flag to `browserPrep`, this seems
  most promising, we just need to preserve all the data.  Maybe we don't actually
  move stuff to items.ref until we reload the unitizer, insted of doing so just
  before we save it.  But this may require too much re-org of existing structure
  (healenvs, etc.).

Strategy for recovering sections

* parent sections are tracked in the unitizerBrowse objects
* these are available in processInput
    * can attach section to each item
* then +,unitizer,unitizerItems-method will need to:
    * pull out section ids
    * copy sections from the new test section to reference
    * handle situations where sections were not recorded, gracefully

Potential issue: tracking sections from older reference tests

* Not really an issue so long as we re-assign the section always in processInput
* Main problem is for tests that are deleted from new source file but kept by
  user; these should just be assigned an NA section, which can be re-labeled as
  removed/missing tests when browsing, though might need some explanation

We want to re-use browse infrastructure as much as possible

* A version of browsePrep that handles items.ref instead of the just run tests
* Add all the passed tests to the existing version of browsePrep
* reviewNext needs a mode to suppress the passed tests, vs one that doesn't
* One problem here is that when re-loading a store, we're dealing with stuff in
  ref, whereas with passed tests we're dealing with stuff in new.  Does that mean
  that we need to move the reference stuff to new?  Probably, but not really
  desirable given all the dependencies involved with building up `items.new`
* Actually, re ^^, looks like we can just do this by passing hte ref items as
  new to the browser sub-section.

# Scenarios to test

* Very large objects produced by tests
* test `description` for unitizer_section
* what do we do about capture.tests.R
* Need to add non-interactive tests
