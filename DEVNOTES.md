# Known Bugs

See git repo **[issues](https://github.com/brodieG/unitizer/issues)**.

# Internal Notes

This section contains developer notes for things that are unresolved, need
fixing, or hare-brained ideas for features.  Read at your own risk.

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
* And how to make it clear that tests are ordered first by test outcome, and as
  a result a display order will not be the same as the order in the file?
  Document clearly?
* removed tests coming last?  Trying to match them back to a section is too
  onerous, and besides, likely wouldn't have them in the same spot anyway.

## Internal

* There is heavy usage of `parent.env<-`, what do we do about the note in R docs
  stating this might be removed in the future?
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

# Scenarios to test

* Very large objects produced by tests
* test `description` for unitizer_section
* what do we do about capture.tests.R
* Need to add non-interactive tests
