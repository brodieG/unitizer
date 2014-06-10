# Features

## Vignette

Self evident

## Capture

* use `evaluate::replay`?
* capture plots for testing (is this realistic?)
* Should we really allow execution in interactive mode with stderr() and 
  stdout() captured?  seems weird.
* implement capture handling at condition handler level and integrate output 
  and condition streams so taht we can do the replay? right now conditions come
  out of order.
* should we capture actual stdout() vs stdout caused by a visible
  value separately?  probably yes, makes way more sense for the
  show.testoritem method 
* need a better mechanism for handling the options changes for error and 
  warning?  do we really want to change the error options? or allow warning=2? 
* can we get away with textconnection captures for the files to avoid creating/
  \ recreating files every time?  right now the simplicity of creating the files 
  in the innermost part of the loop is great, but might be causing overhead (
  weight against notorious slowness of text connections)
* debugging partially implemented by disabling captures, but really,
  that's not real debugging.  unfortunately, because we capture std.err
  and we can't tee that, there is no good debugging to implement.
*  should we capture all options in case user changes some that we don't expect
   and don't know what to do about?

## Browse

* performance problems from too many nested environments? decided 
  to collapse non-tests into tests to limit  of environments, not clear
  whether this is actually an issue
* should we keep objects that user creates while browsing across tests?  
  probably, but we don't right now.
* accept all (hidden?) option
* "aborted mismatch" confusing, it means in reference to one test caused an
  execution abortion, whereas the other one didn't, but no one will have any
  idea what that means (e.g. there was a stop in one, but not in the other)
* Current behavior of automatically storing new non-tests and discarding 
  reference non tests can be potentially confusing to someone examining the
  environment and not realizing the object they are looking at is not the same
  that was used in the evaluation of the reference test; need to think about
  now to handle this (throw warning whenever such objects are accessed?)
* lack of traceback when browsing an error is a bit of a problem; is there
  a way to provide one?

## Order of Tests

* what do we do with default section that has values scattered throughout?
* And how to make it clear that tests are ordered first by test outcome, and as
  a result a display order will not be the same as the order in the file?
  Document clearly?
* removed tests coming last?  Trying to match them back to a section is too
  onerous, and besides, likely wouldn't have them in the same spot anyway.

## Environments

* Should run in an empty environment by default, with option to run with 
  globalenv as parent?  Had some confusing issues crop up as a result of a 
  variable getting picked up from globalenv

## Internal

* There is heavy usage of `parent.env<-`, what do we do about the note in R docs
  stating this might be removed in the future?
* need to think through intialize methods so hopefully we can avoid instantiating
  with .items argument for testorlist inheritors since that's a bit weird 
* generally, need to add more methods so we're not messing with slots
  directly
* why are testor classes available even though they are not exported????
  do they have to be in order to support reading the stored objects?
  related: why is it that it seems that the generics are exported
  after documentation, but any subsequent re-installs apperas to blow them
  away?
* not sure that condition comparison function should automatically loop
  through lists. this complicates the function substantially, though it
  does have the advantage of allowing the user to handle the cases were
  condition lists have different number of entries; maybe there is an option
  to provide a list version vs. normal version? and if there is a mismatch
  in condition lengths, then actively tell the user they can use the
  list version?
* need to check whether there are non-standard re-starts configured?
  or do we just not support that? probably don't support it.
* when assigning comments, should change comment resetting so we don't
  need to go through the entire parse tree every time.
* short comments on short expressions should be appended to end of
  expression rather than to start?

## Misc

* should run faster
* provide option to only run new tests?  Makes incremental work on a large
  file easier so not everything has to be re-run
* provide feedback on what testor is doing while it is running tests (i.e.
  fancy progress bar)
* backup of the testor .rds file should be created
* provide facilities to upate testor id when testors are moved, though
  typically the testor should always be in the same relative location
  to the script that runs it.

## Extreme Programing

* add an extreme programing mode that requires new tests to evaluate to TRUE
  or NULL or some such

---

# Scenarios to test

* Very large objects produced by tests
* test `description` for testor_section
* what do we do about capture.tests.R
* Need to add non-interactive tests

---

# Bugs

* Unreproduced: when running `match_call` tests, after accepting 2 changes and 
  keeping one reference test, re-running caused testor to not recognized any of 
  the tests as having been run
* make sure ls doesn't get defined in global env!! right now it seems to be,
  every now and then (this may no longer be a problem).
* Line wrapping doesn't seem to be well implemented; headers end well, but
  text output seems to overflow by a word or so.
* history restoration failed when using browser() in browse() and quitting from
  both