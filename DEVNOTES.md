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
* Current behavior of automatically storing new non-tests and discarding
  reference non tests can be potentially confusing to someone examining the
  environment and not realizing the object they are looking at is not the same
  that was used in the evaluation of the reference test; need to think about
  now to handle this (throw warning whenever such objects are accessed?)
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

## Side Effects

Rationalizing side effects.

Major types:

* search path changes (library / attach / etc)
* options
* working directory changes

Types that are treated differently

* history
* reference objects             # can't do anything yet (hashing in future?)
* evaluation environment        # is this changeable?  Not really
* search path keep?

Simplified implementation:

* special treatment
* no special treatment

Different meaning for each of the types of things.

---

Old possibilities:

General modes:

* Vanilla: do nothing
* before preloads
* after preloads
* after each

Actions

* nothing
* reset
* warn / nothing
* warn / reset
* stop


        lib    opt     wd     hist
b4 pre    x      -      -        -
af pre    -      -      -        x
af tst    x      x      x        x

Each needs to be controlled separately, so main question is do we do it as an
argument to `unitize_core`, or do we do it as global options?

Note we gave up on the above; either enable or disable feature is only option,
and if finer grain control needed then do it through options.  `par.env` is
a bit different because state is not tracked.

### Philosophy

Offer two options for each setting:

* 0: Nothing (don't track / reset, etc.)
* 1: Track (includes resetting between unitizers, but accepting initial state as is)
* 2: Zero-set

There is some interplay between search path and options, particularly because
many options are often set up when a path is first loaded.  Perhaps this means
that you can only have zero-set options if you also have zero set search path.

Additionally this means that the search path trimming should happen with
unloading namespaces as well so that when they are re-attached onload scripts
get run again (and define options as they often do)?

### Seed Tracking

The Mersenne Seed is way too large, need to use Marsaglia Multi Carry or some
such.

### Options Tracking

It is viable to store all the non-function options in each unitizer (2-3KB
expected per unitizer).  But there are a few issues:

* what to do before any tests are run (i.e. with options that come from global)
* after each test is run (undo option changes)?

Details

* several options are system specific and could potentially cause problems when
  running tests on different machines
* we have to drop function type options
  * typically non impacting to tests, and fairly large (e.g. `editor`)
  * what about options(error=XXX)?, which is often a function
  * loosely related, `options(error=NULL)` is technically not set
* maybe we record

Possibilities

* options changed by tests are reset
* options changed by user / system are not reset, but if any tests fails then
  we alert to them
  * need mechanism to review options
  * complicated because each unitizer in a directory could have been run with
    different set of options
  * additionally even if we log initial set of reference options, we are not
    tracking the changes that happened (at least not without much more logging)
    so if a user inspects the option delta for a particular test they may not
    get the actual reference options as they were for that test
  * one option would be to report that there was a difference in options when
    the the tests began to be evaluated (if there is an error)
* Alternate approach is to track a well know subset of options, and ignore all
  others.  Or perhaps better, keep a list of well known options that should
  not be tracked...  Really we want to know all system specific options.

Should we make a distinction between options being changed by user vs. by
tests?  The easiest thing to do would be that.

And how do options like working directory fit in all this?  Do we want to change
it to a directory that may not exist?  Should it be test-writer responsibility
to make sure they don't have any relative path references?

Some of these global settings are almost certainly going to be different even
when comparing the creation run to the R CMD check run.  I guess we could force
them to be the same, but what does that even mean for stuff like working
directory?

So again, two broad categories:

* state that is the same for each test file
  * working directory
* state that is the same across unitizer runs
  * empty parent env (sort off)
  * search path (more or less, can get messed up by search.path.keep)
  * options?
  * random.seed

Go the whole hog, track all options and store them for reference tests, need a
lighter weight mechanism for storing the options.  Store only deltas and
recompute them as needed?  Would need getOldState functions that would just pull
the requested entry for most states, but for options would recompute them from
early data.  The main issue remains that there options are that need to be
different when run in different environments, such as:

* pager
* pdfviewer
* device
* width!

etc.  Can we realistically have a list of options not to change?  If we get it
wrong bad things would happen.  What if packages introduce options that are
system specific?  Then they should get loaded on package attach, and will be
different



## Interface

```
unitize <- function(
  test.file, store.id=NULL,
  interactive.mode=interactive(),
  par.env=getOption("unitizer.par.env"),
  reproducible.global.settings=getOption("unitizer.global.settings"),
  force.update=FALSE,
  auto.accept=character(0L),
  pre.load=NULL
)
```
Interface is too complex, need to simplify.

Advanced Features:
* force.update
* auto.accept

Potential for option based:
* history
* interactive.mode

Baseline
* test.file
* store.id

* par.env

* reproducible.state??
* pre??
* post??
* history??

* force.update??
* auto.accept??
* interactive.mode??

Might be fine to keep all features upfront, provided the less useful ones are
deprioritized in order of appearance.

Should history be controlled independent of track.state?  Probably.

Should par.env be controlled independent of track.state?  Probably too, but a
bit more complicated because of intimate relationship between par.env and
search path tracking.  Does the relationship actually need to be that intimate?
One of the problems is that a lot of the enabling / disabling infrastructure is
co-mingled, but maybe it doesn't need to be?


# Scenarios to test

* Very large objects produced by tests
* test `description` for unitizer_section
* what do we do about capture.tests.R
* Need to add non-interactive tests
