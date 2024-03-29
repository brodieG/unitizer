# Known Bugs

See git repo **[issues](https://github.com/brodieG/unitizer/issues)**.

# Internal Notes

This section contains developer notes for things that are unresolved, need
fixing, or hare-brained ideas for features.  Read at your own risk.

## v1.4.17.9000

### Output Capture

Need to figure out if we can if we can disable capture without then messing up
the display of the test.  Might not be possible?  This is indeed a problem as we
loose the message data stream which we do display once we get to the failed
test.  The conditions should still be there.

Need to try a test that does this.

Also need to figure out how to get rid of `"<unitizer sink test>"`.

We need some option to set the progress mode:

* progress detail level
* overwrite or newline
* show evaluation transcript

The last two could default to TRUE for interactive.  Should probably be S4.

```
unitizer_progress <- function(level=TRUE, transcript=!interactive()) {
  structure(c(level=level, transcript=transcript), class='utz_transcript')
}
```

One weirdness is that "transcript" and "progress" feel somewhat separate, but
the trick is the overwrite vs newline.  Maybe there is a transcript mode
modifier.  This makes most sense.  Maybe in the future we provide more control
at the transcript level.

### Multiple Bookmarks

Challenge is that we eval all the unitizers, and then browse all of them, but if
we just want to re-eval one we don't have a good way to interrupt?

So what behavior do we want?  Ideally when we exit with R, we want to return to
that exact unitizer and continue on from there in the original order.

Why do we allow only one bookmark?  The RR option effectively requires multiple
ones.  Actually, maybe it's okay if we only care about returning to the specific
point of a single re-eval, which is likely what we want.  Does RR even make
sense outside of the top level "select-a-unitizer" level?

Currently the bookmarked status would allow us to pick which unitizers to review
after reloading (browse.which).  The problem is that a bookmark is complicated
(see `unitizerBookmarkOrNULL`), and it's presence allow is used to determine
whether a unitizer is bookmarked, but we would need to add a semantic to the
bookmark that's just "start from the beginning" if we add bookmarks to all
subsequent that are not explicitly bookmarked.  I guess we could do this,
although if feels hacky.

### Deparse Issues

Astonishing we got this far without running into it more frequently, but the
matching of calls on deparse suffers from several issues, particularly
instability with different locales, etc, but could also be affected by numerical
precision.

Storing the parsed data is out of the question due to file size (unitizer.R is a
~750 line file):

```
> x <- parse('../unitizer/R/unitizer.R')
> f.lang <- tempfile()
> f.char <- tempfile()
> saveRDS(x, f.lang)
> saveRDS(deparse(x), f.char)
> file.size(f.lang)
[1] 76612
> file.size(f.char)
[1] 5431
```

Parse/deparse cycle on load is technically feasible from a computing cost
perspective, but it is not guaranteed to work as some locales will accept random
bytes as valid (because they are) so they are parsed directly into those bytes,
but then attempting to parse in another locale fails.

We could try to record the locale and iconv, or even try to use RDS3 which
appears to translate, but really we probably will just document for now.  We
could add a warning too?

## v1.4.15.9000

### Windows dir issues

Works on winbuilder.  Most likely explanation is that the CRAN machines when
given a random file name in `normalizePath(blah, mustWork=FALSE)` produce a full
normalized path for `blah` irrespective of whether `blah` exists or not.

So fix here is to change normalize_path to test for file, and if not, return the
unchanged file (and hope for the best?).  Indeed we confirm that non-existent
files on windows are still normalized to the working directory.

Additional complexity that on windows (sometimes?) we can expect drive letters,
so it's not always clear what is an absolute path.

### Old R Versions

We got it working on 3.5, but there are still failures on 3.3.  Should go back
and see if we can figure out what's going on.

Also, do we want to backport the eq-assign fix (probably not)?

## Color Options

Right now a total mess; need a unified way of addressing color usage in:

* `diffobj` calls from within unitizer
* `comment` coloring
* `browse` menu coloring (failed/removed/etc)
* anything else we end up adding

## Display Improvements

### Pager

Use pager?  Seems like almost definitely want to do this for elements like
[B]rowse, etc.  Have to think about how it impacts flow if the pager is external
like in RStudio.

### Colorizing Output

All meta-text should be grey...  But what about large portions of text such as
those we get in [B]rowse?  Maybe the headers and all that in gray, but the
contents in white?  What about the prompts?  Will have to play around.

Color in code comments?  Could do so pretty easily

One of the key objectives is differentiating `unitizer` meta output from normal
code output.  Will need to provide a solution for terminals without color
support, perhaps something like prepending `>>` to every meta line.

### Simple Value Failures

Need some way of mitigating the amount of output put out by `unitizer`.  Right
now with the addition of the new diff it's pretty jarring to see so much red and
yellow and blue.  Some options:

* If only a `value` error, squash `all.equal` and rely instead on `diffobj`
  output only; one issue here is that we might conceal some errors that
  `all.equal` would otherwise show; partly relies on making sure `diffobj` shows
  the `all.equal` output when there are no visible differences.
* Another big issue with quashing the `value` error is that we need to handle
  those cases where the user provides their own comparison function with its own
  output preferences.  The auto-output of `diffObj` might conflict with this.
* The whole `diff_state` business; what's the best way to handle it?

### Value and Condition



## Return values

Still somewhat conflicted about what the return values should be for a unitizer.

Need to be able to address:

1. what happens at unitizerObjectList level
2. at the unitizer level
3. and below?

At a high level we would want, for each unitizerObjectList, how many tests
passed / failed / added / removed etc, for each unitizer.  This is comparable
to what we show during browsing.  This info is generated by
summmary.unitizerObjectList in `unitizer_browse`, with the post eval but
pre browse unitizers.

Additionally, we need the user selections on what happens to each test.  We
need summaries of that, which we can get from the `@change` slot of the post-
browse unitizer, but we also want the full listing as it is shown when pressing
`B` at the `unitizer` prompt.  This information is fully contained in the
`untizerBrowse` object and displayed by `show.unitizerBrowse`.

So we should be able to modify `unitizer_browse` to return the additional
info (`show(y)` from outside `review_prompt` works fine).  The main issue seems
to be making sure there is a reasonable interface to access all this data.

So, to summarize, we should return a `unitizerObjectList` that will contain the
post eval but pre-browse `unitizers`, combined in some way with the post-browse
`unitizerBrowse` objects, with mechanisms for extracting the data in easy to
manipulate structures (e.g. matrices).

Right now we're just returning the data.frame.

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

### Legacy questions

* Should we really allow execution in interactive mode with stderr() and
  stdout() captured?  seems weird.
* implement capture handling at condition handler level and integrate output
  and condition streams so that we can do the replay? right now conditions come
  out of order when displaying test results
* should we capture actual stdout() vs stdout caused by a visible
  value separately?  probably yes, makes way more sense for the
  show.unitizeritem method
* debugging partially implemented by disabling captures, but really,
  that's not real debugging.  unfortunately, because we capture std.err
  and we can't tee that, there is no good debugging to implement.

### Stdout and Message Capture

Turns out stdout and stderr capture is a very small portion of the storage
space taken up in a unitizer.

### Storage optimization

Looks like the majority of storage is actually taken by values, calls, and
environments.  We partially addressed values by no longer storing ignored
tests.  We probably can't do anything about the environments.

For calls, we're planning on storing the deparsed calls rather than the calls
themselves.  This should save a lot of room, at the cost of some computation
time since the deparse/parse cycle is about 150us per call, which is less than
ideal.

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

### Overview

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

### Default Settings

Unfortunately reproducible state can cause issues with packages that cannot
be unloaded (e.g. data.table) and also likely to be a little less robust since
we are doing a lot of borderline stuff to make it happen.  Questions then:

1. Should default mode be full on reproducible state?
2. What should be the interplay between reproducible state and the parent
   environment?
3. What about for translated testthat tests that are pretty much guaranteed not
   to have been written in reproducible mode?

Additionally, would be nice to be able to change just one value of reproducible
state instead of having to do the entire settings string.  Perhaps the best
interface is a combination of either a single string value for a preset, and
the full named integer vector.  For example:

* "max":   everything set to full reproducible
* "safe":  search.path and options off, random.seed and
* "basic": reproducibility only between tests, though a bit odd that safe has a
  higher level of reproducibility than for some settings
* "off":   everything off

An alternative is to create a new S4 class that represents all the state values,
and control the population through the constructor.  We can also extend that
class to provide other pre-set options, so we might have `stateSettings` and
`stateSettingsSafe` or some such.  The main problem with this implementation
is that it would require an S4 object being stored as an option, which is
undesirable.  Perhaps can accept either character value or S4 object?

Then we would have five state values, the traditional four plus par.env?

Is it worth the extra complexity for the argument in order to eliminate one
single argument?  Maybe, the best reason to do this is that then all state
related aspects of evaluation are controlled through the one argument, which
makes more sense.  In particular, this allows us to have the "safe" mode also
reasonably control par.env.

Shouldn't worry too much about 3. since translation is a secondary
consideration.  The reasonable thing would be to run in some lower level of
reproducibility, though annoyingly any time we re-run the tests we would have
to use those exact settings.  This is somewhat mitigated if we

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

### Search Path Tracking

Shimming library/attach/detach likely to be a major obstacle to CRAN approval,
and also introduces complexity and instability as new R versions are released.
Simplest solution is to just add library/attach/detach functions in a "shims"
environment, but not as robust as tracing the actual functions.  In particular,
things can fail if there is a compound expression that uses `base::library`,
either via package namespace or directly.  We can detect the direct calls via
parsing and issue warnings, but this is limited since people can get creative.

Note that even shimming `base::library`, etc, not fullproof since some packages
may be calling internals directly, or some base R functions may do that too?
(seems unlikely though).

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

### Options And Namespaces

There is an additional complication with options which is how to handle the
default options that get set by the `.onLoad` hooks.  In order for everything
to work well, we need to fully unload a namespace so that next time
`library` is called with the same package, the `.onLoad` hooks are kicked off.
Unfortunately, some packages (cough, data.table) cannot be unloaded (and more
generally R docs warn full unloads are not really well supported).

We can't just run `.onLoad` as in a normal `.onLoad` the package environment
is unlocked and `.onLoad` hook scripts expect to be able to modify bindings in
the namespace.

So, what is the most reasonable compromise for this?  If some namespaces are
not unloaded, then we have to make sure the options set by those namespaces are
not undone at any point.  So suppose we start with a blank slate, then:

* `library(data.table)`
* do a bunch of stuff
* go to reset state

We can prevent the unloading of the `data.table` namespace easily enough, but
we cannot undo any options without risking undoing `data.table` options.  In
fact given that the `data.table` namespace may have been loaded even before
we kick off `unitizer`, we can't even see what options were added by
`data.table`.

It seems that if one of the no-unload namespaces is loaded at any time then we
must set the options tracking to 0 if.  Actually, there are three types of
situations:

1. no-unload loaded before we even run unitizer
2. loaded in pre-loads
3. loaded only in pre-loads (i.e. not loaded before hand at all)
4. loaded in a unitizer file

For 1. and 2., we can run in modes 0 and 1.  For 3. we can run in 0:2, but since
we don't unload the namespace that only works the first time since the second
time we run the namespace will already be loaded...  Not super useful.  For
4. we can only run in mode 0.

Hmm, even for 2. running in mode 1 we might have a problem since we would still
be trying to undo the pre-loads, which would undo the options, but not the
no-unload namespaces and dump us back to the normal R prompt in that state.

Let's say we run into one of these namespaces in an incompatible mode; what do
we do?

1. Throw a warning and change modes to compatible mode?
2. Throw an error indicating what mode would be compatible?

Another possible work-around is to force a namespace load even before the
initial state recording and then run in mode 1 so that we don't undo that
namespace load.

Gah... and WTF do we do about all the potentially unloadable namespaces that
we can no longer unload because an unloadable namespace imports them?  This
greatly complicates the problem.

More and more it seems like we need to narrowly define the acceptable modes,
which seem to be 0, or 1 iff the namespace was already loaded by the time we
hit pre-loads.

So do we have an additional "unitizer.load.namespace.if.not.loaded" list?  And
does that make "unitizer.keep.namespace" redundant or do we still need both?
Probably still need both.  So need to check that namespace was already
pre-loaded against keep.namespace list.

## Demo

1. Better display of the file
2. Setup the different versions of the packages up front
3. Use `install(..., quiet=TRUE)
4. Check the resulting `unitizer` and make sure the results make sense (can we
   do this without crapping al)


## Other Issues

* Very large objects produced by tests
* test `description` for unitizer_section
* what do we do about capture.tests.R
* Need to add non-interactive tests
