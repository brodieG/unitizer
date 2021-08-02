source(file.path("_helper", "init.R"))

options(unitizer.color = FALSE, width = 80L)

# - "Random Seed" --------------------------------------------------------------

old.seed <- if (!exists(".Random.seed")) NULL else .Random.seed
seed.dat <- getOption("unitizer.seed")

suppressWarnings(
  untz.glob <-
    unitizer:::unitizerGlobal$new(enable.which = setNames(2L, "random.seed"))
)
do.call(set.seed, seed.dat)
new.seed <- .Random.seed
state <- untz.glob$state()
invisible(runif(10))    # see if we can reset state after this
untz.glob$reset(state)
identical(.Random.seed, new.seed)
untz.glob$resetFull()

if (is.null(old.seed)) {
  !isTRUE(exists(".Random.seed"))
} else identical(old.seed, .Random.seed)

# - "State Show" ---------------------------------------------------------------

show(unitizer:::unitizerStatePristine())

# - "all.equal.unitizerDummy" --------------------------------------------------

dummy <- new("unitizerDummy")
blah <- "hello"
ref.txt <- "`.REF` value was not recorded, but `.NEW` value was; they are likely different"
identical(all.equal(dummy, blah), ref.txt)
all.equal(dummy, dummy)
identical(
  all.equal(blah, dummy),
  "`.NEW` value was not recorded, but `.REF` value was; they are likely different"
)
# testing S4 / S3 methods, first works, second doesn't since we can't
# have an S3 generic with dispatch on 2nd arg

identical(
  evalq(all.equal(new("unitizerDummy"), "hello"), getNamespace("stats")),
  ref.txt
)
evalq(all.equal("hello", new("unitizerDummy")), getNamespace("stats"))

# - "All Equal States" ---------------------------------------------------------

# Doesn't seem like we're comparing these to anything?  Maybe should look into
# doing so?

state.A <- new("unitizerGlobalState", search.path = letters[1:3],
    options = list(a = 5:7, b = new("unitizerDummy"), c = "hello"),
    working.directory = "a/b/c")
state.B <- new("unitizerGlobalState", search.path = letters[1:3],
    options = list(a = 5:7, b = new("unitizerDummy"), d = "goodbye",
        c = new("unitizerDummy")), working.directory = new("unitizerDummy"),
    random.seed = 1:3)
state.C <- new("unitizerGlobalState", search.path = letters,
    options = list(a = list(5, 6, 7), c = LETTERS), working.directory = new("unitizerDummy"),
    random.seed = 1:3)
# just compare to A
state.D <- new("unitizerGlobalState", search.path = letters[1:3],
    options = list(a = list(1, 2, 3), b = new("unitizerDummy"),
        c = "hello"), working.directory = "a/b/c")
state.E <- new("unitizerGlobalState", options = setNames(as.list(1:20),
    head(letters, 20)))
state.F <- new("unitizerGlobalState", options = setNames(as.list(1:20),
    tail(letters, 20)))
# This one is supposed to return something non-character or TRUE when used
# with the provided all.equal
state.G <- new("unitizerGlobalState", options = list(a = structure(TRUE,
    class = "unitizer_glob_state_test"), b = 0))
state.H <- new("unitizerGlobalState", options = list(a = structure(FALSE,
    class = "unitizer_glob_state_test"), b = 2))

# - "as.state" -----------------------------------------------------------------

identical(
  unitizer:::as.state("recommended"),
  unitizer:::as.state(unitizer:::unitizerStateSuggested())
)
identical(
  unitizer:::as.state("suggested"),
  unitizer:::as.state(unitizer:::unitizerStateSuggested())
)
identical(
  unitizer:::as.state("pristine"),
  unitizer:::as.state(unitizer:::unitizerStatePristine())
)

# unitizerStateProcessed should produce the default object (which currently
# is "off")

all.equal(
  unitizer:::as.state(.GlobalEnv),
  unitizer:::as.state(unitizer:::unitizerStateSuggested(par.env = .GlobalEnv))
)
all.equal(
  unitizer:::as.state(in_pkg("stats")),
  unitizer:::as.state(
    unitizer:::unitizerStateSuggested(par.env = getNamespace("stats"))
) )

stats.lib <- file.path(system.file(package = "stats"), "R")
all.equal(
  unitizer:::as.state(in_pkg(), test.files = stats.lib),
  unitizer:::as.state(
    unitizer:::unitizerStateSuggested(par.env = getNamespace("stats"))
) )
try(unitizer:::as.state(200))
state <- unitizer:::unitizerStateOff()
# bypass validity method
state@options <- 2L
try(validObject(state))
# state raw conversions
identical(
  unitizer:::as.state(unitizer:::unitizerStateRaw()),
  unitizer:::unitizerStateProcessed()
)
identical(
  unitizer:::as.state(unitizer:::unitizerStateRaw(par.env = "stats")),
  unitizer:::unitizerStateProcessed(par.env = getNamespace("stats"))
)
state@options <- 0L
state.proc <- unitizer:::as.unitizerStateProcessed(state)
state.raw <- unitizer:::as.unitizerStateRaw(state.proc)
is(state.raw, "unitizerStateRaw")
all.equal(
  lapply(slotNames(state), slot, object = state.proc),
  lapply(slotNames(state.raw), slot, object = state.raw)
)
try(unitizer:::as.state(unitizer:::unitizerStateRaw(par.env = in_pkg())))

identical(
  unitizer:::as.state(unitizer:::unitizerStateRaw(par.env = in_pkg("stats"))),
  unitizer:::unitizerStateProcessed(par.env = getNamespace("stats"))
)
try(
  unitizer:::as.state(
    unitizer:::unitizerStateRaw(par.env = in_pkg("asdfalkdfasd"))
) )
try(
  unitizer:::as.state(
    unitizer:::unitizerStateRaw(par.env = in_pkg("")), test.files = getwd()
) )
# impossible states
state.obj <- unitizer:::unitizerStateRaw()
state.obj@options <- 2L
try(unitizer:::as.state(state.obj))
state.obj@namespaces <- 2L
state.obj@search.path <- 1L
try(unitizer:::as.state(state.obj))

# - "as.state_raw" -------------------------------------------------------------

old.opt.loc <- options(unitizer.state = .GlobalEnv)
try(unitizer:::as.state_raw(.GlobalEnv))
options(unitizer.state = 42L)
try(unitizer:::as.state_raw(.GlobalEnv))
state.raw <- unitizer:::as.unitizerStateRaw(unitizer:::unitizerStateOff())
state.proc <- unitizer:::as.unitizerStateProcessed(state.raw)
my.env <- new.env()
options(unitizer.state = state.raw)
state.raw@par.env <- my.env
all.equal(unitizer:::as.state_raw(my.env), state.raw)
options(unitizer.state = state.proc)
my.env <- new.env()
state.proc@par.env <- my.env
all.equal(
  unitizer:::as.state_raw(my.env),
  unitizer:::as.unitizerStateRaw(state.proc)
)
options(old.opt.loc)

# - "state" --------------------------------------------------------------------

# all these assume we set the options to be in recommended mode

all.equal(state("stats"), unitizer:::unitizerStateSuggested(par.env = "stats"))

all.equal(
  state(in_pkg("stats")),
  unitizer:::unitizerStateSuggested(par.env = in_pkg("stats"))
)
all.equal(
  state(in_pkg()), unitizer:::unitizerStateSuggested(par.env = in_pkg())
)
all.equal(
  state(search.path = 1), unitizer:::unitizerStateSuggested(search.path = 1L)
)
s1 <- unitizer:::unitizerStateSuggested(par.env = .GlobalEnv)
for (i in setdiff(slotNames(s1), "par.env")) slot(s1, i) <- 0L
s2 <- unitizer:::unitizerStateOff()
all.equal(s1, s2)
# invalid state
try(state(search.path = 3))
try(state(options = 2, namespaces = 1))
try(state(namespaces = 2, search.path = 1))
state.inv <- unitizer:::unitizerStateProcessed()
state.inv@options <- 2L
try(unitizer:::as.state(state.inv))
state.inv@namespaces <- 2L
try(unitizer:::as.state(state.inv))
# captured <in: >
any(grepl("<in: .*>", capture.output(show(state(in_pkg())))))
any(grepl("<in: package:stats>", capture.output(show(state(in_pkg("stats"))))))
any(grepl("namespace:stats", capture.output(show(state(asNamespace("stats"))))))

# - "in_pkg" -------------------------------------------------------------------

try(in_pkg(""))
identical(as.character(in_pkg()), "<in: auto-detect-pkg>")
identical(as.character(in_pkg("stats")), "<in: package:stats>")
identical(capture.output(show(in_pkg())), "<in: auto-detect-pkg>")
try(unitizer:::in_pkg_to_env(in_pkg(), "/"))

# - "merge states" -------------------------------------------------------------

trk.new <- new("unitizerGlobalTrackingStore", search.path = list(1,
    2, 3), options = list("a", "b"))
trk.ref <- new("unitizerGlobalTrackingStore", search.path = list(4,
    5, 6), options = list("c", "d"))
items <- new("unitizerItems")
items <- items + new("unitizerItem", call = quote(1 + 1), glob.indices = new("unitizerGlobalIndices",
    search.path = 1L, options = 2L))
items <- items + new("unitizerItem", call = quote(2 + 1), glob.indices = new("unitizerGlobalIndices",
    search.path = 2L, options = 1L))
items <- items + new("unitizerItem", call = quote(1 * 1), reference = TRUE,
    glob.indices = new("unitizerGlobalIndices", search.path = 1L,
        options = 1L))
items <- items + new("unitizerItem", call = quote(2 * 1), reference = TRUE,
    glob.indices = new("unitizerGlobalIndices", search.path = 3L,
        options = 2L))
res <- unitizer:::mergeStates(items, trk.new, trk.ref)

sapply(res$items, function(x) as.integer(slot(x, "glob.indices")))
s.n.to.check <- c("search.path", "options", "working.directory",
    "random.seed", "namespaces")
sapply(s.n.to.check, slot, object = res$states)
# No reference items
items.no.ref <- items[1:2]
identical(
  unitizer:::mergeStates(items.no.ref, trk.new, trk.ref), 
  list(items = items.no.ref, states = trk.new)
)
# No new items; note that we only remap the used states to the new state
# which is why we need all the .mod objects

items.no.new <- items[3:4]
items.no.new.mod <- items.no.new
items.no.new.mod[[2L]]@glob.indices@search.path <- 2L
trk.ref.mod <- trk.ref
trk.ref.mod@search.path[[2L]] <- NULL

identical(
  unitizer:::mergeStates(
    items.no.new, new("unitizerGlobalTrackingStore"),trk.ref
  ),
  list(items = items.no.new.mod, states = trk.ref.mod)
)

