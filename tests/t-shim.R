source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))
source(file.path("aammrtf", "mock.R"))

old.state <- tracingState(TRUE)

# - "trace_at_end" -------------------------------------------------------------

if (is(unitizer:::trace_test_fun, "functionWithTrace"))
  untrace("trace_test_fun", where = asNamespace("unitizer"))
unitizer:::trace_at_end("trace_test_fun", quote(if (!inherits(.res,
    "try-error")) cat(sprintf("x: %d\n", .res$value))), print = FALSE,
    where = asNamespace("unitizer"))
coi(unitizer:::trace_test_fun())
tracingState(FALSE)
identical(capture.output(unitizer:::trace_test_fun()), character())
tracingState(TRUE)

err <- try(unitizer:::trace_test_fun(stop("hello")), silent = TRUE)
cond <- attr(err, "condition")
conditionMessage(cond)
conditionCall(cond)
# return/missing etc. corner cases
f <- function(x, y, z = 5) {
    if (missing(x)) {
        return(TRUE)
    }
    else if (z > 5) {
        stop("OMG, z > 5")
    }
    else if (identical(substitute(y), "hey")) {
        "substitute!"
    }
    else FALSE
}
unitizer:::trace_at_end("f", quote(cat("hello\n")), FALSE, environment())
res <- f()
res
res2 <- f(1)
res2  # FALSE
err <- try(f(1, z = 6), silent = TRUE)
is(err, "try-error")
attr(err, "condition")
res3 <- f(1, y = "hey")
res3

# - "Parent Env Stays on Top" --------------------------------------------------

try(detach("package:unitizerdummypkg1", unload = TRUE), silent = TRUE)
while ("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))
unitizer.dummy.list <- list(z = 23, x = 1, y = "hello")
my.env <- new.env()
state.set <- c(search.path = 2L)
# make sure to unset this at end
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env,
    enable.which = state.set, set.global = TRUE)
untz.glob$shimFuns()
sp <- search()
curr2 <- sp[[2L]]


identical(environmentName(parent.env(my.env)), curr2)
library("unitizerdummypkg1", lib.loc = TMP.LIB)
identical(environmentName(parent.env(my.env)), "package:unitizerdummypkg1")
attach(unitizer.dummy.list)
identical(environmentName(parent.env(my.env)), "unitizer.dummy.list")
detach("unitizer.dummy.list")
identical(environmentName(parent.env(my.env)), "package:unitizerdummypkg1")
detach("package:unitizerdummypkg1", unload = TRUE)
identical(environmentName(parent.env(my.env)), curr2)
untz.glob$checkShims()

# - "Parent env tracking with search path manip" -------------------------------

untz.glob$state()
keep.more <- c(getOption("unitizer.search.path.keep.base"))
unitizer:::search_path_trim(keep.more, global = untz.glob)
untz.glob$state()
identical(environmentName(parent.env(my.env)), search()[[2L]])
untz.glob$resetFull()
identical(environmentName(parent.env(my.env)), curr2)

# - "Disable Unshims, etc." ----------------------------------------------------

untz.glob$unshimFuns()
!any(vapply(list(library, detach, attach), inherits, logical(1L),
    "functionWithTrace"))
untz.glob$release()

# - "Checks, errors, etc." -----------------------------------------------------

# make sure to unset this at end
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env,
    enable.which = state.set, set.global = TRUE)
tracingState(FALSE)
untz.glob$shimFuns() # warning
parent.env(my.env)
tracingState(TRUE)
untz.glob$release()
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env,
    set.global = TRUE)
trace("library", quote(cat("I am traced\n")), where = .BaseNamespaceEnv)
lib.trace <- library
untz.glob$shimFuns()  # warning
parent.env(my.env)
inherits(attach, "functionWithTrace")  # FALSE
inherits(detach, "functionWithTrace")  # FALSE
inherits(library, "functionWithTrace")
identical(lib.trace, library)
untrace("library", where = .BaseNamespaceEnv)
untz.glob$release()
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env,
    set.global = TRUE)
untz.glob$shimFuns()
trace("attach", quote(cat("I am traced\n")), where = .BaseNamespaceEnv)
attach.trace <- attach
untz.glob$checkShims()   # warning
parent.env(my.env)
inherits(detach, "functionWithTrace")    # FALSE
inherits(library, "functionWithTrace")   # FALSE
inherits(attach, "functionWithTrace")
identical(attach.trace, attach)
untrace("attach", where = .BaseNamespaceEnv)
untz.glob$release()
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env,
    set.global = TRUE)
untz.glob$shimFuns()
tracingState(FALSE)
untz.glob$checkShims()   # warning
parent.env(my.env)
tracingState(TRUE)
inherits(detach, "functionWithTrace")    # FALSE
inherits(library, "functionWithTrace")   # FALSE
inherits(attach, "functionWithTrace")    # FALSE
# try tracing some stuff that shouldn't be
untz.glob$shimFuns("baljevzxhjLsdc")     # Warning
# test unexpected message or behavior from `trace_at_end`
try(untz.glob$shimFun("sum"))

mock(unitizer:::trace_at_end, quote(stop("trace_at_end fail")))
any(
  grepl(
    "trace_at_end fail",
    capture.output(
      trace.fail <- untz.glob$shimFun("library"), type = "message"
    ),
    fixed = TRUE
  )
)
unmock(unitizer:::trace_at_end)

trace.fail   # FALSE
mock(unitizer:::trace_at_end, quote(message("random message")))
untz.glob$shimFun("library")
unmock(unitizer:::trace_at_end)

mock(unitizer:::trace_at_end, quote(TRUE))
dont.trace <- untz.glob$shimFun("library") # Warning "not traced"
unmock(unitizer:::trace_at_end)

dont.trace        # FALSE
untz.glob$release()
# untrace condition
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, set.global = TRUE)
untz.glob$shimFuns()

mock(
  unitizer:::untrace_utz,
  quote({
    message("untrace dummy")
    base::untrace(what = what, signature = signature, where = where)
  })
)
untz.glob$unshimFuns()  # message untrace dummy
unmock(unitizer:::untrace_utz)
untz.glob$release()

try(detach("package:unitizerdummypkg1", unload = TRUE), silent = TRUE)

while ("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))

# - "find_returns" -------------------------------------------------------------

fun <- function() {
    if (TRUE)
        return(1)
    else {
        {
            2 + 2
            identity(c(1, 2, return(3), {
                list(1, 2, 5)
                return(return(4))
            }))
            return(5)
        }
        return(6)
    }
    if (TRUE)
        return(7)
    else return(8)
    return(9)
    return(10)
}
ret.loc <- unitizer:::find_returns(fun)
ret.loc

# Validate visually that this worked

all(vapply(unitizer:::get_returns(fun, ret.loc), function(x) x[[1L]] ==
    quote(return), logical(1L)))

