library(unitizer)
library(testthat)
# context("Shim")
old.state <- tracingState(TRUE)
on.exit(tracingState(old.state))
# test_that("trace_at_end", {
#     if (is(unitizer:::trace_test_fun, "functionWithTrace")) 
#         untrace("trace_test_fun", where = asNamespace("unitizer"))
#     unitizer:::trace_at_end("trace_test_fun", quote(if (!inherits(.res, 
#         "try-error")) cat(sprintf("x: %d\n", .res$value))), print = FALSE, 
#         where = asNamespace("unitizer"))
#     expect_equal(capture.output(unitizer:::trace_test_fun()), 
#         "x: 2")
#     tracingState(FALSE)
#     expect_equal(capture.output(unitizer:::trace_test_fun()), 
#         character())
#     tracingState(TRUE)
#     err <- try(unitizer:::trace_test_fun(stop("hello")), silent = TRUE)
#     cond <- attr(err, "condition")
#     expect_equal(conditionMessage(cond), "hello")
#     expect_equal(conditionCall(cond), quote(unitizer:::trace_test_fun(stop("hello"))))
#     f <- function(x, y, z = 5) {
#         if (missing(x)) {
#             return(TRUE)
#         }
#         else if (z > 5) {
#             stop("OMG, z > 5")
#         }
#         else if (identical(substitute(y), "hey")) {
#             "substitute!"
#         }
#         else FALSE
#     }
#     unitizer:::trace_at_end("f", quote(cat("hello\n")), FALSE, 
#         environment())
#     expect_equal(capture.output(res <- f()), "hello")
#     expect_true(res)
#     expect_equal(capture.output(res2 <- f(1)), "hello")
#     expect_equal(res2, FALSE)
#     out <- capture.output(err <- try(f(1, z = 6), silent = TRUE))
#     expect_equal(out, "hello")
#     expect_is(err, "try-error")
#     expect_equal(attr(err, "condition"), structure(list(message = "OMG, z > 5", 
#         call = quote(f(1, z = 6))), class = c("simpleError", 
#         "error", "condition")))
#     expect_equal(capture.output(res3 <- f(1, y = "hey")), "hello")
#     expect_equal(res3, "substitute!")
# })
# - "trace_at_end" -------------------------------------------------------------


if (is(unitizer:::trace_test_fun, "functionWithTrace")) untrace("trace_test_fun", 
    where = asNamespace("unitizer"))
unitizer:::trace_at_end("trace_test_fun", quote(if (!inherits(.res, 
    "try-error")) cat(sprintf("x: %d\n", .res$value))), print = FALSE, 
    where = asNamespace("unitizer"))
# expect_equal(capture.output(unitizer:::trace_test_fun()), "x: 2")
capture.output(unitizer:::trace_test_fun())
tracingState(FALSE)
# expect_equal(capture.output(unitizer:::trace_test_fun()), character())
capture.output(unitizer:::trace_test_fun())
tracingState(TRUE)
err <- try(unitizer:::trace_test_fun(stop("hello")), silent = TRUE)
cond <- attr(err, "condition")
# expect_equal(conditionMessage(cond), "hello")
conditionMessage(cond)
# expect_equal(conditionCall(cond), quote(unitizer:::trace_test_fun(stop("hello"))))
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
# expect_equal(capture.output(res <- f()), "hello")
capture.output(res <- f())
# expect_true(res)
res
# expect_equal(capture.output(res2 <- f(1)), "hello")
capture.output(res2 <- f(1))
# expect_equal(res2, FALSE)
res2
out <- capture.output(err <- try(f(1, z = 6), silent = TRUE))
# expect_equal(out, "hello")
out
expect_is(err, "try-error")
# expect_equal(attr(err, "condition"), structure(list(message = "OMG, z > 5", 
#     call = quote(f(1, z = 6))), class = c("simpleError", "error", 
#     "condition")))
attr(err, "condition")
# expect_equal(capture.output(res3 <- f(1, y = "hey")), "hello")
capture.output(res3 <- f(1, y = "hey"))
# expect_equal(res3, "substitute!")
res3
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
# test_that("Parent Env Stays on Top", {
#     expect_identical(environmentName(parent.env(my.env)), curr2)
#     library("unitizerdummypkg1", lib.loc = tmp.lib)
#     expect_identical(environmentName(parent.env(my.env)), "package:unitizerdummypkg1")
#     attach(unitizer.dummy.list)
#     expect_identical(environmentName(parent.env(my.env)), "unitizer.dummy.list")
#     detach("unitizer.dummy.list")
#     expect_identical(environmentName(parent.env(my.env)), "package:unitizerdummypkg1")
#     detach("package:unitizerdummypkg1", unload = TRUE)
#     expect_identical(environmentName(parent.env(my.env)), curr2)
#     expect_true(untz.glob$checkShims())
# })
# - "Parent Env Stays on Top" --------------------------------------------------


# expect_identical(environmentName(parent.env(my.env)), curr2)
environmentName(parent.env(my.env))
library("unitizerdummypkg1", lib.loc = tmp.lib)
# expect_identical(environmentName(parent.env(my.env)), "package:unitizerdummypkg1")
environmentName(parent.env(my.env))
attach(unitizer.dummy.list)
# expect_identical(environmentName(parent.env(my.env)), "unitizer.dummy.list")
environmentName(parent.env(my.env))
detach("unitizer.dummy.list")
# expect_identical(environmentName(parent.env(my.env)), "package:unitizerdummypkg1")
environmentName(parent.env(my.env))
detach("package:unitizerdummypkg1", unload = TRUE)
# expect_identical(environmentName(parent.env(my.env)), curr2)
environmentName(parent.env(my.env))
# expect_true(untz.glob$checkShims())
untz.glob$checkShims()
# test_that("Parent env tracking with search path manip", {
#     untz.glob$state()
#     keep.more <- c("package:testthat", getOption("unitizer.search.path.keep.base"))
#     unitizer:::search_path_trim(keep.more, global = untz.glob)
#     untz.glob$state()
#     expect_identical(environmentName(parent.env(my.env)), search()[[2L]])
#     untz.glob$resetFull()
#     expect_identical(environmentName(parent.env(my.env)), curr2)
# })
# - "Parent env tracking with search path manip" -------------------------------


untz.glob$state()
keep.more <- c("package:testthat", getOption("unitizer.search.path.keep.base"))
unitizer:::search_path_trim(keep.more, global = untz.glob)
untz.glob$state()
# expect_identical(environmentName(parent.env(my.env)), search()[[2L]])
environmentName(parent.env(my.env))
untz.glob$resetFull()
# expect_identical(environmentName(parent.env(my.env)), curr2)
environmentName(parent.env(my.env))
# test_that("Disable Unshims, etc.", {
#     untz.glob$unshimFuns()
#     expect_true(!any(vapply(list(library, detach, attach), inherits, 
#         logical(1L), "functionWithTrace")))
# })
# - "Disable Unshims, etc." ----------------------------------------------------


untz.glob$unshimFuns()
# expect_true(!any(vapply(list(library, detach, attach), inherits, 
#     logical(1L), "functionWithTrace")))
!any(vapply(list(library, detach, attach), inherits, logical(1L), 
    "functionWithTrace"))
untz.glob$release()
# test_that("Checks, errors, etc.", {
#     untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
#         enable.which = state.set, set.global = TRUE)
#     tracingState(FALSE)
#     expect_warning(untz.glob$shimFuns(), "tracing state is FALSE")
#     expect_identical(parent.env(my.env), .GlobalEnv)
#     tracingState(TRUE)
#     untz.glob$release()
#     untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
#         set.global = TRUE)
#     trace("library", quote(cat("I am traced\n")), where = .BaseNamespaceEnv)
#     lib.trace <- library
#     expect_warning(untz.glob$shimFuns(), "already traced")
#     expect_identical(parent.env(my.env), .GlobalEnv)
#     expect_false(inherits(attach, "functionWithTrace"))
#     expect_false(inherits(detach, "functionWithTrace"))
#     expect_true(inherits(library, "functionWithTrace"))
#     expect_identical(lib.trace, library)
#     untrace("library", where = .BaseNamespaceEnv)
#     untz.glob$release()
#     untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
#         set.global = TRUE)
#     expect_true(untz.glob$shimFuns())
#     trace("attach", quote(cat("I am traced\n")), where = .BaseNamespaceEnv)
#     attach.trace <- attach
#     expect_warning(untz.glob$checkShims(), "functions unexpectedly changed")
#     expect_identical(parent.env(my.env), .GlobalEnv)
#     expect_false(inherits(detach, "functionWithTrace"))
#     expect_false(inherits(library, "functionWithTrace"))
#     expect_true(inherits(attach, "functionWithTrace"))
#     expect_identical(attach.trace, attach)
#     untrace("attach", where = .BaseNamespaceEnv)
#     untz.glob$release()
#     untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
#         set.global = TRUE)
#     expect_true(untz.glob$shimFuns())
#     tracingState(FALSE)
#     expect_warning(untz.glob$checkShims(), "Tracing state off")
#     expect_identical(parent.env(my.env), .GlobalEnv)
#     tracingState(TRUE)
#     expect_false(inherits(detach, "functionWithTrace"))
#     expect_false(inherits(library, "functionWithTrace"))
#     expect_false(inherits(attach, "functionWithTrace"))
#     expect_warning(untz.glob$shimFuns("baljevzxhjLsdc"), "some cannot be found")
#     expect_error(untz.glob$shimFun("sum"), "missing shim data")
#     with_mock(`unitizer:::trace_at_end` = function(...) stop("trace_at_end fail"), 
#         expect_true(any(grepl("trace_at_end fail", capture.output(trace.fail <- untz.glob$shimFun("library"), 
#             type = "message"), fixed = TRUE))))
#     expect_false(trace.fail)
#     with_mock(`unitizer:::trace_at_end` = function(...) message("random message"), 
#         expect_message(untz.glob$shimFun("library"), "random"))
#     with_mock(`unitizer:::trace_at_end` = function(...) TRUE, 
#         expect_warning(dont.trace <- untz.glob$shimFun("library"), 
#             "not traced"))
#     expect_false(dont.trace)
#     untz.glob$release()
#     untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
#         set.global = TRUE)
#     untz.glob$shimFuns()
#     with_mock(`unitizer:::untrace_utz` = function(what, signature = NULL, 
#         where = topenv(parent.frame())) {
#         message("untrace dummy")
#         base::untrace(what = what, signature = signature, where = where)
#     }, expect_message(untz.glob$unshimFuns(), "untrace dummy"))
#     untz.glob$release()
# })
# - "Checks, errors, etc." -----------------------------------------------------


# make sure to unset this at end
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
    enable.which = state.set, set.global = TRUE)
tracingState(FALSE)
# expect_warning(untz.glob$shimFuns(), "tracing state is FALSE")
untz.glob$shimFuns()
# expect_identical(parent.env(my.env), .GlobalEnv)
parent.env(my.env)
tracingState(TRUE)
untz.glob$release()
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
    set.global = TRUE)
trace("library", quote(cat("I am traced\n")), where = .BaseNamespaceEnv)
lib.trace <- library
# expect_warning(untz.glob$shimFuns(), "already traced")
untz.glob$shimFuns()
# expect_identical(parent.env(my.env), .GlobalEnv)
parent.env(my.env)
# expect_false(inherits(attach, "functionWithTrace"))
inherits(attach, "functionWithTrace")
# expect_false(inherits(detach, "functionWithTrace"))
inherits(detach, "functionWithTrace")
# expect_true(inherits(library, "functionWithTrace"))
inherits(library, "functionWithTrace")
# expect_identical(lib.trace, library)
lib.trace
untrace("library", where = .BaseNamespaceEnv)
untz.glob$release()
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
    set.global = TRUE)
# expect_true(untz.glob$shimFuns())
untz.glob$shimFuns()
trace("attach", quote(cat("I am traced\n")), where = .BaseNamespaceEnv)
attach.trace <- attach
# expect_warning(untz.glob$checkShims(), "functions unexpectedly changed")
untz.glob$checkShims()
# expect_identical(parent.env(my.env), .GlobalEnv)
parent.env(my.env)
# expect_false(inherits(detach, "functionWithTrace"))
inherits(detach, "functionWithTrace")
# expect_false(inherits(library, "functionWithTrace"))
inherits(library, "functionWithTrace")
# expect_true(inherits(attach, "functionWithTrace"))
inherits(attach, "functionWithTrace")
# expect_identical(attach.trace, attach)
attach.trace
untrace("attach", where = .BaseNamespaceEnv)
untz.glob$release()
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
    set.global = TRUE)
# expect_true(untz.glob$shimFuns())
untz.glob$shimFuns()
tracingState(FALSE)
# expect_warning(untz.glob$checkShims(), "Tracing state off")
untz.glob$checkShims()
# expect_identical(parent.env(my.env), .GlobalEnv)
parent.env(my.env)
tracingState(TRUE)
# expect_false(inherits(detach, "functionWithTrace"))
inherits(detach, "functionWithTrace")
# expect_false(inherits(library, "functionWithTrace"))
inherits(library, "functionWithTrace")
# expect_false(inherits(attach, "functionWithTrace"))
inherits(attach, "functionWithTrace")
# try tracing some stuff that shouldn't be
# expect_warning(untz.glob$shimFuns("baljevzxhjLsdc"), "some cannot be found")
untz.glob$shimFuns("baljevzxhjLsdc")
# test unexpected message or behavior from `trace_at_end`
# expect_error(untz.glob$shimFun("sum"), "missing shim data")
untz.glob$shimFun("sum")
with_mock(`unitizer:::trace_at_end` = function(...) stop("trace_at_end fail"), 
    expect_true(any(grepl("trace_at_end fail", capture.output(trace.fail <- untz.glob$shimFun("library"), 
        type = "message"), fixed = TRUE))))
# expect_false(trace.fail)
trace.fail
with_mock(`unitizer:::trace_at_end` = function(...) message("random message"), 
    expect_message(untz.glob$shimFun("library"), "random"))
with_mock(`unitizer:::trace_at_end` = function(...) TRUE, expect_warning(dont.trace <- untz.glob$shimFun("library"), 
    "not traced"))
# expect_false(dont.trace)
dont.trace
untz.glob$release()
# untrace condition
untz.glob <- unitizer:::unitizerGlobal$new(par.env = my.env, 
    set.global = TRUE)
untz.glob$shimFuns()
with_mock(`unitizer:::untrace_utz` = function(what, signature = NULL, 
    where = topenv(parent.frame())) {
    message("untrace dummy")
    base::untrace(what = what, signature = signature, where = where)
}, expect_message(untz.glob$unshimFuns(), "untrace dummy"))
untz.glob$release()
try(detach("package:unitizerdummypkg1", unload = TRUE), silent = TRUE)
while ("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))
# test_that("find_returns", {
#     fun <- function() {
#         if (TRUE) 
#             return(1)
#         else {
#             {
#                 2 + 2
#                 identity(c(1, 2, return(3), {
#                   list(1, 2, 5)
#                   return(return(4))
#                 }))
#                 return(5)
#             }
#             return(6)
#         }
#         if (TRUE) 
#             return(7)
#         else return(8)
#         return(9)
#         return(10)
#     }
#     expect_equal(ret.loc <- unitizer:::find_returns(fun), list(2:3, 
#         c(2L, 4L, 2L, 3L, 2L, 4L), c(2L, 4L, 2L, 3L, 2L, 5L, 
#             3L), c(2L, 4L, 2L, 4L), c(2L, 4L, 3L), c(3L, 3L), 
#         3:4, 4L, 5L))
#     expect_true(all(vapply(unitizer:::get_returns(fun, ret.loc), 
#         function(x) x[[1L]] == quote(return), logical(1L))))
# })
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
# expect_equal(ret.loc <- unitizer:::find_returns(fun), list(2:3, 
#     c(2L, 4L, 2L, 3L, 2L, 4L), c(2L, 4L, 2L, 3L, 2L, 5L, 3L), 
#     c(2L, 4L, 2L, 4L), c(2L, 4L, 3L), c(3L, 3L), 3:4, 4L, 5L))
ret.loc <- unitizer:::find_returns(fun)
# # Validate visually that this worked
# expect_true(all(vapply(unitizer:::get_returns(fun, ret.loc), 
#     function(x) x[[1L]] == quote(return), logical(1L))))
all(vapply(unitizer:::get_returns(fun, ret.loc), function(x) x[[1L]] == 
    quote(return), logical(1L)))
