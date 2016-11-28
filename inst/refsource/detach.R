# Make sure to update the location of the shims if you modify this code

function (
    name, pos = 2L, unload = FALSE, character.only = FALSE, force = FALSE
){
    if (!missing(name)) {
        if (!character.only)
            name <- substitute(name)
        pos <- if (is.numeric(name))
            name
        else {
            if (!is.character(name))
                name <- deparse(name)
            match(name, search())
        }
        if (is.na(pos))
            stop("invalid 'name' argument")
    }
    packageName <- search()[[pos]]
    if (!grepl("^package:", packageName))
        return(invisible(.Internal(detach(pos))))
    pkgname <- sub("^package:", "", packageName)
    for (pkg in search()[-1L]) {
        if (grepl("^package:", pkg) && exists(".Depends", pkg,
            inherits = FALSE) && pkgname %in% get(".Depends",
            pkg, inherits = FALSE))
            if (force)
                warning(gettextf("package %s is required by %s, which may no longer work correctly",
                  sQuote(pkgname), sQuote(sub("^package:", "",
                    pkg))), call. = FALSE, domain = NA)
            else stop(gettextf("package %s is required by %s so will not be detached",
                sQuote(pkgname), sQuote(sub("^package:", "",
                  pkg))), call. = FALSE, domain = NA)
    }
    env <- as.environment(pos)
    libpath <- attr(env, "path")
    hook <- getHook(packageEvent(pkgname, "detach"))
    for (fun in rev(hook)) try(fun(pkgname, libpath))
    ns <- .getNamespace(pkgname)
    if (!is.null(ns) && exists(".onDetach", mode = "function",
        where = ns, inherits = FALSE)) {
        .onDetach <- get(".onDetach", mode = "function", pos = ns,
            inherits = FALSE)
        if (!is.null(libpath)) {
            res <- tryCatch(.onDetach(libpath), error = identity)
            if (inherits(res, "error")) {
                warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s",
                  ".onDetach", "detach", pkgname, deparse(conditionCall(res))[1L],
                  conditionMessage(res)), call. = FALSE, domain = NA)
            }
        }
    }
    else if (exists(".Last.lib", mode = "function", where = pos,
        inherits = FALSE)) {
        .Last.lib <- get(".Last.lib", mode = "function", pos = pos,
            inherits = FALSE)
        if (!is.null(libpath)) {
            res <- tryCatch(.Last.lib(libpath), error = identity)
            if (inherits(res, "error")) {
                warning(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s",
                  ".Last.lib", "detach", pkgname, deparse(conditionCall(res))[1L],
                  conditionMessage(res)), call. = FALSE, domain = NA)
            }
        }
    }
    .Internal(detach(pos))
    if (isNamespaceLoaded(pkgname)) {
        if (unload) {
            tryCatch(unloadNamespace(pkgname), error = function(e) warning(gettextf("%s namespace cannot be unloaded:\n  ",
                sQuote(pkgname)), conditionMessage(e), call. = FALSE,
                domain = NA))
        }
    }
    else {
        if (.isMethodsDispatchOn() && methods:::.hasS4MetaData(env))
            methods:::cacheMetaData(env, FALSE)
        .Internal(lazyLoadDBflush(paste0(libpath, "/R/", pkgname,
            ".rdb")))
    }
    invisible()
}
