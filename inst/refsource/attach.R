# Make sure to update the location of the shims if you modify this code

function(
    what, pos = 2L, name = deparse(substitute(what)), warn.conflicts = TRUE
) {
    checkConflicts <- function(env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
            ".Random.seed", ".Last.lib", ".onDetach", ".packageName",
            ".noGenerics", ".required", ".no_S3_generics", ".requireCachedGenerics")
        sp <- search()
        for (i in seq_along(sp)) {
            if (identical(env, as.environment(i))) {
                db.pos <- i
                break
            }
        }
        ob <- objects(db.pos, all.names = TRUE)
        if (.isMethodsDispatchOn()) {
            these <- ob[substr(ob, 1L, 6L) == ".__T__"]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(db.pos, match(c("Autoloads",
            "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob,
                nomatch = 0L)
            if (any(obj.same > 0L)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if (length(Classobjs))
                  same <- same[-Classobjs]
                same.isFn <- function(where) vapply(same, exists,
                  NA, where = where, mode = "function", inherits = FALSE)
                same <- same[same.isFn(i) == same.isFn(db.pos)]
                if (length(same)) {
                  pkg <- if (sum(sp == sp[i]) > 1L)
                    sprintf("%s (pos = %d)", sp[i], i)
                  else sp[i]
                  message(.maskedMsg(same, pkg, by = i < db.pos),
                    domain = NA)
                }
            }
        }
    }
    if (pos == 1L) {
        warning("*** 'pos=1' is not possible; setting 'pos=2' for now.\n",
            "*** Note that 'pos=1' will give an error in the future")
        pos <- 2L
    }
    if (is.character(what) && (length(what) == 1L)) {
        if (!file.exists(what))
            stop(gettextf("file '%s' not found", what), domain = NA)
        if (missing(name))
            name <- paste0("file:", what)
        value <- .Internal(attach(NULL, pos, name))
        load(what, envir = as.environment(pos))
    }
    else value <- .Internal(attach(what, pos, name))
    if (warn.conflicts && !exists(".conflicts.OK", envir = value,
        inherits = FALSE)) {
        checkConflicts(value)
    }
    if (length(names(value)) && .isMethodsDispatchOn())
        methods:::cacheMetaData(value, TRUE)
    invisible(value)
}
