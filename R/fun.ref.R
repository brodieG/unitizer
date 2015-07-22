#' List of functions we intend to shim, original values so we can make sure they
#' haven't changed before we try to shim them.
#'
#' Code is copied directly from R 3.1.2
#'
#' @keywords internal

.unitizer.base.funs.ref <- list(
  library=function(
    package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
    logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE,
    verbose = getOption("verbose")
  ) {
    testRversion <- function(pkgInfo, pkgname, pkgpath) {
        if (is.null(built <- pkgInfo$Built))
            stop(gettextf("package %s has not been installed properly\n",
                sQuote(pkgname)), call. = FALSE, domain = NA)
        R_version_built_under <- as.numeric_version(built$R)
        if (R_version_built_under < "3.0.0")
            stop(gettextf("package %s was built before R 3.0.0: please re-install it",
                sQuote(pkgname)), call. = FALSE, domain = NA)
        current <- getRversion()
        if (length(Rdeps <- pkgInfo$Rdepends2)) {
            for (dep in Rdeps) if (length(dep) > 1L) {
                target <- dep$version
                res <- if (is.character(target)) {
                  do.call(dep$op, list(as.numeric(R.version[["svn rev"]]),
                    as.numeric(sub("^r", "", dep$version))))
                }
                else {
                  do.call(dep$op, list(current, as.numeric_version(target)))
                }
                if (!res)
                  stop(gettextf("This is R %s, package %s needs %s %s",
                    current, sQuote(pkgname), dep$op, target),
                    call. = FALSE, domain = NA)
            }
        }
        if (R_version_built_under > current)
            warning(gettextf("package %s was built under R version %s",
                sQuote(pkgname), as.character(built$R)), call. = FALSE,
                domain = NA)
        platform <- built$Platform
        r_arch <- .Platform$r_arch
        if (.Platform$OS.type == "unix") {
            if (!nzchar(r_arch) && length(grep("\\w", platform)) &&
                !testPlatformEquivalence(platform, R.version$platform))
                stop(gettextf("package %s was built for %s",
                  sQuote(pkgname), platform), call. = FALSE,
                  domain = NA)
        }
        else {
            if (nzchar(platform) && !grepl("mingw", platform))
                stop(gettextf("package %s was built for %s",
                  sQuote(pkgname), platform), call. = FALSE,
                  domain = NA)
        }
        if (nzchar(r_arch) && file.exists(file.path(pkgpath,
            "libs")) && !file.exists(file.path(pkgpath, "libs",
            r_arch)))
            stop(gettextf("package %s is not installed for 'arch = %s'",
                sQuote(pkgname), r_arch), call. = FALSE, domain = NA)
    }
    checkLicense <- function(pkg, pkgInfo, pkgPath) {
        L <- tools:::analyze_license(pkgInfo$DESCRIPTION["License"])
        if (!L$is_empty && !L$is_verified) {
            site_file <- path.expand(file.path(R.home("etc"),
                "licensed.site"))
            if (file.exists(site_file) && pkg %in% readLines(site_file))
                return()
            personal_file <- path.expand("~/.R/licensed")
            if (file.exists(personal_file)) {
                agreed <- readLines(personal_file)
                if (pkg %in% agreed)
                  return()
            }
            else agreed <- character()
            if (!interactive())
                stop(gettextf("package %s has a license that you need to accept in an interactive session",
                  sQuote(pkg)), domain = NA)
            lfiles <- file.path(pkgpath, c("LICENSE", "LICENCE"))
            lfiles <- lfiles[file.exists(lfiles)]
            if (length(lfiles)) {
                message(gettextf("package %s has a license that you need to accept after viewing",
                  sQuote(pkg)), domain = NA)
                readline("press RETURN to view license")
                encoding <- pkgInfo$DESCRIPTION["Encoding"]
                if (is.na(encoding))
                  encoding <- ""
                if (encoding == "latin1")
                  encoding <- "cp1252"
                file.show(lfiles[1L], encoding = encoding)
            }
            else {
                message(gettextf("package %s has a license that you need to accept:\naccording to the DESCRIPTION file it is",
                  sQuote(pkg)), domain = NA)
                message(pkgInfo$DESCRIPTION["License"], domain = NA)
            }
            choice <- menu(c("accept", "decline"), title = paste("License for",
                sQuote(pkg)))
            if (choice != 1)
                stop(gettextf("license for package %s not accepted",
                  sQuote(package)), domain = NA, call. = FALSE)
            dir.create(dirname(personal_file), showWarnings = FALSE)
            writeLines(c(agreed, pkg), personal_file)
        }
    }
    checkNoGenerics <- function(env, pkg) {
        nenv <- env
        ns <- .getNamespace(as.name(pkg))
        if (!is.null(ns))
            nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE))
            TRUE
        else {
            length(objects(env, pattern = "^\\.__T", all.names = TRUE)) ==
                0L
        }
    }
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics,
        env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
            ".Random.seed", ".Last.lib", ".onDetach", ".packageName",
            ".noGenerics", ".required", ".no_S3_generics", ".Depends",
            ".requireCachedGenerics")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ob <- objects(lib.pos, all.names = TRUE)
        if (!nogenerics) {
            these <- ob[substr(ob, 1L, 6L) == ".__T__"]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != package]
            ob <- ob[!(ob %in% gen)]
        }
        fst <- TRUE
        ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads",
            "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob,
                nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if (length(Classobjs))
                  same <- same[-Classobjs]
                same.isFn <- function(where) vapply(same, exists,
                  NA, where = where, mode = "function", inherits = FALSE)
                same <- same[same.isFn(i) == same.isFn(lib.pos)]
                not.Ident <- function(ch, TRAFO = identity, ...) vapply(ch,
                  function(.) !identical(TRAFO(get(., i)), TRAFO(get(.,
                    lib.pos)), ...), NA)
                if (length(same))
                  same <- same[not.Ident(same)]
                if (length(same) && identical(sp[i], "package:base"))
                  same <- same[not.Ident(same, ignore.environment = TRUE)]
                if (length(same)) {
                  if (fst) {
                    fst <- FALSE
                    packageStartupMessage(gettextf("\nAttaching package: %s\n",
                      sQuote(package)), domain = NA)
                  }
                  msg <- .maskedMsg(same, pkg = sQuote(sp[i]),
                    by = i < lib.pos)
                  packageStartupMessage(msg, domain = NA)
                }
            }
        }
    }
    if (verbose && quietly)
        message("'verbose' and 'quietly' are both true; being verbose then ..")
    if (!missing(package)) {
        if (is.null(lib.loc))
            lib.loc <- .libPaths()
        lib.loc <- lib.loc[file.info(lib.loc)$isdir %in% TRUE]
        if (!character.only)
            package <- as.character(substitute(package))
        if (length(package) != 1L)
            stop("'package' must be of length 1")
        if (is.na(package) || (package == ""))
            stop("invalid package name")
        pkgname <- paste("package", package, sep = ":")
        newpackage <- is.na(match(pkgname, search()))
        if (newpackage) {
            pkgpath <- find.package(package, lib.loc, quiet = TRUE,
                verbose = verbose)
            if (length(pkgpath) == 0L) {
                txt <- if (length(lib.loc))
                  gettextf("there is no package called %s", sQuote(package))
                else gettext("no library trees found in 'lib.loc'")
                if (logical.return) {
                  warning(txt, domain = NA)
                  return(FALSE)
                }
                else stop(txt, domain = NA)
            }
            which.lib.loc <- normalizePath(dirname(pkgpath),
                "/", TRUE)
            pfile <- system.file("Meta", "package.rds", package = package,
                lib.loc = which.lib.loc)
            if (!nzchar(pfile))
                stop(gettextf("%s is not a valid installed package",
                  sQuote(package)), domain = NA)
            pkgInfo <- readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)
            if (!package %in% c("datasets", "grDevices", "graphics",
                "methods", "splines", "stats", "stats4", "tcltk",
                "tools", "utils") && isTRUE(getOption("checkPackageLicense",
                FALSE)))
                checkLicense(package, pkgInfo, pkgpath)
            if (is.character(pos)) {
                npos <- match(pos, search())
                if (is.na(npos)) {
                  warning(gettextf("%s not found on search path, using pos = 2",
                    sQuote(pos)), domain = NA)
                  pos <- 2
                }
                else pos <- npos
            }
            .getRequiredPackages2(pkgInfo, quietly = quietly)
            deps <- unique(names(pkgInfo$Depends))
            if (packageHasNamespace(package, which.lib.loc)) {
                tt <- try({
                  ns <- loadNamespace(package, c(which.lib.loc,
                    lib.loc))
                  env <- attachNamespace(ns, pos = pos, deps)
                })
                if (inherits(tt, "try-error"))
                  if (logical.return)
                    return(FALSE)
                  else stop(gettextf("package or namespace load failed for %s",
                    sQuote(package)), call. = FALSE, domain = NA)
                else {
                  on.exit(detach(pos = pos))
                  nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env,
                    package)
                  if (warn.conflicts && !exists(".conflicts.OK",
                    envir = env, inherits = FALSE))
                    checkConflicts(package, pkgname, pkgpath,
                      nogenerics, ns)
                  on.exit()
                  if (logical.return)
                    return(TRUE)
                  else return(invisible(.packages()))
                }
            }
            else stop(gettextf("package %s does not have a namespace and should be re-installed",
                sQuote(package)), domain = NA)
        }
        if (verbose && !newpackage)
            warning(gettextf("package %s already present in search()",
                sQuote(package)), domain = NA)
    }
    else if (!missing(help)) {
        if (!character.only)
            help <- as.character(substitute(help))
        pkgName <- help[1L]
        pkgPath <- find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"),
            file.path(pkgPath, "INDEX"))
        if (file.exists(vignetteIndexRDS <- file.path(pkgPath,
            "Meta", "vignette.rds")))
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector("list", 3L)
        readDocFile <- function(f) {
            if (basename(f) %in% "package.rds") {
                txt <- readRDS(f)$DESCRIPTION
                if ("Encoding" %in% names(txt)) {
                  to <- if (Sys.getlocale("LC_CTYPE") == "C")
                    "ASCII//TRANSLIT"
                  else ""
                  tmp <- try(iconv(txt, from = txt["Encoding"],
                    to = to))
                  if (!inherits(tmp, "try-error"))
                    txt <- tmp
                  else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible",
                    call. = FALSE)
                }
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) +
                  3)
            }
            else if (basename(f) %in% "vignette.rds") {
                txt <- readRDS(f)
                if (is.data.frame(txt) && nrow(txt))
                  cbind(basename(gsub("\\.[[:alpha:]]+$", "",
                    txt$File)), paste(txt$Title, paste0(rep.int("(source",
                    NROW(txt)), ifelse(txt$PDF != "", ", pdf",
                    ""), ")")))
                else NULL
            }
            else readLines(f)
        }
        for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
    }
    else {
        if (is.null(lib.loc))
            lib.loc <- .libPaths()
        db <- matrix(character(), nrow = 0L, ncol = 3L)
        nopkgs <- character()
        for (lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for (i in sort(a)) {
                file <- system.file("Meta", "package.rds", package = i,
                  lib.loc = lib)
                title <- if (file != "") {
                  txt <- readRDS(file)
                  if (is.list(txt))
                    txt <- txt$DESCRIPTION
                  if ("Encoding" %in% names(txt)) {
                    to <- if (Sys.getlocale("LC_CTYPE") == "C")
                      "ASCII//TRANSLIT"
                    else ""
                    tmp <- try(iconv(txt, txt["Encoding"], to,
                      "?"))
                    if (!inherits(tmp, "try-error"))
                      txt <- tmp
                    else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible",
                      call. = FALSE)
                  }
                  txt["Title"]
                }
                else NA
                if (is.na(title))
                  title <- " ** No title available ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if (length(a) == 0L)
                nopkgs <- c(nopkgs, lib)
        }
        dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
        if (length(nopkgs) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages",
                "libraries %s contain no packages"), pkglist)
            warning(msg, domain = NA)
        }
        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }
    if (logical.return)
        TRUE
    else invisible(.packages())
  },
  attach=function(
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
    if (length(ls(envir = value, all.names = TRUE)) && .isMethodsDispatchOn())
        methods:::cacheMetaData(value, TRUE)
    invisible(value)
  },
  detach=function (
    name, pos = 2L, unload = FALSE, character.only = FALSE,
    force = FALSE
  ) {
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
    if (pkgname %in% loadedNamespaces()) {
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
  },
  require=function (
    package, lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE,
    character.only = FALSE
  ) {
    if (!character.only)
        package <- as.character(substitute(package))
    loaded <- paste("package", package, sep = ":") %in% search()
    if (!loaded) {
        if (!quietly)
            packageStartupMessage(gettextf("Loading required package: %s",
                package), domain = NA)
        value <- tryCatch(library(package, lib.loc = lib.loc,
            character.only = TRUE, logical.return = TRUE, warn.conflicts = warn.conflicts,
            quietly = quietly), error = function(e) e)
        if (inherits(value, "error")) {
            if (!quietly) {
                msg <- conditionMessage(value)
                cat("Failed with error:  ", sQuote(msg), "\n",
                  file = stderr(), sep = "")
                .Internal(printDeferredWarnings())
            }
            return(invisible(FALSE))
        }
        if (!value)
            return(invisible(FALSE))
    }
    else value <- TRUE
    invisible(value)
  }
)
