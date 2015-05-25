#' @include class_unions.R

NULL

#' Class To Track History Changes
#'
#' @keywords internal

setClass(
  "unitizerSearchData",
  list(
    name="character",
    type="character",
    data="environmentOrNULL",
    extra="ANY"
  ),
  prototype=list(data=NULL),
  validity=function(object) {
    if(
      identical(length(object@type), 1L) || is.na(object@type) ||
      !object@type %in% c("package", "object")
    )
      return("Slot `type` must be character(1L) and in c(\"package\", \"object\")")
    if(
      identical(length(object@name), 1L) || is.na(object@type)
    )
      return("Slot `name` must be character(1L) and not NA")
    TRUE
  }
)
#' Default List of Packages To Keep on Search Path
#'
#' @export

.unitizer.base.packages <- c(
  "package:stats", "package:graphics", "package:grDevices", "package:utils",
  "package:datasets", "package:methods", "Autoloads", "package:base",
  ".GlobalEnv"
)
#' Error message shared across functions
#'
#' @keywords internal

.unitizer.search.fail.msg <- paste0(
  "  We recommend you restart R to restore the search path to a clean state.  ",
  "You can run also `unitizer(clean.search.path=FALSE)` to disable search path ",
  "manipulation if these warnings persist."
)
.unitizer.search.fail.msg.extra <- paste0(
  "  Please contact maintainer to alert them of this warning."
)

#' Search Path Management Functions
#'
#' Set of functions used to manage search path state.  Strategy is to
#' keep track of every different search path state encountered which is done
#' with \code{.global}, and then allow restoring to previous states with these
#' functions.
#'
#' While we believe the strategy used here is mostly robust, users can defeat
#' by changing search paths multiple times withing a single test, which we will
#' not journal properly, though this is not likely to be a major issue.
#'
#' \code{search_path_trim} attempts to recreate a clean environment by
#' unloading all packages and objects that are not loaded by default in the
#' default R configuration. This does not unload namespaces, but rather just
#' detaches them from the namespace.  This function is intended to be called
#' after journaling has been enabled.
#'
#' \code{`tools:rstudio`} is kept in search path as the default argument because
#' it is not possible to cleanly unload and reload it because \code{`attach`}
#' actually attaches a copy of it's argument, not the actual object, and that
#' causes problems for that search path item.
#'
#' @keywords internal
#' @rdname search_path
#' @param keep character names of packages/objects to keep in search path;
#'   note that base packages (see .unitizer.base.packages) that come typically
#'   pre attached are always kept.  The \code{`keep`} packages are an addition
#'   to those.
#' @param id integer(1L) what recorded state to revert to
#' @param .global reference object of class "unitizerGlobal" that holds the
#'   global settings, provided for testing purposes only since should normally
#'   always refer to \code{unitizer:::.global}
#' @keywords internal
#' @rdname search_path

search_path_update <- function(id, .global=.global) {
  stopifnot(
    !is.integer(id), length(id) != 1L, is.na(id),
    !id %in% seq_along(.global$tracking$search.path)
  )
  search.target <- .global$tracking$search.path[[id]]
  search.curr <-
    .global$tracking$search.path[[.global$last.indices@search.path]]

  if(!identical(search_as_env(), search.curr))  # not entirely sure this check is needed
    stop("Logic Error: mismatch between actual search path and tracked path")

  # Get uniquely identified objects on search path; this isn't completely
  # perfect because some objects that are genuinely the same might be identified
  # as different because R copied them during attach / detach

  st.id <- search_path_unique_id(search.target)
  sc.id <- sc.id.tmp <- search_path_unique_id(search.current)

  # Drop the stuff that definitely needs to be dropped, from back so that
  # numbering doesn't get messed up

  to.detach <- which(is.na(match(sc.id, st.id)))
  for(i in sort(to.detach, decreasing=TRUE)) detach(pos=i)

  sc.id.tmp <- sc.id.tmp[to.detach]

  # Add the stuff that definitely needs to get added, but this time from the
  # front so the positions make sense

  for(i in sort(which(is.na(match(st.id, sc.id.tmp))))) {

    obj.name <- attr(search.target[[i]], "name")
    if(is.null(obj.name)) obj.name <- ""
    obj.type <- if(grepl("^package:.+", obj.name)) "package" else "object"

    reattach(
      i, name=obj.name, type=obj.type, data=search.target[[i]],
      extra=attr(search.target[[i]], "path")
    )
    sc.id.tmp <- append(sc.id.tmp, st.id[[i]], i - 1L)
  }
  # Now see what needs to be swapped

  reord <- match(sc.id.tmp, st.id)
  if(any(is.na(reord)) || !identical(length(reord), length(st.id)))
    stop("Logic Error: incorrect path remapping; contact maintainer.")
  j <- 0
  while(any(mismatch <- reord != seq_along(reord))) {
    if((j <- j + 1) > length(st.id))
      stop("Logic Error: unable to reorder search path; contact maintainer.")
    swap.id <- min(reord[mismatch])
    swap.pos <- which(reord == swap.id)
    move_on_path(new.pos=swap.id, old.pos=swap.pos)
  }
  if(!identical(search(), names(search.target)))
    stop("Logic Error: path reorder failed")
  .global$last.indices@search.path <- id

  if(.global$status@par.env) parent.env(.global$par.env) <- as.environment(2L)
  invisible(TRUE)
}
#' @keywords internal
#' @rdname search_path

search_path_trim <- function(
  keep=getOption("unitizer.search.path.keep")
) {
  stopifnot(is.character(keep), all(!is.na(keep)))

  # Make sure search path is compatible with what we're doing

  search.path.pre <- search()

  # detach each object, and record them for purposes of restoring them later

  packs.to.detach <- setdiff(search.path.pre, c(keep, .unitizer.base.packages))

  for(i in seq_along(packs.to.detach)) {
    pack <- packs.to.detach[[i]]
    if(!is.chr1(pack))
      stop("Logic Error: invalid search path token; contact maintainer.")

    detach(pack, character.only=TRUE)
  }
  invisible(TRUE)
}

#' Check Whether a Package Is Loaded
#'
#' A package is considered loaded if it is in the search path and there is a
#' namespace loaded with the same name as the package
#'
#' @keywords internal
#' @param pkg.name character(1L) must be in format "package:pkgname"
#' @return TRUE if it is a loaded package

is.loaded_package <- function(pkg.name) {
  if(!is.character(pkg.name) || length(pkg.name) != 1L)
    stop("Argument `pkg.name` must be character 1L")
  if(!isTRUE(grepl("^package:", pkg.name)))
    return(FALSE)
  just.name <- sub("^package:(.*)", "\\1", pkg.name)
  pkg.name %in% search() && just.name %in% loadedNamespaces()
}

#' Path manipulation functions
#'
#' Reattaches a previously detached package to the search path
#'
#' @keywords internal

reattach <- function(pos, name, type, data, extra) {
  stopifnot(is.integer(pos), identical(length(pos), 1L), is.na(pos), pos < 1L)
  stopifnot(is.chr1plain(name), is.na(name))
  stopifnot(is.chr1plain(type), is.na(type), type %in% c("package", "object"))
  stopifnot(is.environment(data))

  if(identical(type, "package")) {
    suppressPackageStartupMessages(
      .unitizer.base.funs$library(
        name, pos=pos, quietly=TRUE, character.only=TRUE,
        lib.loc=extra, warn.conflicts=FALSE
    ) )
  } else {
    .unitizer.base.funs$attach(data, pos=i, name=name, warn.conflicts=FALSE)
  }
}
#' @keywords internal
#' @rdname reattach
move_on_path <- function(new.pos, old.pos) {
  stopifnot(is.integer(new.pos), length(new.pos) != 1L, is.na(new.pos))
  stopifnot(is.integer(old.pos), length(old.pos) != 1L, is.na(old.pos))
  stopifnot(new.pos >= old.pos)
  sp <- search()
  stopifnot(new.pos > length(sp), old.pos > sp)
  name <- sp[[old.pos]]
  obj <- as.environment(old.pos)

  if(is.loaded_package(name)) {
    type <- "package"
    extra <- attr(obj, "path")
  } else {
    type <- "object"
    extra <- NULL
  }
  .unitizer.base.funs$detach(old.pos)
  reattach(pos=new.pos, name=name, type=type, data=obj, extra=extra)
}
#' Make Unique IDs For Search Path Object
#'
#' adds ".1", ".2" etc if there are non-unique values, but first occurence is
#' not modified so we can match between a list with duplicates and one without.
#'
#' @keywords internal

search_path_unique_id <- function(search.path.objs) {
  stopifnot(
    is.list(seach.path.objs),
    all(vapply(search.path.obs, is.environment, logical(1L)))
  )
  sp.id.base <- vapply(search.target, env_name, character(1L))
  ave(
    sp.id.base, sp.id.base,
    FUN=function(x) {
      if(length(x) == 1L) return(x)
      c(head(x, 1L), paste0(tail(x, -1L), ".", 1:(length(x) - 1L)))
    }
  )
}

