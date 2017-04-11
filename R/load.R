## Store Retrieve Unitizer
##
## If this errors, calling function should abort
##
## @keywords internal
## @param unitizer a \code{\link{unitizer-class}} object
## @param store.id anything for which there is a defined \code{\link{get_unitizer}}
##   method; by default should be the path to a unitizer; if
##   \code{`\link{get_unitizer}`} returns \code{`FALSE`} then this will create
##   a new unitizer
## @param par.frame the environment to use as the parent frame for the \code{unitizer}
## @param test.file the R file associated with the store id
## @param force.upgrade whether to allow upgrades in non-interactive mode, for
##   testing purposes
## @param global the global tracking object
## @return a \code{unitizer} object, or anything, in which case the calling
##   code should exit

load_unitizers <- function(
  store.ids, test.files, par.frame, interactive.mode, mode, force.upgrade=FALSE,
  global=unitizerGlobal$new()
) {
  if(!is.character(test.files))
    stop("Argument `test.files` must be character")
  if(!is.environment(par.frame))
    stop("Argument `par.frame` must be an environment")
  if(!is.list(store.ids) || !identical(length(store.ids), length(test.files)))
    stop(
      "Argument `store.ids` must be a list of the same length as `test.files`"
    )
  stopifnot(isTRUE(interactive.mode) || identical(interactive.mode, FALSE))
  stopifnot(is.chr1plain(mode), !is.na(mode), mode %in% c("unitize", "review"))

  # Get names for display

  chr.ids <- vapply(
    seq(store.ids),
    function(x) best_store_name(store.ids[[x]], test.files[[x]]),
    character(1L)
  )
  chr.files <- vapply(
    seq(store.ids),
    function(x) best_file_name(store.ids[[x]], test.files[[x]]),
    character(1L)
  )
  # Get RDSs and run basic checks; `valid` will contain character strings
  # describing failures, or 0 length string if succeeded

  unitizers <- lapply(
    seq(store.ids),
    function(x) {
      if(is(store.ids[[x]], "unitizer")) {
        return(store.ids[[x]])
      }
      store.ids[[x]] <- try(get_unitizer(store.ids[[x]]), silent=TRUE)
      if(inherits(store.ids[[x]], "try-error"))
        return(
          paste0(
            c(
              "`get_unitizer` error: ",
              conditionMessage(attr(store.ids[[x]], "condition"))
            ),
            collapse=""
        ) )
      if(is(store.ids[[x]], "unitizer")) return(store.ids[[x]])
      if(identical(store.ids[[x]], FALSE)) {
        return(
          new(
            "unitizer", id=norm_store_id(store.ids[[x]]),
            zero.env=new.env(parent=par.frame),
            test.file.loc=norm_file(test.files[[x]])
      ) ) }
      return(
        "`get_unitizer` returned something other than a `unitizer` or FALSE"
  ) } )
  null.version <- package_version("0.0.0")
  curr.version <- packageVersion("unitizer")
  valid <- vapply(
    unitizers, unitizer_valid, character(1L), curr.version=curr.version
  )
  # unitizers without a `version` slot or slot in incorrect form not eligible
  # for upgrade

  versions  <- lapply(
    unitizers,
    function(x)
      if(
        !is(x, "unitizer") ||
        inherits(
          x.ver <- try(package_version(x@version), silent=TRUE), "try-error"
        ) || !is.package_version(x.ver)
      ) null.version else x.ver
  )
  version.out.of.date <- vapply(
    versions, function(x) !identical(x, null.version) && curr.version > x,
    logical(1L)
  )
  valid.idx <- which(!nchar(valid))
  invalid.idx <- which(nchar(valid) & !version.out.of.date)
  toup.idx <- which(version.out.of.date & nchar(valid))
  toup.fail.idx <- integer(0L)

  # Attempt to resolve failures by upgrading if relevant

  if(length(toup.idx)) {
    many <- length(toup.idx) > 1L
    meta_word_cat(
      paste0(
        "\nThe following unitizer", if(many) "s",
        if(force.upgrade) " will" else " must", " be upgraded to version '",
        as.character(curr.version), "':\n"
      ),
      as.character(
        UL(
          paste0(
            chr.ids[toup.idx], " (at '",
            vapply(versions[toup.idx], as.character, character(1L))
            , "')"
        ) ) ,
        width=getOption("width") - 2L
      )
    )
    if(!interactive.mode && !force.upgrade)
      stop("Cannot upgrade unitizers in non-interactive mode")

    pick <- if(interactive.mode) {
      meta_word_msg("unitizer upgrades are IRREVERSIBLE.  Proceed?")
      unitizer_prompt(
        "Upgrade unitizer stores?", hist.con=NULL,
        valid.opts=c(Y="[Y]es", N="[N]o"), global=global,
        browse.env=
      )
    } else "Y"

    if(identical(pick, "Y")) {
      upgraded <- lapply(unitizers[toup.idx], upgrade)
      upgrade.success <- vapply(upgraded, is, logical(1L), "unitizer")

      for(i in which(upgrade.success)) {
        # Actually same unitizer may be run against multiple test files
        # so this check is useless
        if(
          !identical(
            basename(upgraded[[i]]@test.file.loc),
            basename(test.files[toup.idx][[i]])
          )
        ) warning(
          "Upgraded test file does not match original test file ",
          "('", basename(upgraded[[i]]@test.file.loc), "' vs '",
          basename(test.files[toup.idx][[i]]), "').", immediate.=TRUE
        )
        upgraded[[i]]@id <- norm_store_id(store.ids[toup.idx][[i]])
        upgraded[[i]]@test.file.loc <- norm_file(test.files[toup.idx][[i]])

        store.attempt <- try(store_unitizer(upgraded[[i]]), silent=TRUE)
        if(inherits(store.attempt, "try-error")) {
          upgraded[[i]] <- paste0(
            "Unable to store upgraded unitizer: ",
            conditionMessage(attr(store.attempt, "condition"))
          )
          upgrade.success[[i]] <- FALSE
        }
      }
      unitizers[toup.idx[upgrade.success]] <- upgraded[upgrade.success]
      valid.idx <- c(valid.idx, toup.idx[upgrade.success])
      toup.fail.idx <- toup.idx[!upgrade.success]
      valid[toup.fail.idx] <- upgraded[!upgrade.success]
    } else {
      meta_word_msg("unitizer(s) listed above will not be tested")
      toup.fail.idx <- toup.idx
      valid[toup.fail.idx] <- "User elected not to upgrade unitizers"
      valid.idx <- which(!nchar(valid))
    }
  }
  # Cleanup the unitizers

  for(i in valid.idx) {
    unitizers[[i]]@id <- norm_store_id(store.ids[[i]])
    unitizers[[i]]@test.file.loc <- norm_file(test.files[[i]])

    parent.env(unitizers[[i]]@zero.env) <- par.frame
    unitizers[[i]]@global <- global
    # awkward, shouldn't be done this way
    unitizers[[i]]@eval <- identical(mode, "unitize")

    # # check for and normalize state issues; this came up as a result of #197
    # # where our state objects were corrupted; really this should be handled
    # # in a more systematic way like we do for broader validation; if this
    # # happens more often we can look into that;
    # # NOTE: figured out what was going on here so commenting this out for now

    # ref.indices <- do.call(
    #   cbind,
    #   lapply(
    #     unitizers[[i]]@items.ref,
    #     function(z) as.integer(slot(z, "glob.indices"))
    #   )
    # )
    # ref.state <- lapply(
    #   slotNames(unitizers[[i]]@state.ref),
    #   function(z) length(slot(unitizers[[i]]@state.ref, z))
    # )
    # if(!identical(names(ref.state), names(ref.indices)))
    #   stop(
    #     "Internal error: incompatible global index structure; ",
    #     "contact maintainer"
    #   )

    # if(
    #   any(
    #     unlist(
    #       Map(
    #         function(v, w) any(v > w),
    #         split(ref.indices, row(ref.indices), ref.state), ref.state
    #   ) ) )
    # ) {
    #   meta_word_msg(
    #     "Unitizer ", i, " has corrupted state indices, we are resetting ",
    #     "them to NULL, which means review of reference tests will not ",
    #     "correctly reflect the reference states.  This is not expected ",
    #     "behavior and you should contact maintainer if it persists.", sep=""
    #   )
    #   glob.ind.def <- new("unitizerGlobalIndices")
    #   for(j in seq_along(unitizers[[i]]@items.ref))
    #     unitizers[[i]]@items.ref[[j]]@glob.indices <- glob.ind.def
    # }
  }
  # Issue errors as required

  if(length(invalid.idx)) {
    meta_word_msg(
      paste0(
        "\nThe following unitizer", if(length(invalid.idx) > 1L) "s",
        " could not be loaded:"
      ),
      as.character(
        UL(paste0(chr.ids[invalid.idx], ": ",  valid[invalid.idx])),
        width=getOption("width") - 2L
      )
    )
  }
  if(length(toup.fail.idx)) {
    meta_word_msg(
      paste0(
        "\nThe following unitizer", if(length(toup.fail.idx) > 1L) "s",
        " could not be upgraded to version '", as.character(curr.version),
        "':\n"
      ),
      as.character(
        UL(
          paste0(
            chr.files[toup.fail.idx], " at '",
            vapply(versions[toup.fail.idx], as.character, character(1L)),
            "': ", valid[toup.fail.idx]
        ) ),
        width=getOption("width") - 2L
      )
    )
  }
  if(!length(valid.idx) && (length(invalid.idx) || length(toup.fail.idx)))
    meta_word_msg(
      "No valid unitizer", if(length(store.ids) > 1L) "s", " to load", sep=""
    )
  # Create fail load objects for all failures

  invalid.idx <- which(!seq_along(unitizers) %in% valid.idx)
  unitizers[invalid.idx] <- lapply(
    invalid.idx,
    function(x)
      new(
        "unitizerLoadFail", test.file=test.files[[x]],
        store.id=list(store.ids[[x]]), # this is a list b/c could be S3
        reason=valid[[x]]
  ) )
  new("unitizerObjectList", .items=unitizers)
}

# Need to make sure we do not unintentionally store a bunch of references to
# objects or namespaces we do not want:
#
# \itemize{
#   \item reset parent env to be base
#   \item remove all contents of base.env (otherwise we get functions with
#     environments that reference namespaces)
# }

store_unitizer <- function(unitizer) {
  if(!is(unitizer, "unitizer")) return(invisible(TRUE))

  old.par.env <- parent.env(unitizer@zero.env)
  on.exit(parent.env(unitizer@zero.env) <- old.par.env)
  parent.env(unitizer@zero.env) <- baseenv()

  # zero out connections we'v been using

  if(!is.null(unitizer@global$cons)) close_and_clear(unitizer@global$cons)

  # to avoid taking up a bunch of storage on large object

  unitizer@global <- NULL

  rm(list=ls(unitizer@base.env, all.names=TRUE), envir=unitizer@base.env)

  # Reset other fields

  unitizer@res.data <- NULL
  unitizer@updated.at.least.once <- FALSE
  unitizer@bookmark <- NULL

  # blow away calls; these should be memorialized as deparsed versions and the
  # original ones take up a lot of room to store

  for(i in seq_along(unitizer@items.ref)) unitizer@items.ref[[i]]@call <- NULL

  # shouldn't really be anything here

  for(i in seq_along(unitizer@items.new)) unitizer@items.new[[i]]@call <- NULL

  success <- try(set_unitizer(unitizer@id, unitizer))

  if(!inherits(success, "try-error")) {
    meta_word_msg("unitizer updated.")
  } else {
    stop("Error attempting to save unitizer, see previous messages.")
  }
  return(invisible(TRUE))
}
unitizer_valid <- function(x, curr.version=packageVersion("unitizer")) {
  if(!is(x, "unitizer")) {
    if(!is.chr1plain(x) || nchar(x) < 1L)
      return("unknown unitizer load failure")
    return(x)
  }
  null.version <- package_version("0.0.0")
  version <- try(x@version, silent=TRUE)

  if(inherits(version, "try-error")) {
    msg <- conditionMessage(attr(version, "condition"))
    paste0(
      "could not retrieve version from `unitizer`: ",
      if(nchar(msg)) sprintf(": %s", msg)
    )
  } else {
    # Make sure not using any `unitizer`s with version older than what we're at

    if(!identical(version, null.version) && curr.version < version) {
      paste0(
        "Cannot load a unitizer store of version greater (", version,
        ") than of installed unitizer package (", curr.version, ")"
      )
    } else {
      attempt <- try(validObject(x, complete=TRUE), silent=TRUE)
      if(inherits(attempt, "try-error")) {
        msg <- conditionMessage(attr(attempt, "condition"))
        paste0(
          "unitizer object is invalid", if(nchar(msg)) sprintf(": %s", msg)
        )
      } else ""
} } }
setClass(
  "unitizerLoadFail",
  slots=c(
    test.file="character",
    store.id="list",
    reason="character"
  ),
  validity=function(object) {
    if(!is.character(object@test.file) || length(object@test.file) != 1L)
      return("Slot `test.file` must be character(1L)")
    if(!is.chr1(object@reason))
      return("Slot `reason` must be character(1L)")
    TRUE
  }
)
#' @rdname unitizer_s4method_doc

setMethod(
  "show", "unitizerLoadFail",
  function(object) {
    meta_word_cat(sep="\n",
      "Failed Loading Unitizer:",
      as.character(
        UL(
          c(
            paste0(
              "Test file: ", best_file_name(object@store.id, object@test.file)
            ),
            paste0(
              "Store: ",
              best_store_name(object@store.id[[1L]], object@test.file)
            ),
            paste0("Reason: ", object@reason)
        ) ),
        width=getOption("width") - 2L
    ) )
    invisible(NULL)
  }
)

# Manipulate \code{unitizer} Store and File Names
#
# Used to provide display friendly or absolute versions of \code{unitizer}
# test file or store identifiers.
#
# @section \code{norm_store_id}, \code{norm_file}:
#
# Loosely related to \code{getTarget,unitizer-method} and
# \code{getName,unitizer-method} although these are not trying to convert to
# character or check anything, just trying to normalize if possible.
#
# @section \code{best_store_name}, \code{best_file_name}:
#
# Generate the most intuitive names possible for either the store or the test
# file.
#
# @section \code{as.store_id_chr}:
#
# Converts as store ID to character
#
# @param store.id a \code{unitizer} store id
# @param test.file the location of the R test file
# @return character(1L), except for \code{as.store_id_chr}, which returns FALSE
#   on failure

norm_store_id <- function(x) if(is.default_unitizer_id(x)) norm_file(x) else x

norm_file <- function(x) {
  if(
    !inherits(  # maybe this should just throw an error
      normed <- try(normalize_path(x, mustWork=TRUE), silent=TRUE),
      "try-error"
    )
  ) normed else x
}
as.store_id_chr <- function(x) {
  if(is.chr1plain(x)){
    return(relativize_path(x))
  }
  target <- try(as.character(x), silent=TRUE)
  if(inherits(target, "try-error"))
    stop(
      "Unable to convert store id to character; if you are using custom ",
      "store IDs be sure to define an `as.character` method for them"
    )
  target
}
# for testing only; needs to be in namespace

as.character.untz_stochrerr <- function(x, ...) stop("I am an error")

best_store_name <- function(store.id, test.file) {
  stopifnot(is.chr1plain(test.file))
  chr.store <- try(as.store_id_chr(store.id), silent=TRUE)
  if(!is.chr1plain(chr.store)) {
    if(is.na(test.file)) return("<untranslateable-unitizer-id>")
    return(
      paste0("unitizer for test file '", relativize_path(test.file), "'")
    )
  }
  chr.store
}
best_file_name <- function(store.id, test.file) {
  stopifnot(is.chr1plain(test.file))
  if(!is.na(test.file)) return(relativize_path(test.file))
  if(!is.chr1plain(chr.store <- as.store_id_chr(store.id))) {
    return("<unknown-test-file>")
  }
  paste0("Test file for unitizer '", chr.store, "'")
}
