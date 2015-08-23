#' Store Retrieve Unitizer
#'
#' If this errors, calling function should abort
#'
#' @keywords internal
#' @param unitizer a \code{\link{unitizer-class}} object
#' @param store.id anything for which there is a defined \code{`\link{get_unitizer}`}
#'   method; by default should be the path to a unitizer; if \code{`\link{get_unitizer}`}
#'   returns \code{`FALSE`} then this will create a new unitizer
#' @param par.frame the environment to use as the parent frame for the \code{unitizer}
#' @param test.file the R file associated with the store id
#' @param force.upgrade whether to allow upgrades in non-interactive mode, for
#'   testing purposes
#' @param global the global tracking object
#' @return a \code{unitizer} object, or anything, in which case the calling
#'   code should exit

load_unitizers <- function(
  store.ids, test.files, par.frame, interactive.mode, mode, force.upgrade=FALSE,
  global=unitizerGlobal$new()
) {
  if(!is.character(test.files))
    stop("Argument `test.files` must be character")
  if(!is.environment(par.frame))
    stop("Argumetn `par.frame` must be an environment")
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
            test.file.loc=norm_file(test.files[[x]]),
            cons=NULL
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
  toup.idx <- which(nchar(valid) & version.out.of.date)
  toup.fail.idx <- integer(0L)

  # Attempt to resolve failures by upgrading if relevant

  if(length(toup.idx)) {
    many <- length(toup.idx) > 1L
    word_cat(
      "\nThe following unitizer", if(many) "s",
      if(force.upgrade) " will" else " must", " be upgraded to version '",
      as.character(curr.version), "':",
      sep=""
    )
    cat(
      as.character(
        UL(
          paste0(
            chr.ids[toup.idx], " (at '",
            vapply(versions[toup.idx], as.character, character(1L))
            , "')"
      ) ) ),
      sep="\n"
    )
    if(!interactive.mode && !force.upgrade)
      stop("Cannot upgrade unitizers in non-interactive mode")

    pick <- if(interactive.mode) {
      word_msg("unitizer upgrades are IRREVERSIBLE.  Proceed?")
      unitizer_prompt(
        "Upgrade unitizer stores?", hist.con=NULL,
        valid.opts=c(Y="[Y]es", N="[N]o")
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
      word_msg("unitizer(s) listed above will not be tested")
      toup.fail.idx <- toup.idx
      valid[toup.fail.idx] <- "User elected not to upgrade unitizers"
    }
  }
  # Cleanup the unitizers

  for(i in valid.idx) {
    unitizers[[i]]@id <- norm_store_id(store.ids[[i]])
    unitizers[[i]]@test.file.loc <- norm_file(test.files[[i]])
    parent.env(unitizers[[i]]@zero.env) <- par.frame
    unitizers[[i]]@global <- global
    unitizers[[i]]@eval <- identical(mode, "unitize") #awkward, shouldn't be done this way
  }
  unitizers[!seq(unitizers) %in% valid.idx] <- FALSE

  # Issue errors as required

  if(length(invalid.idx)) {
    word_msg(
      "\nThe following unitizer", if(length(invalid.idx) > 1L) "s",
      " could not be loaded:", sep=""
    )
    cat(
      as.character(
        UL(paste0(chr.ids[invalid.idx], ": ",  valid[invalid.idx]))
      ),
      sep="\n", file=stderr()
    )
  }
  if(length(toup.fail.idx)) {
    word_msg(
      "\nThe following unitizer", if(length(toup.fail.idx) > 1L) "s",
      " could not be upgraded to version '", as.character(curr.version), "':",
      sep=""
    )
    cat(
      as.character(
        UL(
          paste0(
            chr.files[toup.fail.idx], " at '",
            vapply(versions[toup.fail.idx], as.character, character(1L)),
            "': ", valid[toup.fail.idx]
      ) ) ),
      sep="\n", file=stderr()
    )
  }
  if(!length(valid.idx) && (length(invalid.idx) || length(toup.fail.idx)))
    word_cat(
      "No valid unitizer", if(length(store.ids) > 1L) "s", " to load", sep=""
    )
  new("unitizerObjectList", .items=unitizers)
}

#' Need to make sure we do not unintentionally store a bunch of references to
#' objects or namespaces we do not want:
#'
#' \itemize{
#'   \item reset parent env to be base
#'   \item remove all contents of base.env (otherwise we get functions with
#'     environments that reference namespaces)
#' }
#'
#' @keywords internal
#' @rdname load_unitizers

store_unitizer <- function(unitizer) {
  if(!is(unitizer, "unitizer")) return(invisible(TRUE))

  old.par.env <- parent.env(unitizer@zero.env)
  on.exit(parent.env(unitizer@zero.env) <- old.par.env)
  parent.env(unitizer@zero.env) <- baseenv()
  unitizer@global <- NULL  # to avoid taking up a bunch of storage on large object

  # zero out connections we'v been using

  if(!is.null(unitizer@cons)) {
    close_and_clear(unitizer@cons)
    unitizer@cons <- NULL
  }
  rm(list=ls(unitizer@base.env, all=TRUE), envir=unitizer@base.env)

  # blow away calls; these should be memorialized as deparsed versions and the
  # original ones take up a lot of room to store

  for(i in seq_along(unitizer@items.ref)) unitizer@items.ref[[i]]@call <- NULL
  for(i in seq_along(unitizer@items.new)) unitizer@items.new[[i]]@call <- NULL  # shouldn't really be anything here

  success <- try(set_unitizer(unitizer@id, unitizer))

  if(!inherits(success, "try-error")) {
    message("unitizer updated")
  } else {
    stop("Error attempting to save unitizer, see previous messages.")
  }
  return(invisible(TRUE))
}

#' Get A Store ID in Full Path Format
#'
#' Loosely related to \code{getTarget,unitizer-method} and
#' \code{getName,unitizer-method} although these are not trying to convert to
#' character or check anything, just trying to normalize if possible.
#'
#' Relevant for default ids
#' @keywords internal

norm_store_id <- function(x) if(is.default_unitizer_id(x)) norm_file(x) else x

#' @rdname norm_store_id
#' @keywords internal

norm_file <- function(x) {
  if(
    !inherits(  # maybe this should just throw an error
      normed <- try(normalizePath(x, mustWork=TRUE), silent=TRUE),
      "try-error"
    )
  ) normed else x
}

#' Convert Store ID to Character
#'
#' For display purposes only since path is relativized.
#'
#' If not possible make up a name
#'
#' @keywords internal

as.store_id_chr <- function(x) {
  if(is.chr1plain(x)){
    return(relativize_path(x))
  }
  target <- try(as.character(x), silent=TRUE)
  if(inherits(target, "try-error")) return(FALSE)
  target
}
#' Get Most Intuitive Name for Store
#'
#' Based on data from \code{store.id} and \code{test.file}
#'
#' @param store.id a \code{unitizer} store id
#' @param test.file the location of the R test file
#' @return character(1L)

best_store_name <- function(store.id, test.file) {
  stopifnot(is.chr1plain(test.file))
  if(!is.chr1plain(chr.store <- as.store_id_chr(store.id))) {
    if(is.na(test.file)) return("<untranslateable-unitizer-id>")
    return(
      paste0("unitizer for test file '", relativize_path(test.file), "'")
    )
  }
  chr.store
}
#' @keywords internal
#' @rdname best_store_name

best_file_name <- function(store.id, test.file) {
  stopifnot(is.chr1plain(test.file))
  if(!is.na(test.file)) return(relativize_path(test.file))
  if(!is.chr1plain(chr.store <- as.store_id_chr(store.id))) {
    return("<unknown-test-file>")
  }
  paste0("Test file for unitizer '", chr.store, "'")
}
#' Helper function for load
#'
#' @keywords internal

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
