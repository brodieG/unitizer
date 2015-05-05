#' Store Retrieve Unitizer
#'
#' If this errors, calling function should abort
#'
#' @keywords internal
#' @param unitizer a \code{\link{unitizer}} object
#' @param store.id anything for which there is a defined \code{`\link{get_unitizer}`}
#'   method; by default should be the path to a unitizer; if \code{`\link{get_unitizer}`}
#'   returns \code{`FALSE`} then this will create a new unitizer
#' @param par.frame the environment to use as the parent frame for the \code{`unitizer`}
#' @param test.file the R file associated with the store id
#' @return a \code{`unitizer`} object, or anything, in which case the calling
#'   code should exit

load_unitizers <- function(
  store.ids, test.files, par.frame, interactive.mode, mode
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

  chr.ids <- vapply(store.ids, as.store_id_chr, character(1L))
  chr.files <- ifelse(
    is.na(test.files), paste0("test file for ", chr.ids),
    relativize_path(test.files)
  )
  # Get RDSs and run basic checks; `valid` will contain character strings
  # describing failures, or 0 length string if succeeded

  unitizers <- lapply(
    store.ids,
    function(x) {
      if(is(x, "unitizer")) {
        return(x)
      }
      x <- try(get_unitizer(x))
      if(inherits(x, "try-error"))
        return(
          paste0(
            c("`get_unitizer` error: ", conditionMessage(attr(x, "condition"))),
            collapse=""
        ) )
      if(is(x, "unitizer")) return(x)
      if(identical(x, FALSE)) {
        return(
          new(
            "unitizer", id=norm_store_id(x),
            zero.env=new.env(parent=par.frame),
            test.file.loc=norm_file(test.file)
      ) ) }
      return(
        "`get_unitizer` returned something other than a `unitizer` or FALSE"
  ) } )
  valid <- vapply(
    unitizers,
    function(x) {
      if(!is(x, "unitizer")) {
        if(!is.chr1plain(x) || nchar(x) < 1L)
          return("unknown `unitizer` load failure")
        return(x)
      }
      attempt <- try(validObject(x, complete=TRUE), silent=TRUE)
      if(inherits(attempt, "try-error")) {
        conditionMessage(attr(attempt, "condition"))
      } else ""
    },
    character(1L)
  )
  null.version <- package_version("0.0.0")
  curr.version <- packageVersion("unitizer")

  # unitizers without a `version` slot or slot in incorrect form not  eligible
  # for upgrade

  versions  <- lapply(
    unitizers,
    function(x)
      if(!is(x, "unitizer") || !is.package_version(x@version)) null.version else
      x@version
  )
  version.out.of.date <- vapply(
    versions, function(x) !identical(x, null.version) && curr.version > x,
    logical(1L)
  )
  valid.idx <- which(!nchar(valid))
  invalid.idx <- which(nchar(valid) & !version.out.of.date)
  toup.idx <- which(!nchar(valid) & version.out.of.date)
  toup.fail.idx <- integer(0L)

  # Attempt to resolve failures by upgrading if relevant

  if(length(toup.idx)) {
    many <- length(toup.idx) > 1L
    word_cat(
      "\nThe following `unitizer`", if(many) "s", " must be upgraded",
      if(!interactive.mode)
        paste0(
          ", but that can only be done in interactive mode, so we will ",
          "proceed without testing them"
        ),
      ":",
      sep=""
    )
    cat(as.character(UL(chr.files[toup.idx])), sep="\n")
    word_msg("`unitizer` upgrades are IRREVERSIBLE.  Proceed?")
    pick <- unitizer_prompt("Upgrade unitizer stores?", hist.con=NULL)
    if(identical(pick, "Y")) {
      upgraded <- lapply(unitizers[toup.idx], upgrade)
      upgrade.success <- vapply(upgraded, is, logical(1L), "unitizer")
      unitizers[toup.idx[upgrade.success]] <- upgraded[upgrade.success]
      valid.idx <- c(valid.idx, toup.idx[upgrade.success])

      toup.fail.idx <- toup.idx[!upgrade.success]
    } else {
      word_msg("`unitizer`(s) listed above will not be tested")
      toup.fail.idx <- toup.idx
      valid[toup.fail.idx] <- "`unitizer` upgrade failed"
    }
  }
  # Cleanup the unitizers

  for(i in valid.idx) {
    parent.env(unitizers[[i]]@zero.env) <- par.frame
    unitizers[[i]]@id <- norm_store_id(store.ids[[i]])
    unitizers[[i]]@test.file.loc <- norm_file(test.files[[i]])
    unitizers[[i]]@eval <- identical(mode, "unitize") #awkward, shouldn't be done this way
  }
  unitizers[!seq(unitizers) %in% valid.idx] <- FALSE

  # Issue errors as required

  if(length(invalid.idx)) {
    word_msg(
      "\nThe following `unitizer`", if(length(invalid.idx) > 1L) "s",
      " could not be loaded:", sep=""
    )
    cat(
      as.character(
        UL(paste0(chr.files[invalid.idx], ": ",  valid[valid.idx]))
      ),
      sep="\n", file=stderr()
    )
  }
  if(length(toup.fail.idx)) {
    word_msg(
      "\nThe following `unitizer`", if(length(toup.fail.idx) > 1L) "s",
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
    word_cat("No valid `unitizer`", if(length(store.ids) > 1L) "s", "to load")
  new("unitizerObjectList", .items=unitizers)
}

#' @keywords internal
#' @rdname load_unitizer

store_unitizer <- function(unitizer) {
  if(!is(unitizer, "unitizer")) return(invisible(TRUE))

  old.par.env <- parent.env(unitizer@zero.env)
  on.exit(parent.env(unitizer@zero.env) <- old.par.env)
  parent.env(unitizer@zero.env) <- baseenv()
  success <- try(set_unitizer(unitizer@id, unitizer))

  if(!inherits(success, "try-error")) {
    message("unitizer updated")
  } else {
    stop("Error attempting to save `unitizer`, see previous messages.")
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
      normed <- try(normalizePath(store.id, mustWork=TRUE), silent=TRUE),
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
  if(inherits(target, "try-error")) {
    return("<untranslateable-unitizer-id>")
  }
  target
}


