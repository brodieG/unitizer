#' @include item.R
#' @include item.sub.R
#' @include unitizer.R

NULL

setGeneric("healEnvs", function(x, y,...) standardGeneric("healEnvs"))

#' Fixes The Ancestries of our New Reference Items Object
#'
#' This is necessary because when we let the user pick and chose which
#' tests to store and which ones to reject, there may no longer be a clear
#' ancestry chain within the remaining tests.
#'
#' The healing process is somewhat complex and full of compromises.  We
#' are attempting to create a self consistent set of nested parent environments
#' for each test, but at the same time, we don't want to store all the
#' combinations of reference and new objects.
#'
#' We only store new objects in `unitizer`, with the lone exception of
#' objects associated to a test environment.  These will include any assignments
#' that occur just prior to a test, as well as any objects created by the
#' actual test.
#'
#' There are two ways in which we modify the environment ancestry. If
#' the user decides to not store some new tests, then the objects created
#' in between the previous new stored test and the next new stored test are
#' all moved to the next new stored test, and the previous new stored test
#' becomes the parent of the next new stored test.
#'
#' The second way relates to when the user decides to keep a reference
#' test over a matching new test.  This is a lot more complicated because
#' we do not preserve the reference test environment ancestry.  Effectively,
#' we need to graft the reference test to the new environment ancestry.
#'
#' If a reference test that is being kept matches directly to a new test,
#' then the parent of that new test becomes the parent of the reference
#' test.
#'
#' If there is no direct match, but there are child reference tests that
#' match to a new item, then the parent is the youngest new test that
#' is older than the new test that was matched and is kept.  If no new
#' tests meet this criterion, then base.env is the parent.
#'
#' If there is no direct match, and there are no child reference tests
#' that are being kept that do match to a kept new item, then the parent
#' will be the last new test that is kept.
#'
#' The main takeaway from all this is that reference tests don't really
#' keep their evaluation environment.  Often this environment is similar
#' to the new environment.  When there are difference between the two,
#' the output of \code{`ls`} is customized to highlight
#' which objects were actually available/unmodifed at the time of the
#' reference test evaluation.  Object namees will have the following
#' symbols appended to explain the object status:
#' \itemize{
#'   \item ': object exists in browsing environment, but not the same as
#'      it was when test was evalaluated
#'   \item *: object was present during test evaluation but is not
#'      available in unitizer anymore
#'   \item **: object was not present during test evaluation, but exists
#'      in current environment
#' }
#'
#' @note This is an internal method and should not be used by package users; the
#'   documentation is exposed so that this aspect of \code{`unitizer`} is
#'   documented for package users
#'
#' @seealso \code{`\link{updateLs,unitizerItem-method}`}
#' @param x \code{`\link{unitizerItems-class}`} object
#' @param y \code{`\link{unitizer-class}`} object \code{`x`} was generated from
#' @param ... unused, here for inheriting methods
#' @return \code{`unitizerItems-class`}

setMethod("healEnvs", c("unitizerItems", "unitizer"), valueClass="unitizerItems",
  function(x, y, ...) {
    # Now need to reconstruct all the parenthood relationships between items,
    # start by figuring out the indices of all the new and reference items

    if(!is.environment(x@base.env)) stop("Slot `@base.env` must be defined for Argument `x`")
    items.new.idx <- vapply(as.list(x)[itemsType(x) == "new"], function(x) x@id, integer(1L))
    items.ref.idx <- vapply(as.list(x)[itemsType(x) == "reference"], function(x) x@id, integer(1L))

    # Now find gaps and assign to items prior to gap.  If missing first value,
    # then go to first env

    gap.order <- order(items.new.idx, decreasing=TRUE)
    gaps <- diff(c(items.new.idx[gap.order], 0L)) # note gaps are -ve and there is a gap if gap < -1

    for(i in seq_along(gaps)) {
      i.org <- gap.order[[i]]
      if(gaps[[i]] < -1L) {
        if(items.new.idx[[i.org]] + gaps[[i]] == 0L) {
          item.env <- y@items.new@base.env
        } else if (items.new.idx[[i.org]] + gaps[[i]] < 0) {
          stop("Logic Error, gap too low, contact maintainer.")
        } else {
          item.env <- y@items.new[[items.new.idx[[i.org]] + gaps[[i]]]]@env
        }
        # Any objects defined in gaps should be assigned to the new parent env, though
        # in theory there should be none unless user specifically assigned objects
        # during a test, which is not default behavior

        for(j in (gaps[[i]] + 1L):-1L) {
          interim.env <- y@items.new[[items.new.idx[[i.org]] + j]]@env  # assumes continuous ids in items.new
          interim.names <- ls(envir=interim.env, all.names=T)
          lapply(interim.names, function(z) assign(z, get(z, interim.env), x[itemsType(x) == "new"][[i.org]]@env))
        }
        # no need to run updateLs() athat is done on addition to unitizer
        if(identical(x[itemsType(x) == "new"][[i.org]]@env, item.env)) {
          # This should only happen when an ignored test just before an actual
          # and just after another ignored test is removed from the item list;
          # since both the ignored tests where assigned to the environment of the
          # subsequent test, the normal logic here would cause the test to have
          # it's parent env assigned to itself.  Here we had a unit test that
          # relied on this so we don't want to outright forbid it out of lazyness...

          warning(
            "Logic Problem: would have assigned circular environment reference ",
            "but over-rode that; this message should only show up in `unitizer` ",
            "development tests, if you see it please contact maintainer."
          )
        } else {
          parent.env(x[itemsType(x) == "new"][[i.org]]@env) <- item.env
        }
      }
    }
    # Now need to map reference item environment parents.  See function docs
    # for details on the logic here

    ref.order <- order(items.ref.idx)
    tail.env <- if(length(items.new.idx)) y@items.new[[max(items.new.idx)]]@env else x@base.env
    env.list <- list()
    slot.in <- integer(length(items.ref.idx))  # Note that `slot.in` values can be repeated

    for(i in ref.order) {
      # First find the youngest new test that is verifiably older than
      # our current reference

      if(
        !length(
          matching.new.younger <-
            Filter(Negate(is.na), tail(y@items.ref.map, -items.ref.idx[[i]]))
        )
      ) {
        # Nothing younger, so put at end and set up so that next one goes behind this one
        # need to look for non-kept items as well as otherwise could slot something in
        # too far down the ancestry.
        item.env <- tail.env
        slot.in[[i]] <- max(slot.in, items.new.idx) + 1L
      } else if (  # Nothing younger or older, note that diretly matching env doesn't count
        !length(
          matching.new.older <- (
            m.n.older.tmp <- sort(
              Filter(Negate(is.na), head(y@items.ref.map, items.ref.idx[[i]] - 1L))
          ) )[m.n.older.tmp %in% items.new.idx]
      ) ) {
        # also, in this case must look at kept envs only since parent has to be a kept
        # env
        item.env <- x@base.env
        slot.in[[i]] <- min(slot.in)
      } else {     # Something younger, and older
        # in this case, we must find the closest older kept new env, and use that as a
        # parent.  Old approach used to use the a kept ref env that is between this
        # new env and the ref env we are working with, but this became unmanageable in
        # term of computing the ls diffs.
        item.env <- y@items.new[[tail(matching.new.older, 1L)]]@env
        slot.in[[i]] <- tail(matching.new.older, 1L)
      }
      x[itemsType(x) == "reference"][[i]] <- updateLs(x[itemsType(x) == "reference"][[i]], y@base.env, item.env)
      env.list[[i]] <- item.env
    }
    # Now re-assign the environments; this has to be done after we run all the
    # lses as otherwise the ls diffs won't work since the whole point is they
    # compare the environment from before the re-assignment to the one after

    for(i in ref.order) parent.env(x[itemsType(x) == "reference"][[i]]@env) <- env.list[[i]]

    # Re-order items (basically, by the new items, and slot in the reference
    # ones as per the healing logic above)

    append(x[itemsType(x) == "new"], x[itemsType(x) == "reference"])[
      order(c(items.new.idx, slot.in))
    ]
} )

setGeneric("updateLs", function(x, ...) standardGeneric("updateLs"))

#' Compare The Objects In Environment for Ref vs. New Item
#'
#' Makes sure that when we call \code{`ls`} when browsing its environment
#' the information reflecting any limitations on what objects are actually
#' available to browse is properly reflected.
#'
#' The status of environment objects is tracked in \code{`x@@ls$status`},
#' where objects of different status are marked like so:
#' \itemize{
#'   \item ': object exists in browsing environment, but not the same as
#'      it was when test was evalaluated
#'   \item *: object was present during test evaluation but is not
#'      available in unitizer anymore
#'   \item **: object was not present during test evaluation, but exists
#'      in current environment
#' }
#' @keywords internal
#' @param x the \code{`\link{unitizerItem-class}`}
#' @param base.env the last environment to search through for objects
#' @return \code{`\link{unitizerItem-class}`} object with updated
#'   \code{`ls`} field and environment reference parent

setMethod("updateLs", "unitizerItem",
  function(x, base.env, new.par.env=NULL,  ...) {
    if(!is.null(new.par.env) && !is.environment(new.par.env)) stop("Argument `new.par.env` should be an environment or NULL.")
    if(!is.environment(base.env)) stop("Argument `base.env` should be an environment.")
    if(!x@reference) {    # should only happen with new items
      new.ls <- sort(run_ls(env=x@env, stop.env=base.env, all.names=TRUE))
      fin.ls <- if(length(new.ls))
      cbind(names=new.ls, status="") else
      matrix(character(), ncol=2, dimnames=list(NULL, c("names", "status")))
    } else {
      if(!is.environment(new.par.env)) stop("Argument `new.par.env` should be an environment when in reference mode.")

      #browser()
      ref.env.store <- new.env(parent=emptyenv())
      new.env.store <- new.env(parent=emptyenv())

      run_ls(env=x@env, stop.env=base.env, all.names=TRUE, store.env=ref.env.store)
      run_ls(env=new.par.env, stop.env=base.env, all.names=TRUE, store.env=new.env.store)
      # Since reference test keeps any objects defined in it's own environment, we can cheat
      # for comparison purposes by putting those objects in the "new" environment so they
      # look like they exist there
      lapply(ls(envir=x@env, all.names=TRUE), function(y) assign(y, get(y, x@env), new.env.store))
      ref.ls <- ls(envir=ref.env.store, all.names=TRUE)

      if(nrow(x@ls)) {
        org.ls <- x@ls$names
        org.ref <- x@ls$status == ""
      } else {
        org.ls <- ref.ls
        org.ref <- rep(TRUE, length(ref.ls))
      }
      names(org.ref) <- org.ls
      new.ls <- ls(envir=new.env.store, all.names=TRUE)
      ref.new <- vapply(ref.ls[ref.ls %in% new.ls], function(x) identical(ref.env.store[[x]], new.env.store[[x]]), logical(1L))

      # Equal b/w original, reference, and new

      fin.ls <- matrix(character(), ncol=2)
      if(
        length(
          ls.match <-
            names(org.ref)[
              !is.na(org.ref & (ref.new.match <- ref.new[match(names(org.ref), names(ref.new))])) & ref.new.match
      ] ) ) fin.ls <- cbind(ls.match, "")

      # Exist but modified

      org.new.ls <- intersect(org.ls, new.ls)
      if(length(ls.match <- org.new.ls[!(org.new.ls %in% fin.ls[, 1])])) fin.ls <- rbind(fin.ls, cbind(ls.match, "'"))

      # Disappeared objects, and new objects

      if(length(setdiff(org.ls, new.ls))) fin.ls <- rbind(fin.ls, cbind(setdiff(org.ls, new.ls), "*"))
      if(length(setdiff(new.ls, org.ls))) fin.ls <- rbind(fin.ls, cbind(setdiff(new.ls, org.ls), "**"))
    }
    # Update item and return

    fin.ls <- as.data.frame(fin.ls, stringsAsFactors=FALSE)
    names(fin.ls) <- names(x@ls)
    attr(fin.ls, "reference") <- x@reference
    x@ls <- fin.ls[order(fin.ls$names), ]
    x
} )
