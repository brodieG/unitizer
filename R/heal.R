#' @include item.R
#' @include item.sub.R
#' @include unitizer.R

NULL

setGeneric("healEnvs", function(x, y,...) standardGeneric("healEnvs"))

#' Fix Environment Ancestries
#'
#' This is an internal method and exposed so that this aspect of \code{unitizer}
#' is documented for package users (see Details).
#'
#' Environment healing is necessary because when we let the user pick and chose
#' which tests to store and which ones to reject, there may no longer be a clear
#' ancestry chain within the remaining tests.
#'
#' The healing process is somewhat complex and full of compromises.  We
#' are attempting to create a self consistent set of nested parent environments
#' for each test, but at the same time, we don't want to store all the
#' combinations of reference and new objects.
#'
#' We only store new objects in \code{unitizer}, with the lone exception of
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
#' the output of \code{ls} is customized to highlight
#' which objects were actually available/unmodifed at the time of the
#' reference test evaluation.  Object names will have the following
#' symbols appended to explain the object status:
#' \itemize{
#'   \item ': object exists in browsing environment, but not the same as
#'      it was when test was evalaluated
#'   \item *: object was present during test evaluation but is not
#'      available in unitizer anymore
#'   \item **: object was not present during test evaluation, but exists
#'      in current environment
#' }
#' @note Could be more robust by ensuring that items in \code{x} actually do
#'   come from \code{y}. This is particularly important since when
#'   we re-assemble the final list, we don't actually use x at all.  Signature
#'   for this should probably ultimately change to be something like
#'   \code{c("unitizer", "x")} where x is just a data frame with column 1
#'   the item index, and column 2 whether it originated from "new" or "ref"
#'
#' @aliases healEnvs,unitizerItems,unitizer-method
#' @seealso \code{updateLs,unitizerItem-method}
#' @param x \code{unitizerItems} object
#' @param y \code{unitizer} object \code{x} was generated from
#' @param ... unused, here for inheriting methods
#' @return \code{unitizerItems}
#' @export
#' @name healEnvs
#' @rdname healEnvs

setMethod("healEnvs", c("unitizerItems", "unitizer"),
  valueClass="unitizerItems",
  function(x, y, ...) {
    # Now need to reconstruct all the parenthood relationships between items,
    # start by figuring out the indices of all the new and reference items

    if(!is.environment(x@base.env))
      stop("Slot `@base.env` must be defined for Argument `x`")

    items.new.select <- itemsType(x) == "new" & !ignored(x)
    items.ref.select <- itemsType(x) == "reference" & !ignored(x)
    items.idx <- vapply(as.list(x), function(x) x@id, integer(1L))
    items.new.idx <- items.idx[items.new.select]
    items.ref.idx <- items.idx[items.ref.select]

    # Make sure that our items have a reasonable base environment, though keep
    # in mind we ultimately use the items pulled from `y`, and are relying on
    # the fact that environments are reference objects so that changing their
    # parents from `x` also affects the parents in `y`.  See note in fun docs.
    # IMPORTANT COROLLARY: the only thing that should be changed in `x` is the
    # environment parents; any other changes will be lost since we don't
    # use `x` for anything other than that.

    parent.env(x@base.env) <- y@base.env

    # Reconstitute environment chain for new tests.  Find gaps and assign to
    # items prior to gap.  If missing first value, then go to first env.  Note
    # we're cycling in the opposite order the environments are going in.  Also,
    # ignored tests are considered gaps as they don't have their own environment

    gap.order <- order(items.new.idx, decreasing=TRUE)
    # note gaps are -ve and there is a gap if gap < -1
    gaps <- diff(c(items.new.idx[gap.order], 0L))

    for(i in seq_along(gaps)) {
      i.org <- gap.order[[i]]
      if(gaps[[i]] < -1L) {
        if(items.new.idx[[i.org]] + gaps[[i]] == 0L) {
          item.env <- x@base.env
        } else if (items.new.idx[[i.org]] + gaps[[i]] < 0) {
          # nocov start
          stop("Logic Error, gap too low, contact maintainer.")
          # nocov end
        } else {
          item.env <- y@items.new[[items.new.idx[[i.org]] + gaps[[i]]]]@env
        }
        # Any objects defined in gaps should be assigned to the new parent env,
        # though in theory there should be none unless user specifically
        # assigned objects during a test, which is not default behavior

        for(j in (gaps[[i]] + 1L):-1L) {
          # assumes continuous ids in items.new
          interim.env <- y@items.new[[items.new.idx[[i.org]] + j]]@env
          interim.names <- ls(envir=interim.env, all.names=T)
          lapply(
            interim.names,
            function(z)
              assign(z, get(z, interim.env), x[items.new.select][[i.org]]@env)
        ) }
        # no need to run updateLs() as that is done on addition to unitizer
        if(identical(x[items.new.select][[i.org]]@env, item.env)) {
          # This should only happen when an ignored test just before an actual
          # and just after another ignored test is removed from the item list;
          # since both the ignored tests where assigned to the environment of
          # the subsequent test, the normal logic here would cause the test to
          # have it's parent env assigned to itself.  Here we had a unit test
          # that relied on this so we don't want to outright forbid it out of
          # lazyness...

          # nocov start
          warning(
            "Logic Problem: would have assigned circular environment ",
            "reference but over-rode that; this message should only show up ",
            "in `unitizer` development tests, if you see it please contact ",
            " maintainer."
          )
          # nocov end
        } else {
          parent.env(x[items.new.select][[i.org]]@env) <- item.env
        }
      }
      # Any items that have for parent env the base env of the new items needs
      # to be re-assigned to the base env of the the item list we're processing

      if(
        identical(
          parent.env(x[items.new.select][[i.org]]@env),
          y@items.new@base.env
      ) ) {
        parent.env(x[items.new.select][[i.org]]@env) <- x@base.env
      }
    }
    # Now need to map reference item environment parents.  See function docs
    # for details on the logic here

    ref.order <- order(items.ref.idx)
    tail.env <- if(length(items.new.idx)) {
      y@items.new[[max(items.new.idx)]]@env
    } else x@base.env
    env.list <- list()
    # Note that `slot.in` values can be repeated
    slot.in <- integer(length(items.ref.idx))
    repair <- FALSE
    for(i in ref.order) {
      # First find the youngest new test that is verifiably older than
      # our current reference

      if(
        !length(
          matching.new.younger <-
            Filter(Negate(is.na), tail(y@items.ref.map, -items.ref.idx[[i]]))
        )
      ) {
        # Nothing younger, so put at end and set up so that next one goes behind
        # this one need to look for non-kept items as well as otherwise could
        # slot something in too far down the ancestry.
        item.env <- tail.env
        slot.in[[i]] <- max(slot.in, items.new.idx) + 1L
      } else if (
        # Nothing younger or older, note that diretly matching env doesn't count
        !length(
          matching.new.older <- (
            m.n.older.tmp <- sort(
              Filter(
                Negate(is.na), head(y@items.ref.map, items.ref.idx[[i]] - 1L)
          ) ) )[m.n.older.tmp %in% items.new.idx]
      ) ) {
        # also, in this case must look at kept envs only since parent has to be
        # a kept env
        item.env <- y@items.new@base.env
        slot.in[[i]] <- min(slot.in)
      } else {     # Something younger, and older
        # in this case, we must find the closest older kept new env, and use
        # that as a parent.  Old approach used to use the a kept ref env that is
        # between this new env and the ref env we are working with, but this
        # became unmanageable in term of computing the ls diffs.
        item.env <- y@items.new[[tail(matching.new.older, 1L)]]@env
        slot.in[[i]] <- tail(matching.new.older, 1L)
      }
      # once we need to repair, stop doing this otherwise get spammed with
      # errors

      if(!repair) {
        item.ref.updated <- try(
          updateLs(
            x[items.ref.select][[i]], y@base.env, item.env
        ) )
      }
      if(inherits(item.ref.updated, "try-error")) {
        # nocov start
        stop(
          "Logic Error: item environment history corrupted in unknown way; ",
          "contact maintainer.  You can attempt to recover your `unitizer` by ",
          "using `repair_envs`."
        )
        # nocov end
      } else if (identical(item.ref.updated, FALSE)) {
        # Corrupted env history, will have to repair
        repair <- TRUE
        item.ref.updated <- x[items.ref.select][[i]]
      }
      # Update object and record environment

      y@items.ref[[items.ref.idx[[i]]]] <- item.ref.updated
      env.list[[i]] <- item.env
    }
    # Now re-assign the environments; this has to be done after we run all the
    # lses as otherwise the ls diffs won't work since the whole point is they
    # compare the environment from before the re-assignment to the one after
    #
    # Remember, this modifies parent envs for y@items.ref as well!

    for(i in ref.order)
      parent.env(x[items.ref.select][[i]]@env) <- env.list[[i]]

    # Now re-introduce ignored tests; first figure out what actual test the
    # ignored tests map to.  Note that the logic below means that any ignored
    # tests that don't have any subsequent real tests just get dropped

    ig_assign <- function(items) {
      if(!length(items)) return(integer())
      ave(       # for each ignored, get id of first non-ignored
        1:length(items),
        c(0L, head(cumsum(!ignored(items)), -1L)),
        FUN=max
      )
    }
    # for each ignored, get id of first non-ignored

    new.ig.assign <- ig_assign(y@items.new)
    ref.ig.assign <- ig_assign(y@items.ref)

    # nocov start
    if(
      any(!items.new.idx %in% new.ig.assign) ||
      any(!items.ref.idx %in% ref.ig.assign)
    )
      stop(
        "Logic Error: error re-assigning ignored items to actual tests; ",
        "contact maintainer"
      )
    # nocov end

    # For each selected test, add back the ignored ones; for new ones this is
    # easy because we know they are all in the right order already in
    # y@items.new

    items.new.final <- y@items.new[new.ig.assign %in% items.new.idx]

    # For reference items, need to assign the ignored tests to the correct
    # section since the ignored ones are not pulled from the processed item
    # list, but rather from the original unitizer that hasn't had reference
    # items with meaningless section ids quashed in `processInput`

    ref.sects <- vapply(
      as.list(y@items.ref[ref.ig.assign]), slot, 1L, "section.id"
    )
    for(i in seq_along(ref.ig.assign))
      y@items.ref[[i]]@section.id <- ref.sects[[i]]

    # Refs a bit more complicated since we need to find the correct slot-in
    # spot; slot.in has the correct slot for each item in items.ref.idx, in the
    # order of items.ref.idx

    items.ref.final <- y@items.ref[ref.ig.assign %in% items.ref.idx]

    # Now need everything order as in y@items.new, and then for the `ref` values
    # as per `slot.in`

    items.final <- append(items.new.final, items.ref.final)[
      order(
        c(
          new.ig.assign[new.ig.assign %in% items.new.idx],
          slot.in[Filter(Negate(is.na), match(ref.ig.assign, items.ref.idx))]
    ) ) ]
    # The LSes for reference items are not meaningful so should be invalidated

    items.final[ignored(items.final)] <-
      invalidateLs(items.final[ignored(items.final)])

    # If environments need repairing, do so now (note this means ignored items
    # will get their own env?? Need to check / fix)

    if(repair) {
      items.final <- try(repairEnvs(items.final))
      # nocov start
      if(inherits(x, "try-error")) {
        stop(
          "Logic Error: unable to repair reference test environments; contact ",
          "maintainer."
      ) }
      # nocov end
    }
    items.final
} )
setGeneric("updateLs", function(x, ...) standardGeneric("updateLs"))

# Compare The Objects In Environment for Ref vs. New Item
#
# Makes sure that when we call \code{ls} when browsing its environment
# the information reflecting any limitations on what objects are actually
# available to browse is properly reflected.
#
# The status of environment objects is tracked in \code{x@@ls$status},
# where objects of different status are marked like so:
# \itemize{
#   \item ': object exists in browsing environment, but not the same as
#      it was when test was evalaluated
#   \item *: object was present during test evaluation but is not
#      available in unitizer anymore
#   \item **: object was not present during test evaluation, but exists
#      in current environment
# }
#
# This could definitely be optimized for new items.  It actually represents
# a substantial portion of total evaluation time and does a lot of repetitive
# stuff that could easily be avoided if we put some work into it.
#
# @keywords internal
# @param x the \code{\link{unitizerItem-class}}
# @param base.env the last environment to search through for objects
# @return \code{\link{unitizerItem-class}} object with updated
#   \code{ls} field and environment reference parent, or FALSE if the item
#   has a corrupted environment history

setMethod("updateLs", "unitizerItem",
  function(x, base.env, new.par.env=NULL,  ...) {
    if(!is.null(new.par.env) && !is.environment(new.par.env))
      stop("Argument `new.par.env` should be an environment or NULL.")
    if(!is.environment(base.env))
      stop("Argument `base.env` should be an environment.")
    if(!x@reference) {    # should only happen with new items
      new.ls <- sort(run_ls(env=x@env, stop.env=base.env, all.names=TRUE))
      fin.ls <- if(length(new.ls))
      cbind(names=new.ls, status="") else
      matrix(character(), ncol=2, dimnames=list(NULL, c("names", "status")))
    } else {
      if(!is.environment(new.par.env))
        stop(
          "Argument `new.par.env` should be an environment when in ",
          "reference mode."
        )

      ref.env.store <- new.env(parent=emptyenv())
      new.env.store <- new.env(parent=emptyenv())

      item.ls <- try(
        run_ls(
          env=x@env, stop.env=base.env, all.names=TRUE, store.env=ref.env.store
      ) )
      if(inherits(item.ls, "try-error")) return(FALSE)

      run_ls(
        env=new.par.env, stop.env=base.env, all.names=TRUE,
        store.env=new.env.store
      )

      # Since reference test keeps any objects defined in its own environment,
      # we can cheat for comparison purposes by putting those objects in the
      # "new" environment so they look like they exist there

      lapply(
        ls(envir=x@env, all.names=TRUE),
        function(y) assign(y, get(y, x@env), new.env.store)
      )
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
      ref.new <- vapply(
        ref.ls[ref.ls %in% new.ls],
        function(x) identical(ref.env.store[[x]], new.env.store[[x]]),
        logical(1L)
      )
      # Equal b/w original, reference, and new

      fin.ls <- matrix(character(), ncol=2)
      if(
        length(
          ls.match <-
            names(org.ref)[
              !is.na(
                org.ref &
                (
                  ref.new.match <-
                  ref.new[match(names(org.ref), names(ref.new))]
                )
              ) &
              ref.new.match
      ] ) ) fin.ls <- cbind(ls.match, "")

      # Exist but modified

      org.new.ls <- intersect(org.ls, new.ls)
      if(length(ls.match <- org.new.ls[!(org.new.ls %in% fin.ls[, 1])])) {
        fin.ls <- rbind(fin.ls, cbind(ls.match, "'"))
      }
      # Disappeared objects, and new objects

      if(length(setdiff(org.ls, new.ls)))
        fin.ls <- rbind(fin.ls, cbind(setdiff(org.ls, new.ls), "*"))
      if(length(setdiff(new.ls, org.ls)))
        fin.ls <- rbind(fin.ls, cbind(setdiff(new.ls, org.ls), "**"))
    }
    # Update item and return

    fin.ls <- as.data.frame(fin.ls, stringsAsFactors=FALSE)
    names(fin.ls) <- names(x@ls)
    attr(fin.ls, "reference") <- x@reference
    x@ls <- fin.ls[order(fin.ls$names), ]
    x
} )
