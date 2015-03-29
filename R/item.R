# - Required Code -------------------------------------------------------------

#' @include conditions.R
#' @include class_unions.R
#' @include list.R

NULL

# - Validity ------------------------------------------------------------------

#' Used to run "validity" methods
#'
#' This is to avoid having to automatically run validity on object
#' instantiation as is the case with actual built-in validity objects.  We can
#' create a bunch of objects, and then only run validity when

setGeneric("isValid", function(object, ...) standardGeneric("isValid"))

# - Classes -------------------------------------------------------------------

#' Data Produced During Evaluation of \code{`unitizer`} Test
#'
#' Kept separate from the \code{`\link{unitizerItem-class}`} because these
#' are the slots that get compared from the new item to the reference items
#' which means there are a whole bunch of other classes that need to have the
#' same structure as this and by definining it we let those other classes
#' confirm they have the correct structure.
#'
#' @keywords internal

setClass(
  "unitizerItemData",
  representation(
    value="ANY",
    conditions="conditionList",
    output="character",
    message="character",
    aborted="logical"      # more generally, should this be a withRestarts slot?
) )
#' Full Representation Of an Evaluated \code{`unitizer`} Test
#'
#' @keywords internal
#' @slot call the call that is tested
#' @slot reference whether this is a reference or new \code{`unitizerItem`}
#' @slot ignore whether this test should be treated as a test or just a step
#'   towards compiling a test
#' @slot environment the environment the test was evaluated in, should contain
#'   all relevant objects
#' @slot data the container for the results of the evaluation of \code{`call`}
#' @slot the \code{`unitizer_sect`} from the newly added items this test item
#'   corresponds to; can be NA if section not known; this is used primarily to
#'   keep track of original sections when storing reference tests.

setClass(
  "unitizerItem",
  representation(
    call="ANY",
    id="integer",
    reference="logical",
    env="environmentOrNULL",
    ignore="logical",
    ls="data.frame",
    comment="characterOrNULL",
    trace="list",
    data="unitizerItemData",
    section.id="integer"
  ),
  prototype(
    reference=FALSE, ignore=FALSE, id=1L,
    ls=data.frame(names=character(), status=character()),
    section.id=NA_integer_
) )
unitizerItemSlotNames <- slotNames("unitizerItem")  # cache names

setMethod("isValid", "unitizerItem",
  function(object) {
    if(!identical(length(object@reference), 1L))
      return("Slot `@reference` must be length 1")
    if(!identical(length(object@id), 1L) || !isTRUE(object@id > 0))
      return("Slot `@id` must be length 1 and greater than zero.")
    if(!identical(names(object@ls), c("names", "status")) ||
      !identical(unname(vapply(object@ls, class, "")), rep("character", 2L))) {
      return("Slot `@ls` has incorrect data structure")
    }
    if(length(object@section.id) != 1L || object@section.id < 1L)
      return("Slot `@section.id` must be integer(1L) >= 1L")
    TRUE
} )
setClassUnion("unitizerItemOrNULL", c("unitizerItem", "NULL"))

#' Initialize A \code{`\link{unitizerItem-class}`}
#'
#' Makes the fact that most of the data needs to be part of a
#' \code{`\link{unitizerItemData-class}`} object transparent to the user.
#'
#' @keywords internal

setMethod("initialize", "unitizerItem", function(.Object, ...) {
  dots.all <- list(...)
  dots.names <- names(dots.all)
  if(!("call" %in% dots.names)) .Object@call <- NULL else .Object@call <- dots.all$call
  if("env" %in% dots.names) .Object@env <- dots.all$env
  if(
    is.call(.Object@call) &&
    !inherits(
      try(fun <- eval(.Object@call[[1L]], .Object@env), silent=TRUE), "try-error"
    ) &&
    any(vapply(funs.ignore, identical_fun, logical(1L), fun))
  ) {
    .Object@ignore <- TRUE
  }
  if("comment" %in% dots.names) .Object@comment <- dots.all$comment
  if("trace" %in% dots.names) .Object@trace <- dots.all$trace
  dots <- dots.all[!(dots.names %in% unitizerItemSlotNames)]
  .Object@data <- do.call("new", c(list("unitizerItemData"), dots), quote=TRUE)
  .Object
} )
#' Collection of \code{`\link{unitizerItem-class}`} Objects
#'
#' @keywords internal

setClass("unitizerItems", contains="unitizerList",
  representation(
    base.env="environment"    # should be enclosure of first item
  ),
  validity=function(object) {
    if(!all(vapply(object@.items, is, logical(1L), "unitizerItem")))
      return("slot `items` may only contain objects \"unitizerItem\"")
    TRUE
} )
setClassUnion("unitizerItemsOrNULL", c("unitizerItems", "NULL"))

# - Single Object Methods -----------------------------------------------------

#' Display a \code{`\link{unitizerItem-class}`} Object
#'
#' Highly summarized view of the unitizer object.
#'
#' @keywords internal

setMethod("show", "unitizerItem",
  function(object) {
    cat("~~~ ")
    if(object@reference) cat("Reference") else cat("New")
    cat(" Test ~~~\n")
    cat("* value:", paste0(desc(object@data@value, limit=getOption("width") - 7L), "\n"))
    if(out.len <- length(object@data@output)) cat("* output:", out.len, "lines\n")
    if(err.len <- length(object@data@message)) cat("* message:", err.len, "lines\n")
    if(cond.len <- length(object@data@conditions)) {
      cond.types <- vapply(
        as.list(object@data@conditions),
        function(x) {
          if(inherits(x, "error")) {
            "error"
          } else if (inherits(x, "warning")) {
            "warning"
          } else if (inherits(x, "message")) {
            "message"
          } else {
            "other"
        } },
        character(1L)
      )
      cond.types.summ <- Filter(
        Negate(is.na),
        tapply(
          cond.types,
          factor(
            cond.types,
            levels=c("error", "warning", "message", "other condition")),
          length
      ) )
      word_cat(
        "* conditions:",
        paste0(
          cond.types.summ, " ",
          paste0(
            names(cond.types.summ),
            ifelse(cond.types.summ > 1L, "s", "")
          ), collapse=", "
      ) )
    }
    word_cat(
      "Access component `x` with",
      paste0("`", if(object@reference) ".REF" else ".NEW", "$x`;"),
      "see `help(\"$\", \"unitizer\")`",
      fill=TRUE
    )
} )
#' Methods to Track Whether a \code{`\link{unitizerItem-class}`} Object is New Or Reference
#'
#' Necessitated due to the awkward structure around \code{`\link{reviewNext,unitizerBrowse-method}`},
#' where the only return value is a \code{`\link{unitizerItems-class}`} object and there is
#' no easy way to tell which objects have been kept from reference vs which ones are
#' new.
#'
#' @keywords internal
#' @aliases itemType,unitizerItem-method, itemType<-,unitizerItem-method, itemsitemType,unitizerItems-method,
#'   itemsType<-,unitizerItems-method

setGeneric("itemType", function(x, ...) standardGeneric("itemType"))
setMethod("itemType", "unitizerItem", function(x) if(x@reference) "reference" else "new")
setGeneric("itemType<-", function(x, value) standardGeneric("itemType<-"))
setReplaceMethod("itemType", c("unitizerItem", "character"),
  function(x, value) {
    if(!(value %in% c("new", "reference"))) stop("Argument `value` must be in ", deparse(c("new", "reference")))
    x@reference <- identical(value, "reference")
    x
} )
setGeneric("itemsType", function(x, ...) standardGeneric("itemsType"))
setMethod("itemsType", "unitizerItems",
  function(x) {
    vapply(as.list(x), function(y) if(y@reference) "reference" else "new", character(1L))
} )
setGeneric("itemsType<-", function(x, value) standardGeneric("itemsType<-"))
setReplaceMethod("itemsType", c("unitizerItems", "character"),
  function(x, value) {
    if(length(value) != 1L & !identical(length(x), length(value))) {
      stop("Argument `value` must be length 1L or have same length as argument `x`")
    }
    if(!all(value %in% c("reference", "new"))) stop("Argument `value` may only contain ", deparse(c("new", "reference")))
    if(length(x)) {
      x@.items <- mapply(function(y, z) {
          y@reference <- identical(z, "reference")
          y
        },
        x@.items, value, SIMPLIFY=F
      )
    }
    x
} )
setGeneric("ignored", function(x, ...) standardGeneric("ignored"))

#' Determines Which Items In \code{`\link{unitizerItems-class}`} Are Not Full Tests
#'
#' In order to simplify user interaction, some statements are not considered
#' to be tests, rather, they are set up steps for the actual test.  At the
#' time of this writing, top level assignments are included in this group.
#'
#' @keywords internal

setMethod("ignored", "unitizerItems", function(x, ...) vapply(as.list(x), function(y) y@ignore, logical(1L)))
setMethod("ignored", "unitizerItem", function(x, ...) x@ignore)

# - Multi Object Methods -------------------------------------------------------

#' Add a \code{`\link{unitizerItem-class}`} to a \code{`\link{unitizerItems-class}`}
#' @keywords internal

setMethod("+", c("unitizerItems", "unitizerItemOrNULL"),
  function(e1, e2) {
    if(is.null(e2)) return(e1)
    e1 <- append(e1, list(e2))
    e1
} )
#' Add a \code{`\link{unitizerItem-class}`} to a \code{`\link{unitizerItems-class}`}
#' @keywords internal

setMethod("+", c("unitizerItems", "unitizerItems"), function(e1, e2) append(e1, e2))

#' Retrieve Test Contents From Test Item
#'
#' Intended for use within the \code{unitizer} interactive environment, allows
#' user to retrieve whatever portions of tests are stored by \code{unitizer}.
#'
#' Currently the following elements are available:
#'
#' \itemize{
#'   \item \code{call} the call that was tested as an unevaluated call,
#'     but keep in mind that if you intend to evaluate this for a reference
#'     item the environment may not be the same so you could get different
#'     results (\code{ls} will provide more details)
#'   \item \code{value} the value that results from evaluating the test, note
#'     this is equivalent to using \code{.new} or \code{.ref}
#'   \item \code{output} the screen output (i.e. anything produced by cat/print,
#'     or any visible evaluation output) as a character vector
#'   \item \code{message} anything that was output to \code{stderr}, mostly
#'     this is all contained in the conditions as well, though there could be
#'     other output here, as a character vector
#'   \item \code{conditions} a \code{\link{conditionList-class}} containing all
#'     the conditions produced during test evaluation
#'   \item \code{aborted} whether the test call issues a restart call to the
#'     `abort` restart, as `stop` does.
#' }
#' @export
#' @aliases $
#' @param x a \code{unitizerItem} object, typically \code{.NEW} or \code{.REF}
#'   at the \code{unitizer} interactive prompt
#' @param name a valid test sub-component
#' @return the test component requested
#' @examples
#' ## From the unitizer> prompt:
#' \dontrun{
#' .NEW$call
#' .REF$conditions
#' .NEW$value              # equivalent to `.new`
#' }

setMethod("$", c("unitizerItem"),
  function(x, name) {
    what <- substitute(name)
    what <- if(is.symbol(what)) as.character(what) else name
    data.slots <- slotNames(x@data)
    if(identical(what, "call")) return(x@call)
    else if(length(what) != 1L || ! what %in% data.slots) {
      stop(
        "Argument `name` must be in ",
        paste0(deparse(c("call", data.slots), width.cutoff=500L), collapse=", ")
    ) }
    slot(x@data, what)
} )
