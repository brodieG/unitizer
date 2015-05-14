setClass(
  "unitizerOptsReset",
  slots=c(pre.load="logical", pre.test="logical", exit="logical"),
  prototype=list(pre.load=FALSE, pre.test=FALSE, exit=FALSE),
  validity=function(object)
    if(
      !all(
        vapply(
          slotNames(object),
          function(x) identical(length(slot(object, x)), 1L),
          logical(1L)
    ) ) ) "slots must be length 1" else TRUE
)
#' @rdname unitizerOpts-class

setClass(
  "unitizerOptsVal",
  slots=c(
    reset="unitizerOptsReset",
    warn="integer",
    value="ANY"
  ),
  prototype=list(warn=0L, value=NULL),
  validity=function(object) {
    if(!identical(length(object@warn), 1L)) return("slot `warn` must be length 1")
  }
)
#' @rdname unitizerOpts-class

unitizerOptsSearch <- setClass(
  "unitizerOptsSearch", contains="unitizerOptsVal",
  prototype=list(
    reset=new("unitizerOptsReset", pre.load=TRUE, pre.test=TRUE, exit=TRUE)
  )
)
#' @rdname unitizerOpts-class

unitizerOptsWd <- setClass(
  "unitizerOptsWd", contains="unitizerOptsVal",
  prototype=list(
    reset=new("unitizerOptsReset", pre.load=FALSE, pre.test=TRUE, exit=TRUE)
  )
)
#' @rdname unitizerOpts-class

unitizerOptsOpts <- setClass(
  "unitizerOptsOpts", contains="unitizerOptsVal",
  prototype=list(
    reset=new("unitizerOptsReset", pre.load=FALSE, pre.test=TRUE, exit=TRUE)
  )
)
#' @rdname unitizerOpts-class

unitizerOptsSeed <- setClass(
  "unitizerOptsSeed", contains="unitizerOptsVal",
  prototype=list(
    reset=new("unitizerOptsReset", pre.load=TRUE, pre.test=TRUE, exit=FALSE),
    value=1L
  )
)
#' @rdname unitizerOpts-class

unitizerOptsHistory <- setClass(
  "unitizerOptsHistory", contains="unitizerOptsVal",
  prototype=list(
    reset=new("unitizerOptsReset", pre.load=FALSE, pre.test=TRUE, exit=TRUE)
  )
)
#' Options Management
#'
#' @export

unitizerOpts <- setClass(
  "unitizerOpts",
  slots=c(
    search="unitizerOptsSearch",
    wd="unitizerOptsWd",
    opts="unitizerOptsOpts",
    seed="unitizerOptsSeed",
    history="unitizerOptsHistory"
  )
)
#' @rdname unitizerOpts-class

setMethod(
  "initialize", "unitizerOpts",
  function(.Object, ...) {
    # Since we're not sure how many slots our object will have, but we wwant
    # to allow partial matching to the slot names, we must get pretty creative
    # with creating a function with the slot names for partial matching, and
    # subsequent processing

    sn <- slotNames(.Object)
    s.list <- setNames(
      replicate(length(sn), substitute(x, alist(x=)), simplify=FALSE),
      sn
    )
    f <- function() NULL
    formals(f) <- s.list
    cl <- match.call(f, quote(unitizerOpts(...)))
    env <- parent.frame(2L)

    # Now process the matched values

    args.ev <- try(lapply(cl[-1L], eval, envir=env))
    if(inherits(args.ev, "try-error"))
      stop(
        "Passed arguments caused errors when initializing `unitizerOpts`; ",
        "see previous errors"
      )
    slots.final <- lapply(
      seq(args.ev),
      function(x) {
        obj <- args.ev[[x]]
        if(is(obj, "unitizerOptsVal")) {
          obj
        } else if (isTRUE(obj)) {
          new(getSlots("unitizerOpts")[[names(args.ev)[[x]]]])
        } else if (identical(obj, FALSE)) {
          new(
            getSlots("unitizerOpts")[[names(args.ev)[[x]]]],
            reset=new("unitizerOptsReset")
          )
        }
    } )
    for(i in seq(slots.final)) slot(.Object, names(args.ev)[[i]]) <- slots.final[[i]]
    .Object
  }
)
#' @rdname unitizerOpts-class

#' @rdname unitizerOpts-class

setMethod(
  "as.character", "unitizerOpts",
  function(x, ...) {
    res <- lapply(
      slotNames(x),
      function(y) {
        obj <- slot(x, y)
        c(
          vapply(
            slotNames(obj@reset), function(z) if(z) "x" else "", character(1L)
          ),
          obj@warn
    ) } )
    do.call(rbind, res)
  }
)
#' Set Option if Not Set
#'
#' @param x the name of the option to set
#' @param value the value to set the option to

setOptIfNot <- function(x, value) {
  if(!is.plainchr1(x) || is.na(x))
    stop("Argument `x` must be chracter(1L) and not NA")
  if(!x %in% names(options())) {
    opt.call <- list("options")
    opt.call[[x]] <- value
    eval(as.call(opt.call, parent.frame()))
  } else invisible(NULL)
}


