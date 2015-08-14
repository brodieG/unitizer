#' @include search.R

NULL

#' Global Option State Options
#'
#' Details on how to manage options state settings.
#'
#' When managing sessions state, there is an unfortunate dependency between
#' namespaces and options.  In particular, most packages set their namespaces
#' when they are loaded, not when they are attached.  This means that if we
#' want to ensure packages have reasonable options set and also wish to
#' attach and dettach them from the search path, we must fully unload their
#' namespace and imported namespaces so that options are reset when the
#' package is re-loaded.
#'
#' Matters are complicated by the need to allow preservation of certain
#' packages and namespaces, which by definition are not reloaded.  For state
#' manipulation to work we must ensure that packages and namespaces that
#' cannot be unloaded have their options preserved or at a minimum set to
#' reasonable values.
#'
#' @section Options Settings:
#'
#' To achieve this we have identified all the options that
#' are used in a freshly loaded R session, and split them into two groups:
#'
#' \enumerate{
#'   \item system specific options that are unlikely to affect test
#'     evaluation (e.g. \code{getOption("papersize")})
#'   \item other base options typically loaded in a vanilla R session (e.g.
#'     \code{getOption("width")})
#' }
#'
#' \code{unitizer} never modifies system specific options, and these are
#' specified in \code{.unitizer.opts.sys} as regular expression patterns.
#' Base options will be set to what we believe are the factory settings for
#' them, and those values are stored in \code{.unitizer.opts.base} as a
#' name-value pairs list (an actual list, not a pairlist).
#'
#' In order to allow for modification of these settings \code{unitizer} does
#' not use them directly, rather they are applied to the
#' \code{getOption("unitizer.opts.asis")} and
#' \code{getOption("unitizer.opts.base")} options, which are then read by
#' \code{unitizer}.  This allows you to specify different default values
#' for the base options or to add other options to the list to leave untouched.
#'
#' If you do modify these settings please take care to ensure that you add your
#' values to the options rather than completely overwriting them.
#'
#' @section Un-unloadable Packages And Namespaces:
#'
#' For whatever reason you might want to prevent \code{unitizer} from
#' detaching specific packages or unloading specific namespaces without
#' completely disabling the reproducible search path feature.  You can do so by
#' adding the packages and namespaces in question to
#' \code{getOption("unitizer.search.path.keep")} and
#' \code{getOption("unitizer.namespace.keep")}.
#'
#' If you do add packages and namespaces to the "keep" lists then you need
#' to ensure you also add all the options associated with those namespaces and
#' the namespaces they depend on / import to
#' \code{getOption("unitizer.opts.asis")} as well.  Alternatively, turn off
#' options tracking (see \code{\link{unitizerState}}).
#'
#' Note that some packages cannot be easily loaded and unloaded.  For example
#' \code{data.table} (<= 1.9.5) cannot be unloaded without causing a segfault
#' (see issue \href{https://github.com/Rdatatable/data.table/issues/990}{#990}).
#' For this reason \code{data.table} is included in
#' \code{getOption("unitizer.namespace.keep")} by default.
#'
#' @name unitizer.opts
#' @rdname unitizer.opts
#' @seealso \code{\link{unitizerState}}

NULL

#' @rdname unitizer.opts
#' @name .unitizer.opts.base
#' @export

NULL

.unitizer.opts.base <- list(
  add.smooth = TRUE, browserNLdisabled = FALSE, CBoundsCheck = FALSE,
  check.bounds = FALSE, citation.bibtex.max = 1, continue = "+ ",
  contrasts = structure(
    c("contr.treatment", "contr.poly"), .Names = c("unordered", "ordered")
  ),
  defaultPackages =
    c("datasets", "utils", "grDevices", "graphics", "stats", "methods"),
  demo.ask = "default", deparse.cutoff = 60L, device.ask.default = FALSE,
  digits = 7L, echo = TRUE, encoding = "native.enc", "NA" = NULL,
  example.ask = "default", expressions = 5000L,
  help.search.types = c("vignette", "demo", "help"),
  help.try.all.packages = FALSE, internet.info = 2, keep.source = TRUE,
  keep.source.pkgs = FALSE, locatorBell = TRUE, max.print = 99999L,
  na.action = "na.omit", nwarnings = 50L, OutDec = ".", prompt = "> ",
  repos = structure("http://cran.r-project.org", .Names = "CRAN"),
  rl_word_breaks = " \t\n\"\\'`><=%;,|&{()}", scipen = 0,
  show.coef.Pvalues = TRUE, show.error.messages = TRUE,
  show.signif.stars = TRUE,
  str = list(strict.width = "no", digits.d = 3, vec.len = 4),
  str.dendrogram.last = "`", stringsAsFactors = TRUE, timeout = 60,
  ts.eps = 1e-05, ts.S.compat = FALSE, useFancyQuotes = TRUE, verbose = FALSE,
  warn = 0, warning.length = 1000L, width = 80L
)
#' @rdname unitizer.opts
#' @name .unitizer.opts.sys
#' @export

NULL

.unitizer.opts.sys <- c(
  "^browser$", "^device$", "^dvipscmd$", "^mailer$", "^pager$",  "^pdfviewer$",
  "^pkgType$", "^printcmd$", "^HTTPUserAgent$",  "^texi2dvi$", "^unzip$",
  "^editor$", "^papersize$", "^bitmapType$",  "^menu\\.graphics$",
  "^unitizer\\."
)
#' @rdname unitizer.opts
#' @name .unitizer.namespace.keep
#' @export

NULL

.unitizer.namespace.keep <- c("data.table")

#' @rdname unitizer.opts
#' @name .unitizer.base.packages
#' @export

.unitizer.base.packages <- c(
  "package:stats", "package:graphics", "package:grDevices", "package:utils",
  "package:datasets", "package:methods", "Autoloads", "package:base",
  ".GlobalEnv"
)

#' Set Options to Initial Zero State
#'
#' @keywords internal

options_zero <- function(
  base=getOption("unitizer.opts.base"),
  as.is=getOption("unitizer.opts.asis")
) {
  if(
    !is.list(base) || !is.character(nms <- attr(base, "names")) ||
    length(nms) != length(base) || any(is.na(nms))
  ) {
    stop("Option `unitizer.opts.base` must be a named list")
  }
  if(!is.character(as.is) || any(is.na(as.is)))
    stop("Option `unitizer.opts.asis` must be character and not contain NA")

  curr.opts <- options()
  curr.opts.nms <- names(curr.opts)
  curr.opts.asis <- unlist(lapply(as.is, grep, curr.opts.nms, value=TRUE))

  # Drop unneeded options

  null.opts <- setdiff(names(curr.opts), c(nms, curr.opts.asis))
  options(setNames(vector("list", length(null.opts)), null.opts))

  # Reset others

  options(base)
  return(NULL)
}
#' Set Options to Specified State
#'
#' This makes sure to unset options not present in target.
#'
#' @keywords internal

options_update <- function(tar.opts) {
  stopifnot(is.list(tar.opts), is.character(names(tar.opts)))
  cur.opts <- names(options())
  to.rem <- setdiff(cur.opts, names(tar.opts))
  to.rem.vec <- setNames(vector("list", length(to.rem)), to.rem)
  options(to.rem.vec)
  options(tar.opts)
}
#' @rdname unitizer.opts
#' @name .unitizer.opts.default
#' @export

NULL

.unitizer.opts.default <- list(
  unitizer.par.env=NULL,                   # NULL means use the special unitizer environment
  unitizer.show.output=FALSE,              # Will display output/msg to stdout/stderr in addition to capturing it
  unitizer.disable.capt=FALSE,             # Will prevent capture
  unitizer.test.out.lines=c(50L, 15L),     # How many lines to display when showing test values, or truncate to if exceeds
  unitizer.test.fail.out.lines=c(10L, 5L), # How many lines to display when showing failed objects (note banner means one more line than this displayed)
  unitizer.test.msg.lines=c(10L, 3L),      # How many lines to display when showing test errors, or truncate to if exceeds
  unitizer.prompt.b4.quit.time=10,         # If unitizer runs in fewer seconds than this and has no reviewed items, `Q` will quit directly without prompting for review
  unitizer.max.capture.chars=200000L,      # Maximum number of characters we allow capture of per test
  unitizer.history.file="",                # "" is interpreted as tempfile()
  unitizer.search.path.keep=c(             # what objects to keep on search path when initializing unitizer; if you modify this make sure you ajdust `unitizer.opts.asis` accordingly as well (see reproducible state vignette)
    .unitizer.base.packages,
    "tools:rstudio", "package:unitizer"
  ),
  unitizer.namespace.keep=c(               # namespaces not to auto-unload, no matter what
    .unitizer.namespace.keep
  ),
  unitizer.state="safe",                   # default reproducible state mode
  unitizer.opts.base=.unitizer.opts.base,  # what to set options to when running in reproducible state
  unitizer.opts.asis=.unitizer.opts.sys,   # system dependent and other options that should not be changed; these are matched as regular expressions
  unitizer.seed=                           # random seed to use by default, "Wichman-Hill" because default seed is massive
      list(seed=42L, kind="Wichmann-Hill")
)

#' Checks that options meet expectations before anything gets run

validate_options <- function(opts.to.validate) {
  stopifnot(
    is.list(opts.to.validate),
    all(grep("^unitizer\\.", names(opts.to.validate)))
  )
  names.def <- setdiff(names(.unitizer.opts.default), "unitizer.par.env")  # unitizer.par.env can be NULL
  if(any(missing.opts <- !names.def %in% names(opts.to.validate)))
    stop(
      "The following options must be set in order for `unitizer` to work: ",
      deparse(names(.unitizer.opts.default)[missing.opts], width=500L)
    )
  with(
    opts.to.validate,
    {
      if(!is.TF(unitizer.show.output))
        stop("Option `unitizer.show.output` must be TRUE or FALSE")
      if(!is.TF(unitizer.disable.capt))
        stop("Option `unitizer.disable.capt` must be TRUE or FALSE")
      if(!is.int.pos.2L(unitizer.test.out.lines))
        stop(
          "Option `unitizer.test.out.lines` must be integer(2L), strictly ",
          "positive, and not NA"
        )
      if(!is.int.pos.2L(unitizer.test.fail.out.lines))
        stop(
          "Option `unitizer.test.fail.out.lines` must be integer(2L), ",
          "strictly positive, and not NA"
        )
      if(!is.int.pos.2L(unitizer.test.msg.lines))
        stop(
          "Option `unitizer.test.msg.lines` must be integer(2L), strictly ",
          "positive, and not NA"
        )
      if(
        !is.numeric(unitizer.prompt.b4.quit.time) ||
        length(unitizer.prompt.b4.quit.time) != 1L ||
        is.na(unitizer.prompt.b4.quit.time) ||
        unitizer.prompt.b4.quit.time < 0
      )
        stop(
          "Option `unitizer.prompt.b4.quit.time` must be numeric(1L), not NA, ",
          "and strictly positive"
        )
      if(
        !is.integer(unitizer.max.capture.chars) ||
        length(unitizer.max.capture.chars) != 1L ||
        is.na(unitizer.max.capture.chars) ||
        unitizer.max.capture.chars < 0
      )
        stop(
          "Option `unitizer.max.capture.chars` must be integer(1L), not NA, ",
          "and strictly positive"
        )
      if(!is.chr1(unitizer.history.file))
        stop(
          "Option `unitizer.history.file` must be character(1L) and not NA, ",
          " or NULL"
        )
      if(
        !is.character(unitizer.search.path.keep) ||
        any(is.na(unitizer.search.path.keep))
      )
        stop("Option `unitizer.search.path.keep` must be character and not NA")
      if(
        !is.character(unitizer.namespace.keep) ||
        any(is.na(unitizer.namespace.keep))
      )
        stop("Option `unitizer.namespace.keep` must be character and not NA")

      if(!is(is.valid_state(unitizer.state), "unitizerState"))
        stop("Option `unitizer.state` is invalid; see prior errors")
      if(!is.list(unitizer.opts.base))
        stop("Option `unitizer.opts.base` must be a list")
      if(!is.character(unitizer.opts.asis) || any(is.na(unitizer.opts.asis)))
        stop("Option `unitizer.opts.asis` must be character and not NA")
      if(!is.list(unitizer.seed))  # note, more specific validation done in is.valid_rep_state
        stop("Option `unitizer.seed` must be a list")
  } )
  if( # needs to be outside b/c var may not be defined in option list
    !is.null(opts.to.validate[["unitizer.par.env"]]) &&
    !is.environment(opts.to.validate[["unitizer.par.env"]])
  )
    stop("Option `unitizer.par.env` must be an environment or NULL")
  TRUE
}
