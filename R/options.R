# Copyright (C) 2021 Brodie Gaslam
# 
# This file is part of "unitizer"
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' @include search.R

NULL

#' Unitizer Options
#'
#' Description of major \code{unitizer} option settings.  Once \code{unitizer}
#' is loaded, you can see a full list of \code{unitizer} options with
#' \code{grep("^unitizer", options(), value=TRUE)}.
#'
#' @section Basic State Options:
#'
#' Basic state options:
#'
#' \itemize{
#'   \item \code{unitizer.state}: default state tracking setting (see
#'     \code{unitizerState})
#'   \item \code{unitizer.seed}: default seed to use when random seed tracking
#'     is enabled; this is of type "Wichman-Hill" because it is a lot more
#'     compact than the default R random seed, and should be adequate for most
#'     unit testing purposes.
#' }
#' @section Options State Options:
#'
#' Additionally, when tracking option state we set options to what you would
#' find in a freshly loaded vanilla R session, except for systems specific
#' options which we leave unchanged (e.g. \code{getOption("papersize")}).
#' If you want to add default option values or options to leave unchanged, you
#' can use:
#'
#' \itemize{
#'   \item \code{unitizer.opts.init}: named list, where names are options, and
#'     the associated value is the value to use as the default value for that
#'     option when a \code{unitizer} is launched with options tracking enabled.
#'   \item \code{unitizer.opts.asis}: character, containing regular expressions
#'     to match options to leave unchanged (e.g \code{"^unitizer\\."})
#' }
#' @section Search Path and Namespace State Options:
#'
#' We also provide options to limit what elements can be removed from
#' the search path and/or have their namespaces unloaded when \code{unitizer}
#' tracks the search path state.  For example, we use this mechanism to prevent
#' removal of the \code{unitizer} package itself as well as the default
#' R vanilla session packages.
#'
#' \itemize{
#'   \item \code{unitizer.namespace.keep}: character, names of namespaces to
#'     keep loaded (e.g. \code{"utils"}); note that any imported namespaces
#'     imported by namespaces listed here will also remain loaded
#'   \item \code{unitizer.search.path.keep}: character, names of objects to
#'     keep on search path (e.g. \code{"package:utils"}, note the
#'    \code{"package:"}); associated namespaces will also be kept loaded
#' }
#' \bold{IMPORTANT}: There is a dependency between options tracking and search
#' path / namespace exceptions that stems from most packages setting their
#' default options when they are loaded.  As a result, if you add any packages
#' or namespaces to these options and options state tracking is enabled, then
#' you must also add their options to \code{unitizer.opts.init} or
#' \code{unitizer.opts.asis} to ensure those options remain loaded or at least
#' set to reasonable values.  If you do not do this the packages risk having
#' their options unset.
#'
#' Some packages cannot be easily loaded and unloaded.  For example
#' \code{data.table} (<= 1.9.5) cannot be unloaded without causing a segfault
#' (see issue \href{https://github.com/Rdatatable/data.table/issues/990}{#990}).
#' For this reason \code{data.table} is included in
#' \code{getOption("unitizer.namespace.keep")} by default.
#'
#' @section Sytem Default State Options:
#'
#' The following options hold the default system values for the search
#' path / namespace and options state tracking options:
#' \itemize{
#'   \item \code{unitizer.namespace.keep.base}: namespaces that are known to
#'     cause problems when unloaded (as of this writing includes
#'     \code{data.table})
#'   \item \code{unitizer.search.path.keep.base}: vanilla R session packages,
#'     plus \code{"package:unitizer"} and \code{"tools:rstudio"}, the latter
#'     because its implementation prevents re-attaching it if it is detached.
#'   \item \code{unitizer.opts.asis.base}: system specific options that should
#'     not affect test evaluation (e.g. \code{getOption("editor")}).
#'   \item \code{unitizer.opts.init.base}: base options (e.g.
#'     \code{getOption("width")} that will be set to what we believe are the
#'     factory settings for them.
#' }
#' These are kept separate from the user specified ones to limit the possibility
#' of inadvertent modification. They are exposed as options to allow the user to
#' unset single values if required, though this is intended to be rare.
#' \code{unitizer} runs with the union of user options and the system versions
#' described here.  For \code{unitizer.opts.init}, any options set that are
#' also present in \code{unitizer.opts.init.base} will overrule the base
#' version.
#'
#' @section Display / Text Capture Options:
#'
#' These options control how \code{unitizer} displays data such as diffs, test
#' results, etc.
#'
#' \itemize{
#'   \item \code{unitizer.test.out.lines}: integer(2L), where first values is
#'     maximum number of lines of screen output to show for each test, and
#'     second value is the number of lines to show if there are more lines than
#'     allowed by the first value
#'   \item \code{unitizer.test.msg.lines}: like \code{unitizer.test.out.lines},
#'     but for \code{stderr output}
#'   \item \code{unitizer.test.fail.context.lines}: integer(2L), used
#'     exclusively when comparing new to references tests when test faile; first
#'     values is maximum number of lines of context to show around a test,
#'     centered on differences if there are any, and second value is the number
#'     of context lines to show if using the first value is not sufficient to
#'     fully display the test results
#'   \item \code{unitizer.show.output}: TRUE or FALSE, whether to display test
#'     \code{stdout} and \code{stderr} output as it is evaluated.
#'   \item \code{unitizer.disable.capt}: logical(2L), not NA, with names
#'     \code{c("output", "message")} where each value indicates whether the
#'     corresponding stream should be captured or not.  For \code{stdout} the
#'     stream is still captured but setting the value to FALSE tees it.
#'   \item \code{unitizer.max.capture.chars}: integer(1L) maximum number of
#'     characters to allow capture of per test
#'   \item \code{unitizer.color} whether to use ANSI color escape sequences,
#'     set to TRUE to force, FALSE to force off, or NULL to attempt to auto
#'     detect (based on code from package:crayon, thanks Gabor Csardi)
#'   \item \code{unitizer.use.diff} TRUE or FALSE, whether to use a diff of
#'     test errors (defaults to TRUE)
#' }
#' @section Misc Options:
#'
#' \itemize{
#'   \item \code{unitizer.history.file} character(1L) location of file to use
#'     to store history of command entered by user in in interactive
#'     \code{unitizer} prompt; \code{""} is interpreted as tempfile()
#'   \item \code{unitizer.prompt.b4.quit.time} integer(1L) \code{unitizers} that
#'     take more seconds than this to evaluate will post a confirmation prompt
#'     before quitting; this is to avoid accidentally quitting after running a
#'     \code{unitizer} with many slow running tests and having to re-run them
#'     again.
#'   \item \code{unitizer.restarts.ok} TRUE or FALSE, suppresses warnings when
#'     running inside a `withRestarts` block, which is normally a warning.
#'     Needed due to `test_that` adding a `withRestart`
#' }
#'
#' @name unitizer.opts
#' @rdname unitizer.opts
#' @seealso \code{\link{unitizerState}}

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

.unitizer.opts.asis <- c(
  "^browser$", "^device$", "^dvipscmd$", "^mailer$", "^pager$",  "^pdfviewer$",
  "^pkgType$", "^printcmd$", "^HTTPUserAgent$",  "^texi2dvi$", "^unzip$",
  "^editor$", "^papersize$", "^bitmapType$",  "^menu\\.graphics$",
  "^unitizer\\."
)
.unitizer.namespace.keep <- c("data.table", "covr", "crayon", "tools")

.unitizer.base.packages <- c(
  "package:stats", "package:graphics", "package:grDevices", "package:utils",
  "package:datasets", "package:methods", "Autoloads", "package:base",
  ".GlobalEnv"
)

#' Set Options to Initial Zero State
#'
#' @rdname options_extra
#' @keywords internal

options_zero <- function(
  base=merge_lists(
    getOption("unitizer.opts.init.base"), getOption("unitizer.opts.init")
  ),
  as.is=union(
    getOption("unitizer.opts.asis.base"), getOption("unitizer.opts.asis")
  )
) {
  if(
    !is.list(base) || !is.character(nms <- attr(base, "names")) ||
    length(nms) != length(base) || any(is.na(nms))
  ) {
    stop("Option `unitizer.opts.init` must be a named list")
  }
  if(!is.character(as.is) || any(is.na(as.is)))
    stop("Option `unitizer.opts.asis` must be character and not contain NA")

  curr.opts <- options()
  curr.opts.nms <- names(curr.opts)
  curr.opts.asis <- unlist(lapply(as.is, grep, curr.opts.nms, value=TRUE))

  # Drop unneeded options; need to do 1 by 1 as some options cannot be easily
  # reset

  null.opts <- setdiff(names(curr.opts), c(nms, curr.opts.asis))
  all.opts <- c(
    setNames(vector("list", length(null.opts)), null.opts), base
  )
  opt.success <- vapply(names(all.opts),
    function(opt.name) {
      opt.attempt <- try(options(all.opts[opt.name]), silent=TRUE)
      return(!inherits(opt.attempt, "try-error"))
    },
    logical(1L)
  )
  if(!all(opt.success)) {
    warning(
      word_wrap(
        cc(
          "Unable to reset following options: ",
          deparse(names(all.opts)[!opt.success], width.cutoff=500L)
    ) ) )
  }
  # Reset others

  options(base)
  return(NULL)
}
#' Set Options to Specified State
#'
#' This makes sure to unset options not present in target.
#'
#' @rdname options_extra
#' @keywords internal

options_update <- function(tar.opts) {
  stopifnot(is.list(tar.opts), is.character(names(tar.opts)))
  cur.opts <- names(options())
  to.rem <- setdiff(cur.opts, names(tar.opts))
  to.rem.vec <- setNames(vector("list", length(to.rem)), to.rem)
  options(to.rem.vec)
  options(tar.opts)
}

.unitizer.opts.default <- list(
  # NULL means use the special unitizer environment
  unitizer.par.env=NULL,
  # Will display output/msg to stdout/stderr in addition to capturing it
  unitizer.show.output=FALSE,
  # Attempt to ANSI colorize output, TRUE to force, FALSE to force off, NULL to
  # auto-detect based on terminal capability
  unitizer.color=NULL,
  unitizer.disable.capt=
    c(output=FALSE, message=FALSE),        # Will prevent capture
  # How many lines to display when showing test values, or truncate to if exceeds
  unitizer.test.out.lines=c(50L, 15L),
  # How many lines to display when showing test errors, or truncate to if exceeds
  unitizer.test.msg.lines=c(50L, 15L),
  # How many lines of context to display when showing failed objects
  # (note banner means one more line than this displayed)
  unitizer.test.fail.context.lines=c(10L, 3L),
  # If unitizer runs in fewer seconds than this and has no reviewed items, `Q`
  # will quit directly without prompting for review
  unitizer.prompt.b4.quit.time=10,
  # Maximum number of characters we allow capture of per test
  unitizer.max.capture.chars=200000L,
  unitizer.history.file="",                # "" is interpreted as tempfile()
  # User specified objects to keep on search path; if you modify this make sure
  # you ajdust `unitizer.opts.asis` accordingly as well (see reproducible state
  # vignette)
  unitizer.search.path.keep=character(),
  # Default objects to keep on search path when initializing unitizer;
  unitizer.search.path.keep.base=c(
    .unitizer.base.packages,
    "tools:rstudio", "package:unitizer"
  ),
  unitizer.namespace.keep = character(),   # names of namespaces not auto-unload
  # system namespaces not to auto-unload, no matter what
  unitizer.namespace.keep.base=c(
    .unitizer.namespace.keep
  ),
  # User default option values when running with options state tracking
  unitizer.opts.init=list(),
  # Default option values when running with options state tracking
  unitizer.opts.init.base=.unitizer.opts.base,
  # User specified options that should not be changed; these are matched as
  # regular expressions
  unitizer.opts.asis=character(0L),
  # Default options not to change; these are primarily system dependent and
  # other options; these are matched as regular expressions
  unitizer.opts.asis.base=.unitizer.opts.asis,
  # random seed to use by default, "Wichman-Hill" because default seed is large
  unitizer.seed= list(seed=42L, kind="Wichmann-Hill"),
  unitizer.max.env.depth=20000L,
  unitizer.use.diff=TRUE,
  # whether to warn if `unitizer` is run in `withRestarts` context, added b/c
  # testthat added a restart in 80a81fd
  unitizer.restarts.ok=FALSE
)

#' Checks that options meet expectations before anything gets run
#' @rdname options_extra
#' @keywords internal

validate_options <- function(opts.to.validate, test.files=NULL) {
  stopifnot(
    is.list(opts.to.validate),
    all(grep("^unitizer\\.", names(opts.to.validate)))
  )
  # Check all option existence except those that can be NULL; note that we

  names.def <- setdiff(
    names(.unitizer.opts.default),
    c("unitizer.par.env", "unitizer.color", "unitizer.history.file")
  )
  if(any(missing.opts <- !names.def %in% names(opts.to.validate)))
    stop(
      "The following options must be set in order for `unitizer` to work: ",
      deparse(names.def[missing.opts], width.cutoff=500L)
    )
  # Now validate

  with(
    opts.to.validate,
    {
      if(!is.TF(unitizer.show.output))
        stop("Option `unitizer.show.output` must be TRUE or FALSE")
      if(!is.TF(unitizer.use.diff))
        stop("Option `unitizer.use.diff` must be TRUE or FALSE")
      if(
        exists("unitizer.color", inherits=FALSE) &&
        !is.TF(unitizer.color) && !is.null(unitizer.color)
      )
        stop("Option `unitizer.color` must be TRUE, FALSE, or NULL")
      if(!is.valid_capt_setting(unitizer.disable.capt))
        stop("Option `unitizer.disable.capt` is invalid (see prior message)")
      if(!is.screen.out.vec(unitizer.test.out.lines))
        stop(
          "Option `unitizer.test.out.lines` must be integer(2L), strictly ",
          "positive, not NA, with first value larger than second"
        )
      if(!is.context.out.vec(unitizer.test.fail.context.lines))
        stop(
          "Option `unitizer.test.fail.context.lines` must be integer(2L), ",
          "positive, not NA, with first value larger than second"
        )
      if(!is.screen.out.vec(unitizer.test.msg.lines))
        stop(
          "Option `unitizer.test.msg` must be integer(2L), strictly ",
          "positive, not NA, with first value larger than second"
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
      if(
        !is.character(unitizer.search.path.keep) ||
        any(is.na(unitizer.search.path.keep))
      )
        stop("Option `unitizer.search.path.keep` must be character and not NA")
      if(
        !is.character(unitizer.search.path.keep.base) ||
        any(is.na(unitizer.search.path.keep.base))
      )
        stop("Option `unitizer.search.path.keep.base` must be character and not NA")
      if(
        !is.character(unitizer.namespace.keep) ||
        any(is.na(unitizer.namespace.keep))
      )
        stop("Option `unitizer.namespace.keep` must be character and not NA")
      if(
        !is.character(unitizer.namespace.keep.base) ||
        any(is.na(unitizer.namespace.keep.base))
      )
        stop("Option `unitizer.namespace.keep.base` must be character and not NA")

      if(!is.list(unitizer.opts.init))
        stop("Option `unitizer.opts.init` must be a list")
      if(!is.list(unitizer.opts.init.base))
        stop("Option `unitizer.opts.init.base` must be a list")
      if(!is.character(unitizer.opts.asis) || any(is.na(unitizer.opts.asis)))
        stop("Option `unitizer.opts.asis` must be character and not NA")
      if(
        !is.character(unitizer.opts.asis.base) ||
        any(is.na(unitizer.opts.asis.base))
      )
        stop("Option `unitizer.opts.asis.base` must be character and not NA")
      # note, more specific validation done in is.valid_rep_state
      if(!is.list(unitizer.seed))
        stop("Option `unitizer.seed` must be a list")
  } )
  if( # needs to be outside b/c var may not be defined in option list
    !is.null(opts.to.validate[["unitizer.par.env"]]) &&
    !is.environment(opts.to.validate[["unitizer.par.env"]])
  )
    stop("Option `unitizer.par.env` must be an environment or NULL")
  if( # needs to be outside b/c var may not be defined in option list
    !is.null(opts.to.validate[["unitizer.state"]]) &&
    !is(
      try(
        as.state(opts.to.validate[["unitizer.state"]], test.files)
      ),
      "unitizerState"
    )
  )
    stop("Option `unitizer.state` is invalid; see prior errors")
  if(
    !is.chr1(opts.to.validate[["unitizer.history.file"]]) &&
    !is.null(opts.to.validate[["unitizer.history.file"]])
  )
    stop(
      "Option `unitizer.history.file` must be character(1L) and not NA, ",
      " or NULL"
    )
  # NULL options that should be changed

  if(is.null(opts.to.validate[["unitizer.color"]])) {
    opts.to.validate[["unitizer.color"]] <-
      isTRUE(try(crayon::has_color(), silent=TRUE))
  }
  opts.to.validate
}
