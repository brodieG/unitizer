#' Set Option if Not Set
#'
#' Utility function used by .onLoad to set a global option if it is not set
#' already.  This is equivalent to conditionally using \code{option} to set
#' options that are not defined yet.
#'
#' @export
#' @param x the name of the option to set
#' @param value the value to set the option to
#' @return the result of calling \code{option} if option is not set, or NULL
#'   invisibly if it is already set

setOptIfNotSet <- function(x, value) {
  if(!is.chr1(x)) stop("Argument `x` must be chracter(1L) and not NA")
  if(!x %in% names(options())) {
    opt.call <- list(quote(options))
    opt.call[[x]] <- value
    eval(as.call(opt.call), parent.frame())
  } else invisible(NULL)
}

#' Variables Controlling Global Option State During \code{unitizer}
#'
#' These variables are used when setting options state to the highest
#' reproducibility setting to minimize odds that test failures are caused
#' by changes to global options.
#'
#' \itemize{
#'   \item \code{.unitizer.opts.base} will be passed to \code{options} as the
#'     argument
#'   \item \code{.unitizer.opts.sys} are options to leave unchanged; these are
#'     typically system specific, should be very unlikely to affect test
#'     outcomes, and could possibly cause problems if we unset them or tried to
#'     set them to the defaults of a different system.
#' }
#' All other options are set to NULL.
#'
#' The variables are applied by being as the \code{"unitizer.opts.base"} and
#' \code{"unitizer.opts.asis"} options (e.g.
#' \code{options(unitizer.opts.base=.unitizer.opts.base)}); if you wish to use
#' different values specify them through the options.
#'
#' Note that when \code{unitizer} resets options to base set, the
#' \code{unitizer} options are also re-set, but \code{unitizer} always reads in
#' the options before clearing them.
#'
#' All options are restored to their original values on exit.
#'
#' @rdname unitizer.opts
#' @export

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
#' @export

.unitizer.opts.sys <- c(
  "browser", "device", "dvipscmd", "mailer", "pager", "pdfviewer", "pkgType",
  "printcmd", "HTTPUserAgent", "texi2dvi", "unzip", "editor", "papersize",
  "bitmapType", "menu.graphics"
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

  # Drop unneeded options

  null.opts <- setdiff(names(curr.opts), c(nms, as.is))
  options(setNames(vector("list", length(null.opts)), null.opts))

  # Reset others

  options(base)
  return(NULL)
}


