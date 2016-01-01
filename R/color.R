# Terminal colorization functions
#
# The code is taken and adapted from the "crayon" package by Gabor Csardi
# (https://github.com/gaborcsardi/crayon) under the MIT license:
#
#    License: MIT + file LICENSE
#    YEAR: 2014-2015
#    COPYRIGHT HOLDER: Gabor Csardi

# - R/has_ansi.r ---------------------------------------------------------------
# https://github.com/gaborcsardi/crayon/commit/5de3d97fe6d4d0627cdfa2b8b2f4d402dc404c63

# ansi color code regex pattern

ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
  "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
  "|\\x{001b}[A-M]")

# Check if a sting has some ANSI styling

has_style <- function(string) {
  grepl(ansi_regex, string, perl = TRUE)
}

# Remove ANSI escape sequences from a string

strip_style <- function(string) {
  gsub(ansi_regex, "", string, perl = TRUE)
}
# - R/has_color.R --------------------------------------------------------------
# https://github.com/gaborcsardi/crayon/commit/7c3319a75781c601bdab20c1b35cbd5d4c5085c8

# Does the current R session support ANSI colors?

has_color <- function() {
  ## Colors forced?

  enabled <- getOption("unitizer.color")
  if(is.TF(enabled)) return(enabled)

  ## Are we in a terminal? No?
  if (!isatty(stdout())) { return(FALSE) }

  ## Are we in a windows terminal?
  if (.Platform$OS.type == "windows") { return(FALSE) }

  ## Running in a recent Emacs?
  if (inside_emacs() && emacs_version()[1] >= 23) { return(TRUE) }

  ## COLORTERM set?
  if ("COLORTERM" %in% names(Sys.getenv())) { return(TRUE) }

  ## dumb terminal is not good
  if (Sys.getenv("TERM") == "dumb") { return(FALSE) }

  ## Otherwise try to guess based on TERM
  grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux",
    Sys.getenv("TERM"), ignore.case = TRUE, perl = TRUE)
}

# - R/styles.r ----------------------------------------------------------------
# https://github.com/gaborcsardi/crayon/commit/5d6170adefd5f996558f9b9158680715caf65be5

## Styles

codes <- list(
  reset = c(0, 0),
  bold = c(1, 22), # 21 isn't widely supported and 22 does the same thing
  blurred = c(2, 22),
  italic = c(3, 23),
  underline = c(4, 24),
  inverse = c(7, 27),
  hidden = c(8, 28),
  strikethrough = c(9, 29),

  black = c(30, 39),
  red = c(31, 39),
  green = c(32, 39),
  yellow = c(33, 39),
  blue = c(34, 39),
  magenta = c(35, 39),
  cyan = c(36, 39),
  white = c(37, 39),
  silver = c(90, 39),

  bgBlack = c(40, 49),
  bgRed = c(41, 49),
  bgGreen = c(42, 49),
  bgYellow = c(43, 49),
  bgBlue = c(44, 49),
  bgMagenta = c(45, 49),
  bgCyan = c(46, 49),
  bgWhite = c(47, 49)
)
codes.mx <- do.call(rbind, codes)

# Wrap ansii color codes around a string.  Color codes must be
# `%in% names(codes)`.  NA values mean no color.

clr <- function(
  txt, color, use.color=has_color()
) {
  if(!is.TF(use.color))
    stop("Argument `use.color` must be TRUE or FALSE.")
  if(!isTRUE(use.color)) return(txt)
  if(!is.character(txt)) stop("Argument `txt` must be character")
  color.no.na <- Filter(Negate(is.na), color)
  if(
    !is.character(color) || !all(color.no.na %in% rownames(codes.mx)) ||
    (length(color) != 1L && length(color) != length(txt))
  )
    stop(
      "Argument `color` must be character(1L) or the same length as `txt` ",
      "and may only contain valid color codes."
    )
  ifelse(
    is.na(color), txt,
    sprintf(
      "\u001b[%sm%s\u001b[%sm", codes.mx[, 1L][color], txt,
      codes.mx[, 2L][color]
  ) )
}
# R/utils.r
# https://github.com/gaborcsardi/crayon/commit/7e2e0963acf414e20bfe40a58e9a3bc6a7fe411f

inside_emacs <- function() {
  Sys.getenv("EMACS") != ""
}
emacs_version <- function() {
  ver <- Sys.getenv("INSIDE_EMACS")
  if (ver == "") return(NA_integer_)

  ver <- gsub("'", "", ver)
  ver <- strsplit(ver, ",", fixed = TRUE)[[1]]
  ver <- strsplit(ver, ".", fixed = TRUE)[[1]]
  as.numeric(ver)
}
