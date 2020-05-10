# Copyright (C) 2020  Brodie Gaslam
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


# Strategy for parsing comments:
#
# Look up all entries with parent == 0; these are the top level entries
# All the comments that are -id are top level comments. The id refers to the
# next statement. Comments don't have parents when they are the last thing in
# the file.  So logic, using getParseData():
#
#
# - Get all top level ids
# - Get all comments that have -ids or 0 ids
# - For each comment with -id, check whether comment line is same as the previous
#   top level comment end line
#   + if yes, associate with that statement
#   + if not, associate with the -id statement
#  - For zero ids, just check if on same line as last top level statement
#
# After this, need to break up the data frame into pieces based on what the
# top level parent is.  This is fine and well, though we need to reset the
# top level parents so we can do this recursively for unitizer_sect
#
# Unfortunately, this gets seriously complicated by the fact that comments
# inside calls have for parent the call, irrespective of where they are.
# So we need to figure out (ONLY FOR TOP LEVEL COMMENTS):
# - is a comment on it's own line?
# - if yes, find the next "top level" item
# - if no, find previous "top level" item
#
# Basically, the process is as follows:
# 1. define top level (starts of at zero, and for zero, make sure
#    any negative parents are zero)
# 2. Split data frame by top level, and:
#    - assign comments to each top level object
#    - assign data frame chunks to each top level object
# 3. Recurse through all the objects until we get to terminals
#
# Net result should be an expression that, for each non terminal
# object will have a piece of the original parse data frame attached,
# as well as comments (do we need the original parse data frame, maybe
# not if we process it all in one go?)


# Searches Through Generations Until it Finds Top Level
#
# Returns the id of the ancestor that is just before \code{`top.level`},
# or \code{`top.level`} if the parent already is \code{`top.level`}. The
# idea is to reduce the set of parents for all elements to just the top
# level parents as this allows us to split the parse data into sections,
# including the calls that were direct children of top level, as well as
# the children to those sections.
#
# @param ids integer the ids to look through
# @param par.ids integer the parenthood relationships
# @param top.level the id of the top level
# @return integer the top level parent ids for \code{`ids`}

top_level_parse_parents <- function(ids, par.ids, top.level=0L) {
  if(
    !is.integer(ids) || !is.integer(par.ids) ||
    !identical(length(ids), length(par.ids))
  )
    stop("Arguments `ids` and `par.ids` must be equal length integer vectors")
  if(!identical(length(setdiff(abs(par.ids), c(ids, top.level))), 0L))
    stop("Argument `par.ids` contains ids not in `ids`.")
  if(!is.integer(top.level) && !identical(length(top.level), 1L))
    stop("Argument `top.level` must be a one length integer")
  if(identical(top.level, 0L)) {
    par.ids <- pmax(0L, par.ids)
  } else if (any(par.ids) < 0) {
    # nocov start
    stop(
      "Internal Error: Argument `par.ids` contains values less than zero, ",
      "but that is only allowed when `top.level` == 0L; contact maintainer."
    )
    # nocov end
  }
  # Create lookup matrix so we can look up ids directly.  This will be a slightly
  # sparse matrix to the extend `ids` doesn't contain every number between
  # range(ids).  The idea is to be able to lookup id-par pairs by direct index
  # access.  This is not super efficient since we keep recalculating some of the
  # data over and over with each recursion.

  id.range <- range(ids)
  if(id.range[[1L]] < 1L)
    stop("Expected only strictly positive unique ids")
  par.full <- rep(NA_integer_, id.range[[2L]])
  par.full[ids] <- par.ids
  res <- rep(NA_integer_, length(ids))

  for(i in seq_along(par.ids)) {
    cur.id <- new.id <- par.ids[[i]]
    while(cur.id != top.level) {
      new.id <- par.full[[cur.id]]
      if(is.na(new.id)) break;
      if(new.id == top.level) {
        new.id <- cur.id
        break;
      }
      cur.id <- new.id
    }
    res[[i]] <- new.id
  }
  res
}
# For Each ID Determines Generation
#
# @param ids integer() the object ids
# @param par.ids integer() the parents of each \code{ids}
# @param id integer() the first parent
# @return matrix containing ids and corresponding generation for the ids

ancestry_descend <- function(ids, par.ids, id, level=0L) {
  # Initialize result matrix, can be no bigger than ids

  max.size <- length(ids)
  res <- matrix(
    rep(NA_integer_, max.size * 2L), ncol=2L,
    dimnames=list(NULL, c("children", "level"))
  )
  ind.start <- 1L
  par.idx <- 1L
  par.list <- id
  id.split <- list2env(split(ids, par.ids))

  repeat {
    if(!length(par.list)) break
    child.len <-
      length(children <- id.split[[as.character(par.list[[par.idx]])]])
    if(child.len) {
      ind.end <- ind.start + child.len - 1L
      # if(ind.end > max.size)
      #   stop("Internal Error: exceeded allocated size when finding children; contact maintainer.")
      inds <- ind.start:ind.end
      res[inds, 1L] <- children
      res[inds, 2L] <- level
      ind.start <- ind.end + 1L
    }
    par.idx <- par.idx + 1L
    if(par.idx > length(par.list)) {
      par.list <- res[which(res[, 2L] == level), 1L]
      level <- level + 1L
      par.idx <- 1L
    }
  }
  res[!is.na(res[, 1L]), ]
}
# Need this to pass R CMD check; problems likely caused by `transform` and
# `subset`.

if(getRversion() >= "2.15.1")  utils::globalVariables(c("token", "col1", "line1"))

# Assign Comments From Parse Data to Expression Elements
#
# Based on parse data from \code{`\link{getParseData}`}, figures
# out what comments belong to what expression.  If a comment is
# on the same line as an expression, the comment is assigned to that
# expression (or whatever the nearest expression is on that line if
# there is more than one).  If a comment is on it's own line,
# then the match is done to the next expression.
#
# The expectation is that only "top level" expressions will
# be submitted as part of `comment.dat` (i.e. they all have
# the same parent, they don't strictly have to be top.level).
#
# @param expr and expression
# @param comment.dat a data frame derived from \code{`\link{getParseData}`}
# @return an expression with comments attached as attributes to each
#   expression component

comments_assign <- function(expr, comment.dat) {
  if(!identical(length(unique(comment.dat$parent)), 1L))
    stop( # nocov start
      "Internal Error: there were multiple parent ids in argument ",
      "`comment.dat`; this should not happen"
    )     # nocov end
  if(!length(expr) || !length(which(comment.dat$token == "COMMENT")))
    return(expr)

  # Make sure `comment.dat` is in format we understand
  # Theory: everything not "COMMENT" should be included, except:
  # - opening parens on second row (these denote a function call)
  # - closing braces of any kind on last row
  # Additionally, in order for stuff to match up properly, anything that is not
  # "expr" needs to be moved to the front (in theory, should be at most one thing
  # and should be an infix operator of some sort)

  if(
    !tail(comment.dat$token, 1L) %in%
    c("COMMENT", "expr", tk.lst$non.exps, tk.lst$brac.close, "';'")
  )
    # nocov start
    stop(
      "Internal Error: unexpected ending token in parse data; contact ",
      "maintainer."
    )
    # nocov end
  if(
    length(which(comment.dat$token %in% tk.lst$brac.open)) > 1L ||
    length(which(comment.dat$token %in% tk.lst$brac.close)) > 1L
  ) {
    stop( # nocov start
      "Internal Error: more than one bracket at top level; contact maintainer."
    )     # nocov end
  }
  if(
    length(brac.pos <- which(comment.dat$token %in% tk.lst$brac.close)) &&
    !identical(brac.pos, nrow(comment.dat))
  ) {
    # nocov start
    # shouldn't happen, can't test
    if(
      !identical(comment.dat$token[brac.pos], "')'") ||
      !identical(brac.pos, nrow(comment.dat) - 1L) ||
      !identical(comment.dat$token[[1L]], "FUNCTION")
    ) {
      stop(
        "Internal Error: closing brackets may only be on last row, unless a ",
        "paren and part of a functions formal definition; contact maintainer."
      )
    }
    # nocov end
  }
  if(
    !all(match(comment.dat$token, tk.lst$brac.open[-3L], 0L) <= 1L) ||
    !all(match(comment.dat$token, tk.lst$brac.open[3L], 0L) <= 2L)
  ) {
    stop(   # nocov start
      "Internal Error: opening brackets may only be on first row, or second ",
      "if paren; contact maintainer."
    )       # nocov end
  }
  if(
    !identical(
      which(tk.lst$brac.open %in% comment.dat$token),
      which(tk.lst$brac.close %in% comment.dat$token)
    )
  )
    stop("Internal Error: mismatched brackets; contact maintainer.") # nocov
  # extra.toks <- if(any(brac.open %in% comment.dat$token)) 2L else 1L
  # Trim our data to just what matters:

  comm.notcomm <- prsdat_reduce(comment.dat)
  if(!identical(nrow(comm.notcomm), length(expr))) {
    # nocov start
    stop(
      "Internal Error: Argument `expr` length cannot be matched with values ",
      "in `comment.dat`"
    )
    # nocov end
  }
  # for the purposes of this process, constants and symbols are basically
  # expressions

  comm.notcomm <- transform(
    comm.notcomm,
    token=ifelse(
      token %in% c(
        tk.lst$exps, tk.lst$non.exps, tk.lst$non.exps.extra, tk.lst$ops
      ),
      "expr", token
  ) )
  # what comments are on same line as something else

  comm.comm <- subset(comment.dat, token=="COMMENT")
  comm.expr <- subset(comm.notcomm, token=="expr")

  # identify whether a token is the first or last on it's line.  Values mean
  # - 3L is only item on line (we think)
  # - 2L is last item on line (we think)

  comm.expr$first.last.on.line <- with(
    comm.expr,
    ave(
      col1, line1,
      FUN=function(x)
        if(length(x) == 1L) 3L
        else ifelse(x == max(x), 2L, ifelse(x == min(x), 1L, 0L))
  ) )
  # For each comment on a line that also has an expression, find the expression
  # that is also on that line

  comm.comm$assign.to.prev <- with(
    comm.expr, line2[match(comm.comm$line1, line2)]
  )
  comm.comm$match <- with(comm.expr,  {
    last.or.only <- first.last.on.line %in% 2L:3L
    id[last.or.only][match(comm.comm$assign.to.prev, line1[last.or.only])]
  } )
  # For each comment on its own line, find the expression that follows it

  first.or.only <- comm.expr$first.last.on.line %in% c(1L, 3L)
  comm.comm$assign.to.next <- vapply(
    comm.comm$line1,
    function(x) if(any(idx <- (comm.expr$line1 > x))) min(comm.expr$line1[idx]) else NA_integer_,
    integer(1L)
  )
  comm.comm$match <- ifelse(
    is.na(comm.comm$match),
    comm.expr$id[first.or.only][match(comm.comm$assign.to.next, comm.expr$line1[first.or.only])],
    comm.comm$match
  )
  # Assign comments to matching expression in attributes

  for(i in seq_along(comm.comm$match)) {
    if(is.na(comm.comm$match[[i]])) next
    expr.pos <- which(comm.notcomm$id == comm.comm$match[[i]])
    if(!identical(length(expr.pos), 1L))
      stop("Internal Error; contact maintainer.")  # nocov
    if(!is.null(expr[[expr.pos]])) {
      # names are registered in global pool, so you can only attach attributes
      # to as single unique in memory instance, irrespective of where or how
      # many times a name occurs in an expression.  Because of this, we must
      # turn names that we want to attach comments to into simple language by
      # adding parens.  Note this changes structure of expression but hopefully
      # doesn't mess anything up later on...

      if(is.name(expr[[expr.pos]])) {
        expr[[expr.pos]] <- call("(", expr[[expr.pos]])
        attr(expr[[expr.pos]], "unitizer_parse_symb") <- TRUE
      }
      attr(expr[[expr.pos]], "comment") <-
        c(attr(expr[[expr.pos]], "comment"), comm.comm$text[[i]])
    }
  }
  expr
}
# Need this to pass R CMD check; problems likely caused by `transform` and
# `subset`.

if(getRversion() >= "2.15.1")  utils::globalVariables(c("id", "parent", "token", "line2"))

# Recursively Descends Through a Parsed Expression and Assigns Comments
#
# In order to implement this we had to make several assumptions about the
# behaviour of \code{`\link{getParseData}`}.  In particular:
# \itemize{
#   \item Top level comments show up with negative ids, but are top level
#     for all intents and purposes
#   \item All content tokens (i.e. anything other than brackets, commas,
#     etc.) are contained inside an \code{`expr`}, even if the only thing the
#     `expr` contains is a simple constant (note some exceptions exist to
#     this (search for FUCK in the source).
#   \item Comments are not content tokens and can exist on the top level
#     without being wrapped in an \code{`expr`}
#   \item The only tokens that count as elements in an expression are
#     opening brackets and \code{`expr`}; this assumption is necessary
#     to allow mapping the parsed data back to the expression.  What
#     confuses the issue a bit is that operators show up at the top level,
#     but you can actually
#     ignore them.  Also, parentheses should only be kept if they are the
#     topmost item, as otherwise they are part of a function call and
#     should be ignored.
#   \item Comments inside function formals are not assigned to the formals
#     proper
#   \item `exprlist` tokens are removed completely b/c as far as we can
#     tell they are not part of the parsed object (but exist in parse
#     data).
#   \item known issue: comments in formals after a line break are assigned
#     to the body of the function as opposed to \code{`function`}, but this
#     should not be apparent in common use.
#   \item you cannot attach comments to \code{`NULL`}, if you must use
#     \code{`(NULL)`}.  This is a feature, as it proivdes a way to put
#     comments in the file without them showing up during \code{`unitizer`}
#     use.
# }
# Note that as a result of this trial and error interpretation of
# \code{`\link{getParseData}`} it is likely that comment parsing is
# not 100 percent robust.
#
# Due to some reference weirdness going on when dealing directly with
# expressions had to change this function to accept text/file rather
# than an expression as an input (but even that didn't fix it!!!)
#
# @keywords internal
# @aliases parse_tests
# @seealso comments_assign, getParseData, parse
# @param file containing code to parse with comments
# @param text optional, text to parse if \code{`file`} is not specified
# @param comment logical(1L) whether to try to get comments
# @return an expression with comments retrieved from the parse attached
#   to the appropriate sub-expressions/calls as a \dQuote{comment} \code{`\link{attr}`}

parse_with_comments <- function(file, text=NULL) {
  # Looping to deal with issue #41

  res <- parse_dat_get(file, text)
  parse.dat.raw <- res$dat
  expr <- res$expr
  if(!length(expr)) return(expr)

  # Now proceed with actual parsing

  expr <- comm_reset(expr)  # hack to deal with issues with expressions retaining previous assigned comments (need to examine this further)

  parse.dat.raw.1 <- transform(parse.dat.raw, parent=ifelse(parent < 0, 0L, parent))  # set negative ids to be top level parents
  ancestry <- with(parse.dat.raw.1, ancestry_descend(id, parent, 0L))
  parse.dat <- prsdat_fix_exprlist(parse.dat.raw.1, ancestry)

  if(is.null(parse.dat)) stop("Argument `expr` did not contain any parse data")
  if(!is.data.frame(parse.dat)) stop("Argument `expr` produced parse data that is not a data frame")
  if(!nrow(parse.dat)) return(expr)
  if(!identical(names(parse.dat), c("line1", "col1", "line2", "col2", "id", "parent", "token",  "terminal", "text")))
    stop("Argument `expr` produced parse data with unexpected column names")
  if(!identical(unname(vapply(parse.dat, class, "")), c("integer", "integer", "integer", "integer", "integer", "integer",  "character", "logical", "character")))
    stop("Argument `expr` produced data with unexpected column data types")
  if(!all(parse.dat$token %in% unlist(tk.lst))) {
    # nocov start
    # shouldn't happen, can't test
    stop(
      "Internal Error: unexpected tokens in parse data (",
        paste0(parse.dat$token[!parse.dat$token %in% unlist(tk.lst)]) ,
        "); contact maintainer."
    )
    # nocov end
  }

  prsdat_recurse <- function(expr, parse.dat, top.level) {
    if(identical(parse.dat$token[[1L]], "FUNCTION")) parse.dat <- prsdat_fix_fun(parse.dat)
    if(identical(parse.dat$token[[1L]], "FOR")) parse.dat <- prsdat_fix_for(parse.dat)
    if(identical(parse.dat$token[[1L]], "IF")) parse.dat <- prsdat_fix_if(parse.dat)
    if(identical(parse.dat$token[[1L]], "WHILE")) parse.dat <- prsdat_fix_while(parse.dat)

    par.ids <- with(parse.dat, top_level_parse_parents(id, parent, top.level))
    parse.dat.split <- split(parse.dat, par.ids)
    prsdat.par <- parse.dat.split[[as.character(top.level)]]
    prsdat.children <- parse.dat.split[names(parse.dat.split) != as.character(top.level)]

    # Check that the parse data doesn't break the assumptions we've made,
    # particularly, that for any child section, there are no overlapping
    # sections at the top level

    line.dat <- vapply(
      prsdat.children,
      function(x) with(x, c(max=max(line2), min=min(line1))), c(max=0L, min=0L)
    )
    col.dat <- vapply(
      seq_along(prsdat.children),
      function(i)
        with(
          prsdat.children[[i]],
          {
            c(
              max=max(col2[which(line2 == line.dat["max", i])]),
              min=min(col1[which(line1 == line.dat["min", i])])
        ) } ),
      c(max=0L, min=0L)
    )
    if(
      any(head(line.dat["max", ], -1L) > tail(line.dat["min", ], -1L)) ||
      any(
        head(line.dat["max", ], -1L) == tail(line.dat["min", ], -1L) &
        head(col.dat["max", ], -1L) >= tail(col.dat["min", ], -1L)
    ) ) {
      # nocov start
      # shouldn't happen, can't test

      stop("Internal Error: expression parse data overlapping; contact maintainer")
      # nocov end
    }
    # For each parent expression, assign comments; parent expressions that include
    # a function definition have to exclude the formals part (which is a pairlist)
    # because the `getParseData` output does not produce a parent element for the
    # formals; in practice this shouldn't have any impact because test items will
    # never be at such a low level (i.e. any comments at this level would never
    # be shown anyway).

    assignable.elems <- vapply(
      expr,
      function(x) !identical(typeof(x), "pairlist") && !any("srcref" == class(x)),
      logical(1L)
    )
    if(!is.call(expr) && !is.expression(expr)) {
      if(!length(assignable.elems) %in% c(1L))
        stop(  # nocov start
          "Internal Error: expression is terminal token yet multiple ",
          "assignable elems; contact maintainer."
        )      # nocov end
      if(isTRUE(assignable.elems)) expr <- comments_assign(expr, prsdat.par)
    } else if (!is.null(expr)) {
      expr[assignable.elems] <- comments_assign(expr[assignable.elems], prsdat.par)
    }
    # Now do the same for the child expression by recursively calling this
    # function until there are no children left, but need to be careful here
    # because we only need to call this for non-terminal leaves of the parse
    # tree.  Simply removing non terminal leaves from call should leave
    # everything in correct order because the only time there are order
    # mismatches are with infix operators and those are terminal leaves anyway.

    if(
      !any(
        vapply(
          prsdat.children,
          function(child) with(child, "COMMENT" %in% token),
          logical(1L)
    ) ) ) return(expr)

    # stuff that corresponds to elements in `expr`, will re-order to match `expr`
    prsdat.par.red <- prsdat_reduce(prsdat.par)
    if(!identical(nrow(prsdat.par.red), length(which(assignable.elems)))) {
      # nocov start
      stop(
        "Internal Error: mismatch between expression and parse data; ",
        "contact maintainer."
      )
      # nocov end
    }
    j <- 1
    if(!is.expression(expr) && !is.call(expr)) {
      # nocov start
      # shouldn't happen, can't test

      if(term.len <- length(which(!prsdat.par.red$terminal)) > 1L) {
        stop(
          "Internal Error: terminal expression has more than one token, ",
          "contact maintainer."
        )
      } else if (term.len) {
        expr <- Recall(
          expr, prsdat.children[[j]], as.integer(names(prsdat.children)[[j]])
        )
      }
      # nocov end
    } else {
      for(i in 1:nrow(prsdat.par.red)) {
        if(prsdat.par.red$terminal[[i]]) next
        expr[assignable.elems][[i]] <-
          Recall(expr[assignable.elems][[i]], prsdat.children[[j]], as.integer(names(prsdat.children)[[j]]))
        j <- j + 1
    } }
    expr
  }
  prsdat_recurse(expr, parse.dat, top.level=0L)
}
# Handle the issues with needing to run parse twice due to weird getParseData
# output

parse_dat_get <- function(file, text) {
  parse.dat.raw <- NULL
  for(i in 1:2) {
    if(!is.null(text)) {
      if(!missing(file))
        # nocov start
        stop("Internal Error: cannot specify both `file` and `text` arguments.")
        # nocov end
      expr <- try(parse(text=text, keep.source=TRUE))
    } else {
      expr <- try(parse(file, keep.source=TRUE))
    }
    if(inherits(expr, "try-error")) stop("parsing failed")
    if(!length(expr)) break
    parse.dat.raw <- getParseData(expr)
    if(is.null(parse.dat.raw)) break

    if(!nrow(parse.dat.raw))
      stop("Internal Error: parse data mismatch; contact maintainer.") # nocov
    parse.dat.check <- cbind(
      parse.dat.raw[
        match(parse.dat.raw$parent, parse.dat.raw$id), c("line1", "col1")
      ],
      setNames(
        parse.dat.raw[, c("line1", "col1")], c("line1.child", "col1.child")
      )
    )
    if(
      length(
        with(parse.dat.check,
          which(
            line1.child < line1 |
            (line1.child == line1) & col1.child < col1
      ) ) )
    ) {
      # Parsing is not self consistent; some child items have for parents items
      # that are lexically posterior
      if(identical(i, 1L))  # Try again once to see if that fixes it
        next
      stop("Internal Error: cannot retrieve self consistent parse data") # nocov
    }
    break  # Parsing worked as expected
  }
  list(expr=expr, dat=parse.dat.raw)
}

parse_tests <- function(file, comments=TRUE, text=NULL) {

  if(!isTRUE(comments) && !identical(comments, FALSE))
    stop("Argument `comments` must be TRUE or FALSE")
  if(!is.null(text) && !missing(file))
    stop("If Argument `text` is specified, argument `file` must be missing")

  parsed <- NULL
  if(comments) {
    parsed <- try(parse_with_comments(file, text))
    if(inherits(parsed, "try-error")) {
      if(
        identical(
          conditionMessage(attr(parsed, "condition")),
          "parsing failed"
        )
      )
        stop("Unable to parse test file; see previous messages")
    } else {
      return(parsed)
  } }
  # Either no comment mode, or couldn't extract in comment mode

  if(inherits(parsed, "try-error"))
    warning(
      "Unable to recover comments in parse; attempting simple parse",
      immediate.=TRUE
    )
  if(is.null(text)) {
    parse(file, keep.source=FALSE)
  } else parse(text=text, keep.source=FALSE)
}
# Need this to pass R CMD check; problems likely caused by `transform` and
# `subset`.

if(getRversion() >= "2.15.1")  utils::globalVariables(c("token"))

# Reduce Parsed Data to Just the Things That should Exist In Expression
#
# additionally, special handling due to function and formals not getting wrapped
# in their own `expr` (why the FUCK!!!!)
#
# @aliases prsdat_remove_fun
# @param parse.dat top level parse data
# @return parse data reduced to key elements, ordered so that infix operators
#   show up first instead of in middle

prsdat_reduce <- function(parse.dat) {
  parse.dat.red <- subset(
    parse.dat,
    !token %in% c(tk.lst$brac.close, tk.lst$unassign, tk.lst$seps, "COMMENT") &
    !(token == "'('" & 1L:length(token) == 2L)
  )
  # at this point, must be all expressions, an opening bracket, or an operator of some
  # sort, and iff the operator is @ or $, or if there is only one item in the data frame
  # then it can be NUM_CONST or STR_CONST or symbol for the second one
  if(any(c("'$'", "'@'") %in% parse.dat.red$token)) {
    if(!identical(nrow(parse.dat.red), 3L)) {
      # nocov start
      stop(
        "Internal Error: top level statement with `@` or `$` must be three ",
        "elements long"
      )
      # nocov end
    }
    if(!identical(parse.dat.red$token[[1L]], "expr")) {
      # nocov start
      stop("Internal Error: left argument to `@` or `$` must be an expression")
      # nocov end
    }
    if(
      identical(parse.dat.red$token, "'@'") &&
      !identical(parse.dat.red$token[[3L]], "SLOT")
    ) {
      # nocov start
      stop("Internal Error: right argument to `@` must be SLOT")
      # nocov end
    }
    if(
      identical(parse.dat.red$token, "'$'") &&
      !identical(parse.dat.red$token[[3L]], "SYMBOL")
    ) {
      # nocov start
      stop("Internal Error: right argument to `$` must be SYMBOL")
      # nocov end
    }
  } else if (nrow(parse.dat.red) == 1L) {
    if(
      !parse.dat.red$token[[1L]] %in%
      c("expr", tk.lst$non.exps, tk.lst$non.exps.extra, tk.lst$brac.open)
    ) {
      # nocov start
      stop(
        "Internal Error: single element parent levels must be symbol or ",
        "constant or expr"
      )
      # nocov end
    }
  } else if (
    length(
      which(
        parse.dat.red$token %in%
        c(tk.lst$exps, tk.lst$non.exps, tk.lst$non.exps.extra)
    ) ) < nrow(parse.dat.red) - 1L
  ) {
    # nocov start
    stop(
      "Internal Error: in most cases all but at most one token must be of ",
      "type `expr` or `exprlist`; contact maintainer."
    )
    # nocov end
  }
  parse.dat.red[
    order(
      parse.dat.red$token %in%
      c(tk.lst$exps, tk.lst$non.exps, tk.lst$non.exps.extra)
  ), ]
}
# Need this to pass R CMD check; problems likely caused by `transform` and
# `subset`.

if(getRversion() >= "2.15.1")  utils::globalVariables(c("id", "token"))

# Functions to Adjust Parse Data To Match Expression
#
# \itemize{
#   \item \code{`prsdat_fix_fun`} extract all comments from formals and brings them
#     up a level, and then removes formals
#   \item \code{`prsdat_fix_for`} brings contents of `forcond` to same level as
#     `for` to match up with expression
#   \item \code{`prsdat_fix_for`} extracts expression from the condition (though
#     apparently not from `ifcond`)
#   \item \code{`prsdat_fix_exprlist`} excises the \code{`exprlist`} portions of
#     \code{`exprlist`} as those don't exist in the expressions proper; they
#     don't do anything, and have extraneous semi colons.  We need to remove
#     them, and then make sure all their children become children of the
#     parent of the exprlist
# parent
#   \item \code{`prsdat_find_paren`} returns locations of first set
#     of open and close parens
# }
# @aliases prsdat_fix_for, prsdat_find_paren, prsdat_fix_exprlist
# @param parse.dat a data frame of the type produced by \code{`\link{getParseData}`}
# @return \itemize{
#   \item for \code{`parsdat_fix*`}, a data frame of the type produced by \code{`\link{getParseData}`}
#   \item for \code{`parsdat_find_paren`}, a length two integer vector with the ids of the parens
# }

prsdat_fix_fun <- function(parse.dat) {
  if(!identical(parse.dat$token[[1L]], "FUNCTION"))
    stop("Argument `parse.dat` must start with a 'FUNCTION' token.")
  subset(
    parse.dat,
    1L:nrow(parse.dat) > which(id == prsdat_find_paren(parse.dat)[[2]]) | token == "COMMENT" | 1L:nrow(parse.dat) == 1L
  )
}
# Need this to pass R CMD check; problems likely caused by `transform` and
# `subset`.

if(getRversion() >= "2.15.1")  utils::globalVariables(c("id", "parent", "token"))

prsdat_fix_for <- function(parse.dat) {
  if(!identical(parse.dat$token[[1L]], "FOR"))
    stop("Argument `parse.dat` must start with a 'FOR' token.")
  if(!identical(parse.dat$token[parse.dat$token != "COMMENT"][[2]], "forcond"))
    stop("Argument `parse.dat` does not have token `forcond` in expected location")
  if(!identical(length(which(parse.dat$token == "forcond")), 1L))
    stop("Argument `parse.dat` should have exactly one `forcond` token")
  par.range <- prsdat_find_paren(parse.dat)
  par.level <- subset(parse.dat, id == par.range[[1]])$parent
  tokens <- tail(head(subset(parse.dat, parent==par.level)$token, -1L), -1L)
  tokens.no.comm <- tokens[tokens != "COMMENT"]
  if(!identical(length(tokens.no.comm), 3L))
    stop("Logic error: `forcond` should have three elements")
  if(!identical(which(tokens.no.comm == "IN"), 2L))
    stop("Logic error: `forcond` should have exactly one 'IN' in position 2L")
  parse.dat.mod <- subset(parse.dat, !token %in% c("forcond", "IN") & ! id %in% par.range)
  `[<-`(parse.dat.mod, parse.dat.mod$parent == par.level, "parent", parse.dat[1L, "parent"])
}
# Need this to pass R CMD check; problems likely caused by `transform` and
# `subset`.

if(getRversion() >= "2.15.1")  utils::globalVariables(c("id", "parent", "token"))

prsdat_fix_simple <- function(parse.dat, tok) {
  if(! tok %in% c("IF", "WHILE"))
    # nocov start
    stop(
      "Internal Error, this function only supports 'IF' and 'WHILE' tokens"
    )
    # nocov end
  if(!identical(parse.dat$token[[1L]], tok))
    stop("Argument `parse.dat` must start with an '", tok, "' token.")
  par.id <- parse.dat$parent[[1L]]
  par.range <- prsdat_find_paren(parse.dat)
  early.tokens <- parse.dat$token[1L:(which(parse.dat$id == par.range[[1L]]) - 1L)]
  if(
    any(! early.tokens %in% c(tok, "COMMENT")) ||
    !identical(length(which(early.tokens == tok)), 1L)
  )
    # nocov start
    stop(
      "Internal Error: could not parse ", tok, " statement; contact maintainer."
    )
    # nocov end
  parse.delete <-
    subset(parse.dat, parent == par.id & token %in% c("'('", "')'", "ELSE"))
  if(!nrow(parse.delete) %in% c(2L, 3L))
    # nocov start
    stop(
      "Internal Error: unexpected number of ", tok,
      " statement sub-components; contact maintainer."
    )
    # nocov end
  if(any(parse.dat$parent %in% parse.delete$id))
    # nocov start
    stop(
      "Internal Error: unexpected parent relationships in ", tok,
      " statement; contact maintainer."
    )
    # nocov end
  subset(parse.dat, ! id %in% parse.delete$id)
}
prsdat_fix_if <- function(parse.dat) prsdat_fix_simple(parse.dat, "IF")
prsdat_fix_while <- function(parse.dat) prsdat_fix_simple(parse.dat, "WHILE")

prsdat_find_paren <- function(parse.dat) {
  par.clos.pos <- match("')'", parse.dat$token)
  if(is.na(par.clos.pos))
    # nocov start
    stop(
      "Internal Error; failed attempting to parse function block; contact  ",
      "maintainer"
    )
    # nocov end
  par.op.pos <- match("'('", parse.dat$token[1:par.clos.pos])
  if(is.na(par.op.pos))
  if(
    !identical(par.op.pos, 2L) &&
    !identical(unique(parse.dat$token[2L:(par.op.pos - 1L)]), "COMMENT")
  )
    # nocov start
    stop(
      "Internal Error; failed attempting to `for` function block; contact ",
      "maintainer"
    )
    # nocov end
  c(open=parse.dat$id[[par.op.pos]], close=parse.dat$id[[par.clos.pos]])
}
prsdat_fix_exprlist <- function(parse.dat, ancestry) {

  z <- ancestry
  z[, "level"] <- z[match(parse.dat$id, z[, "children"]), "level"]
  # order by level to make sure we remove exprlists in correct order
  lev.ord <- order(z[, "level"])
  dat.ord <- parse.dat[lev.ord, ]
  ind.all <- seq.int(nrow(dat.ord))
  # map parents vs. position in ordered list
  par.map <- list2env(split(ind.all, dat.ord[["parent"]]))

  dat.exprlist <- which(dat.ord[["token"]] == "exprlist")
  ind.exp <- seq_along(dat.exprlist)
  ind.exclude <- logical(length(ind.all))

  if(length(dat.exprlist)) {
    dat.ord <- within(
      dat.ord,
      {
        for(exprlist.ind in dat.exprlist) {

          # Find first `exprlist`

          exprlist.par <- parent[[exprlist.ind]]

          # Promote all children of exprlist and remove semi-colons and actual
          # exprlist.  This requires updating the value of the parent column in
          # `dat.ord`, and then re-assigning the parent ship relationship

          exprlist.par.chr <- as.character(exprlist.par)
          exprlist.id.chr <- as.character(id[[exprlist.ind]])
          exprlist.children <- par.map[[exprlist.id.chr]]

          # semi colons with exprlist as parent need to be discarded

          semicol.ind <- exprlist.children[which(token[exprlist.children] == "';'")]

          # change exprlist children parent to exprlist parent

          parent[exprlist.children] <- exprlist.par

          # Update mapping to reflect new parentship

          par.map[[exprlist.par.chr]] <<- c(
            par.map[[exprlist.par.chr]],
            par.map[[exprlist.id.chr]]
          )
          par.map[[exprlist.id.chr]] <<- NULL

          # extend exclusion list

          ind.exclude[c(exprlist.ind, semicol.ind)] <<- TRUE
        }
        rm(
          exprlist.par, exprlist.par.chr, exprlist.id.chr, exprlist.children,
          exprlist.ind, semicol.ind
  ) } ) }
  # Now actually remove the exprlist and semi colons, and re-order

  parse.dat.mod <-
    dat.ord[order(lev.ord), ][which(!ind.exclude[order(lev.ord)]), ]
  if(!all(parse.dat.mod$parent %in% c(0, parse.dat.mod$id)))
    stop("Internal Error: `exprlist` excision did not work!")  # nocov
  parse.dat.mod
}
# Removes Symbol Marker Used To Hold Comments

symb_mark_rem <- function(x) {
  if(isTRUE(attr(x, "unitizer_parse_symb"))) {
    if(length(x) != 2L || x[[1L]] != as.name("(") || !is.name(x[[2L]])) {
      stop(  # nocov start
        "Internal Error: Unexpected structure for object with language with ",
        "'unitizer_parse_symb' attribute attached; contact maintainer"
    ) }      # nocov end
    x <- x[[2L]]
  }
  x
}
# Utility Function to Extract Comments From Expression
#
# Note that when dealing with expressions the very first item will typically
# be NULL to allow for logic that works with nested structures.
#
# \code{comm_and_call_extract} also pulls out a cleaned up version of the call
# along with the comments, but the comments come out in a vector instead of a
# list showing the structure where the comments were pulled from.
#
# Used mostly for testing purposes.

comm_extract <- function(x) {

  if(missing(x)) return(list(NULL))
  comm <- attr(x, "comment")
  x <- symb_mark_rem(x)             # get rid of comment container
  if(missing(x)) return(list(NULL)) # need to do this twice because missing args that are parsed aren't necessarily recognized as missing immediately

  if(is.expression(x) || length(x) > 1L) {
    return(c(list(comm), lapply(x, comm_extract)))
  } else {
    return(list(comm))
} }

comm_and_call_extract <- function(x) {

  comments <- character()

  rec <- function(call) {
    if(missing(call)) return(call)
    comm <- attr(call, "comment")
    if(!is.null(comm)) {
      comments <<- c(comments, comm)
      attr(call, "comment") <- NULL
    }
    call.clean <- symb_mark_rem(call)             # get rid of comment container
    if(missing(call.clean)) return(call.clean)    # need to do this twice because missing args that are parsed aren't necessarily recognized as missing immediately

    if(is.expression(call.clean) || length(call.clean) > 1L) {
      for(i in seq_along(call.clean)) {
        call.sub <- call.clean[[i]]
        if(!missing(call.sub) && !is.null(call.sub)) call.clean[[i]] <- rec(call.clean[[i]])
    } }
    call.clean
  }
  list(call=rec(x), comments=comments)
}
# Utility Function to Reset Comments
#
# Required due to bizarre behavior (bug?) where some expression attributes
# appear to have reference like behavior even when they are re-generated
# from scratch from a text expression (wtf, really).

comm_reset <- function(x) {
  if(is.null(x) || is.name(x) && !nchar(x)) return(x)
  attr(x, "comment") <- NULL
  if(is.pairlist(x)) return(x)
  if(length(x) > 1L || is.expression(x))
    for(i in seq_along(x)) if(!is.null(x[[i]])) x[[i]] <- Recall(x[[i]])
  x
}
# Listing on known tokens
#
# As of this writing, the following tokens from \file{src/main/gram.c} are
# not handled:
#
#      [,1]           [,2]             [,3]                [,4]
# [1,] "'\\n'"        "cr"             "ifcond"            "sub"
# [2,] "'%'"          "END_OF_INPUT"   "INCOMPLETE_STRING" "sublist"
# [3,] "$accept"      "equal_assign"
# [4,] "$end"         "error"          "LINE_DIRECTIVE"    "TILDE"
# [5,] "$undefined"   "ERROR"          "LOW"               "UMINUS"
# [6,] "COLON_ASSIGN" "expr_or_assign" "NOT"               "UNOT"
# [7,] "cond"         "formlist"       "prog"              "UPLUS"
#
# So far, we have not been able to produce \code{`getParseData`} data frames
# that contain them.  It may not be possible to do so for all of them.  For
# example, \code{`INCOMPLETE_STRING`} shows up during a parse error, so could
# never be part of a fully parsed expression.

tk.lst <- list(
  comment="COMMENT",
  brac.close=c("'}'", "']'", "')'"),
  brac.open=c("'{'", "'['", "'('", "LBB"),
  exps=c("expr", "exprlist"),
  seps=c("','", "';'"),                                          # no comments on these as they are just removed
  non.exps=c(                                                    # in addition to `expr`, these are the ones that can get comments attached
    "SYMBOL", "STR_CONST", "NUM_CONST", "NULL_CONST",
    "SLOT", "NEXT", "BREAK", "SYMBOL_FUNCTION_CALL"
  ),
  non.exps.extra=c(                                              # these can also get comments attached, but shouldn't be at the end of a parse data block
    "FUNCTION", "FOR",
    "IF", "REPEAT", "WHILE", "SYMBOL_PACKAGE"                    # not 100% sure SYMBOL_PACKAGE belongs here; it can't possibly have comments right after it on the same line
  ),
  ops=c(
    paste0(
      "'",
      c("-", "+", "!", "~", "?", ":", "*", "/", "^", "$", "@"),
      "'"
    ),
    "SPECIAL", "GT", "GE", "LT", "LE", "EQ", "NE", "AND", "AND2",
    "OR", "OR2", "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN"
  ),
  ops.other=c("NS_GET", "NS_GET_INT"),                           # note these should never show up at top level
  unassign=c(                                                    # these cannot have comments attached to them
    "EQ_SUB", "SYMBOL_SUB", "EQ_FORMALS", "SYMBOL_FORMALS",
    "IN", "forcond", "ELSE"
  )
)
