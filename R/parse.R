
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
# top level parents so we can do this recursively for testor_sect
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


#' Searches Through Generations Until it Finds Top Level
#'
#' Returns the id of the ancestor that is just before \code{`top.level`},
#' or \code{`top.level`} if the parent already is \code{`top.level`}. The
#' idea is to reduce the set of parents for all elements to just the top
#' level parents as this allows us to split the parse data into sections,
#' including the calls that were direct children of top level, as well as
#' the children to those sections.
#' 
#' @param ids integer the ids to look through
#' @param par.ids integer the parenthood relationships
#' @param top.level the id of the top level
#' @return integer the top level parent ids for \code{`ids`}


top_level_parse_parents <- function(ids, par.ids, top.level=0L) {
  if(!is.integer(ids) || !is.integer(par.ids) || !identical(length(ids), length(par.ids)))
    stop("Arguments `ids` and `par.ids` must be equal length integer vectors")
  if(!identical(length(setdiff(abs(par.ids), c(ids, top.level))), 0L))
    stop("Argument `par.ids` contains ids not in `ids`.")
  if(!is.integer(top.level) && !identical(length(top.level), 1L))
    stop("Argument `top.level` must be a one length integer")
  if(identical(top.level, 0L)) {
    par.ids <- pmax(0L, par.ids)
  } else if (any(par.ids) < 0) {
    stop("Argument `par.ids` contains values less than zero, but that is only allowed when `top.level` == 0L")
  }
  ancestry_descend <- function(par.id) {
    par.id <- abs(par.id)
    if(identical(par.id, top.level)) return(par.id)
    new.id <- abs(par.ids[match(par.id, ids)])
    if(identical(new.id, top.level)) return(par.id) else if (is.na(new.id)) return(new.id)
    Recall(new.id)
  }
  vapply(par.ids, ancestry_descend, integer(1L))
}

#' Assign Comments From Parse Data to Expression Elements
#' 
#' Based on parse data from \code{`\link{getParseData}`}, figures
#' out what comments belong to what expression.  If a comment is
#' on the same line as an expression, the comment is assigned to that
#' expression (or whatever the nearest expression is on that line if
#' there is more than one).  If a comment is on it's own line,
#' then the match is done to the next expression.
#' 
#' The expectation is that only "top level" expressions will
#' be submitted as part of `comment.dat` (i.e. they all have
#' the same parent, they don't strictly have to be top.level).
#' 
#' @param expr and expression
#' @param comment.dat a data frame derived from \code{`\link{getParseData}`}
#' @return an expression with comments attached as attributes to each 
#'   expression component

comments_assign <- function(expr, comment.dat) {
  if(!identical(length(unique(comment.dat$parent)), 1L))
    stop("Logic Error: there were multiple parent ids in argument `comment.dat`; this should not happen")

  # what comments are on same line as something else

  comm.comm <- subset(comment.dat, token=="COMMENT")
  comm.expr <- subset(comment.dat, token=="expr")
  comm.notcomm <- subset(comment.dat, (token %in% c("'['", "'{'", "expr")) | (token == "'('" & 1:nrow(comment.dat) == 1L))
  
  if(!identical(nrow(comm.notcomm), length(expr))) {
    stop("Argument `expr` length cannot be matched with values in `comment.dat`")
  }

  comm.expr <- transform(  # identify whether a token is the first or last on it's line
    comm.expr, 
    first.last.on.line=ave(
      col1, line1, 
      FUN=function(x) if(identical(length(x), 1L)) 3L else ifelse(x == max(x), 2L, ifelse(x == min(x), 1L, 0L))
  ) )
  # For each comment on a line that also has an expression, find the expression 
  # that is also on that line

  comm.comm <- transform(comm.comm, assign.to.prev=comm.expr$line2[match(line1, comm.expr$line2)])
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
    if(!identical(length(expr.pos), 1L)) stop("Logic Error; contact maintainer.")
    attr(expr[[expr.pos]], "comment") <- c(attr(expr[[expr.pos]], "comment"), comm.comm$text[[i]])
  }
  expr
}
#' Recursively Descends Through a Parsed Expression and Assigns Comments
#' 
#' In order to implement this we had to make several assumptions about the
#' behaviour of \code{`\link{getParseData}`}.  In particular:
#' \itemize{
#'   \item Top level comments show up with negative ids, but are top level
#'     for all intents and purposes
#'   \item All content tokens (i.e. anything other than brackets, commas,
#'     etc.) are contained inside an \code{`expr`}, even if the only thing the
#'     `expr` contains is a simple constant.
#'   \item Comments are not content tokens and can exist on the top level
#'     without being wrapped in an \code{`expr`}
#'   \item The only tokens that count as elements in an expression are
#'     opening brackets and \code{`expr`}; this assumption is necessary
#'     to allow mapping the parsed data back to the expression.  What
#'     confuses the issue a bit is that operators (e.g. \code{`:`} or
#'     \code{`%in%`}, etc.) show up at the top level, but you can actually
#'     ignore them.  Also, parantheses should only be kept if they are the
#'     topmost item, as otherwise they are part of a function call and
#'     should be ignored.
#' }
#' Note that as a result of this trial and error interpretation of 
#' \code{`\link{getParseData}`} it is likely that comment parsing is
#' not 100% robust.
#' 
#' @seealso comments_assign, getParseData, parse
#' @param an expression produced by \code{`\link{parse}`}
#' @return an expression with comments retrieved from the parse attached
#'   to the appropriate sub-expressions/calls as a "comment" \code{`\link{attr}`}

parse_data_assign <- function(expr) {
  if(!is.expression(expr)) stop("Argument `expr` must be an expression")
  parse.dat <- getParseData(expr)
  if(is.null(parse.dat)) stop("Argument `expr` did not contain any parse data")
  if(!is.data.frame(parse.dat)) stop("Argument `expr` produced parse data that is not a data frame")
  if(!identical(names(parse.dat), c("line1", "col1", "line2", "col2", "id", "parent", "token",  "terminal", "text")))
    stop("Argument `expr` produced parse data with unexpected column names")
  if(!identical(unname(vapply(dat, class, "")), c("integer", "integer", "integer", "integer", "integer", "integer",  "character", "logical", "character")))
    stop("Argument `expr` produced data with unexpected column data types")
  parse.dat <- transform(parse.dat, parent=ifelse(parent < 0, 0L, parent))

  prsdat_recurse <- function(expr, parse.dat, top.level) {
    
    browser()
    par.ids <- with(parse.dat, top_level_parse_parents(id, parent, top.level))
    parse.dat.split <- split(parse.dat, par.ids)
    prsdat.par <- parse.dat.split[[as.character(top.level)]]
    # if(!identical(sort(unique(prsdat.par$token)), c("COMMENT", "expr", "")))
    #   stop("Logic Error, unexpected token values; contact maintainer.")
    prsdat.children <- parse.dat.split[names(parse.dat.split) != as.character(top.level)]

    if(!identical(length(expr), length(prsdat.children))) {
      browser()
      stop("Logic Error: mismatch between parse data and expression")
    }
    line.dat <- vapply(prsdat.children, function(x) c(max=max(x$line2), min=min(x$line1)), c(max=0L, min=0L))
    col.dat <- vapply(
      seq_along(prsdat.children), 
      function(i) c(
        max=max(subset(prsdat.children[[i]], line2==line.dat["max", i])$col2), 
        min=min(subset(prsdat.children[[i]], line1==line.dat["min", i])$col1)
      ),
      c(max=0L, min=0L)
    )
    if(
      any(head(line.dat["max", ], -1L) > tail(line.dat["min", ], -1L)) ||
      any(
        head(line.dat["max", ], -1L) == tail(line.dat["min", ], -1L) &
        head(col.dat["max", ], -1L) >= tail(col.dat["min", ], -1L))
    ) {
      stop("Logic Error: expression parse data overlapping; contact maintainer")
    }
    expr <- comments_assign(expr, prsdat.par)
    
    for(i in seq_along(prsdat.children)) {
      expr[[i]] <- Recall(expr[[i]], prsdat.children[[i]], as.integer(names(prsdat.children)[[i]]))
    }
    expr
  }
  prsdat_recurse(expr, parse.dat, top.level=0L)
}



# prs1 <- parse(text='# This is an early comment

# hello <- 25

# # multi
# # line
# # comment

# matrix(1:9, 3)  # and another!

# sample(1:10)

# # and this comment belongs to whom?

# runif(20) 
# print("woo")  # and I?')

# prs2 <- parse(text='# This is an early comment

# hello <- 25

# # multi
# # line
# # comment

# matrix(1:9, 3)  # and another!

# testor_sect("here is a section", {
#   # test that were not crazy

#   1 + 1 == 2   # TRUE hopefully
  
#   # Still not crazy

#   2 * 2 == 2 ^ 2
#   # Tada
# } )
# sample(1:10)

# # and this comment belongs to whom?

# runif(20) 
# print("woo")  # and I?')

