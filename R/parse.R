
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


#' Searches Through Generation Until it Finds Top Level
#'
#' Returns the id of the ancestor that is just before \code{`id==0`}
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
  ancestry_descend <- function(par.id) {
    par.id <- abs(par.id)
    if(identical(par.id, top.level)) return(par.id)
    new.id <- abs(par.ids[match(par.id, ids)])
    if(identical(new.id, top.level)) return(par.id) else if (is.na(new.id)) return(new.id)
    Recall(new.id)
  }
  vapply(par.ids, ancestry_descend, integer(1L))
}


parse_data_assign <- function(expr, parse.data, top.level=0L) {
  # if(!is.language())  # NOT CLEAR IF WE NEED / CAN TEST EXPRESSION
  if(!is.data.frame(parse.data)) stop("Argument `parse.data` must be a data frame.")
  if(!identical(names(parse.data), c("line1", "col1", "line2", "col2", "id", "parent", "token",  "terminal", "text")))
    stop("Argument `parse.data` does not have the expected column names")
  if(!identical(cat(deparse(unname(vapply(dat, class, "")))), c("integer", "integer", "integer", "integer", "integer", "integer",  "character", "logical", "character")))
    stop("Argument `parse.data` does not have the expected column data types")
  #  MOVE IN PARSE DATA GENERATION STEP INTO HERE
  if(identical(top.level, 0L)) parse.data <- transform(parse.data, parent=ifelse(parent < 0, 0L, parent))

  comments_assign <- function(expr, comment.dat) {
    # what comments are on same line as something else

    comm.comm <- subset(comment.dat, token=="COMMENT")
    comm.expr <- subset(comment.dat, token=="expr")

    comment.expr <- transform(  # identify whether a token is the first or last on it's line
      comment.expr, 
      first.last.on.line=ave(
        col1, line1, 
        FUN=function(x) if(identical(length(x), 1L)) 3L else ifelse(x == max(x), 2L, ifelse(x == min(x), 1L, 0L))
    ) )
    # For each comment on a line that also has an expression, find the expression 
    # that is also on that line

    comm.comm <- transform(comm.comm, assign.to.prev=match(line1, comm.expr$line2))
    comm.comm$match <- with(comm.expr,  {
      last.or.only <- first.last.on.line %in% 2L:3L
      first.or.only <- first.last.on.line %in% c(1L, 3L)
      id[last.or.only][match(comm.comm$assign.to.prev, line1[last.or.only]]))
    } )  
    # For each comment on its own line, find the expression that follows it
    
    transform(comm.comm, assign.to.next=vapply(line1, function(x) min(comm.expr$line1[comm.expr$line1 > x]), integer(1L)))
    comm.comm$match <- with(comm.expr, 
      ifelse(
        is.na(comm.comm$match), 
        id[first.or.only][match(comm.comm$assign.to.next, line1[first.or.only])],
        comm.comm$match
    ) )
    # Assign comments to matching expression in attributes
    
    with(
      subset(comm.comm, !is.na(match)), {
        for(i in seq_along(match)) {
          expr.pos <- which(comm.expr == match[[i]])
          if(!identical(length(expr.pos), 1L)) stop("Logic Error; contact maintener.")
          attr(expr[[expr.pos]], "comment") <- c(attr(expr[[expr.pos]], "comment"), text[[i]])
    } } )
    expr
  }
  prsdat_recurse <- function(expr, parse.dat, top.level) {
    
    par.ids <- with(parse.data, top_level_parse_parents(id, parent))
    parse.data.split <- split(parse.dat, par.ids)
    prsdat.par <- parse.data.split[[as.character(parent)]]
    if(!identical(sort(unique(prsdat.par$token)), c("COMMENT", "expr")))
      stop("Logic Error, unexpected token values; contact maintainer.")
    prsdat.children <- parse.data.split[names(parse.data.split) != as.character(parent)]

    if(!identical(length(expr), length(prsdat.children))) stop("Logic Error: mismatch between parse data and expression")

    expr <- comments_assign(expr, prsdat.par)
    
    # NEED TO GUARANTEE expr AND prsdat.children ARE SAME ORDER!!!

    for(i in seq_along(prsdat.children)) {
      expr[[i]] <- Recall(expr[[i]], prsdat.children[[i]], as.integer(names(prsdat.children)[[i]]))
    }
    expr
} }



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

