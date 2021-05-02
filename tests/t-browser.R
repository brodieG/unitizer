source(file.path("_helper", "init.R"))
source(file.path("aammrtf", "ref.R")); make_ref_obj_funs("refobjs")

options(unitizer.color = FALSE)
zero.env <- parent.env(.GlobalEnv)
obj.item <- new("unitizerItem", call = quote(1 + 1), env = new.env())
obj.item@data@value <- list(2)
obj.item@data@output <- c("two", "dos", "due")
obj.item@data@conditions <- new("conditionList", .items = list(simpleError("hello"),
    simpleWarning("What a warning")))
obj.item@data@message <- vapply(unitizer:::as.list(obj.item@data@conditions),
    conditionMessage, character(1L))
obj.item@data@aborted <- TRUE

# - "unitizerItem accessor functions work" -------------------------------------

obj.item$value
obj.item$output
obj.item$conditions

# Create a bunch of expressions for testing

exps1 <- expression(
  library(stats),
  unitizer_sect("Section 1", {
    1 + 1
    runif(20)
    stop("woohoo")
    "I'll be removed"
    "I too will be removed"
  }),
  unitizer_sect("Section 2", {
    "I three will be removed"
    sample(20)
}))
exps2 <- expression(
  library(stats),
  unitizer_sect("Section 1", {
    1 + 1
    runif(20)
    stop("woohoo")
    var <- 200
    matrix(1:9, 3)
    }),
  unitizer_sect("Section 2", {
    1 + 20
    var1 <- list(1, 2, 3)
    sample(20)
    matrix(1:9, ncol = 3)
    lm(x ~ y, data.frame(x = 1:10, y = c(5, 3, 3, 2, 1, 8, 2,
        1, 4, 1.5)))
}))
my.unitizer <- new("unitizer", id = 1, zero.env = zero.env)
coi(my.unitizer <- my.unitizer + exps1)
my.unitizer2 <- new("unitizer", id = 2, zero.env = zero.env)
# make previous items into reference items
my.unitizer2 <- my.unitizer2 + my.unitizer@items.new
# now add back items to compare
coi(my.unitizer2 <- my.unitizer2 + exps2)
unitizer.prepped <- unitizer:::browsePrep(my.unitizer2, mode = "unitize")

# NOTE: for some reason, changes in between revisions d9619db and a46e941
# should have caused the tests to fail, but didn't.  We did not notice
# failures until we ran tests quite a bit later at ca9f540364.  Not sure why
# this happened.  The failures were due to the order of tests changing because
# we moved ignored tests to be in the same sub-section as the subsequent non-
# ignored tests

# - "Can convert to data.frame" ------------------------------------------------

all.equal(unitizer:::as.data.frame(unitizer.prepped), rds("browse_df1"))

# - "unitizerBrowse correctly processes unitizer for display" ------------------

# force all tests to be reviewed so they will be shown
unitizer.prepped@mapping@reviewed <-
  rep(TRUE, length(unitizer.prepped@mapping@reviewed))
unitizer.prepped@mapping@review.val <-
  rep("Y", length(unitizer.prepped@mapping@reviewed))
all.equal(as.character(unitizer.prepped, 60), rds("browse_aschar1"))

# Alternating tests
unitizer.prepped@mapping@reviewed <-
  as.logical(seq(length(unitizer.prepped@mapping@reviewed))%%2)
all.equal(as.character(unitizer.prepped, 60), rds("browse_aschar2"))

# Errors / warnings
try(as.character(unitizer.prepped, -1))           # positive
prep.narrow <- as.character(unitizer.prepped, 5)  # too small

all.equal(prep.narrow, rds("browse_ascharnarrow"))

# Colors work (should be last in this section) since the reference @global

unitizer.prepped@global$unitizer.opts[["unitizer.color"]] <- TRUE
old.opt <- options(crayon.enabled = TRUE)
prep.color <- as.character(unitizer.prepped, 60)
all.equal(prep.color, rds("browse_aschar3"))
unitizer.prepped@global$unitizer.opts[["unitizer.color"]] <- FALSE
options(old.opt)

# - "processInput generates Correct Item Structure" ----------------------------

# Here we just test that the calls of each item are what we expect, making
# sure that different behavior for Y or N depending on sub-section type is
# observed correctly (e.g. a Y for new test means keep it, where as for
# removed test means don't keep it)
# For debugging:
# cbind(substr(unitizer:::deparseCalls(unitizer.prepped), 1, 15), as.character(unitizer.prepped@mapping@review.type), unitizer.prepped@mapping@review.val, unitizer.prepped@mapping@reviewed)
# cat(deparse(width=500,
#   lapply(
#     unitizer:::as.list(unitizer:::processInput(unitizer.prepped)),
#     function(x) call("quote", slot(x, "call")))
# ) )
unitizer.prepped@mapping@reviewed <-
  rep(TRUE, length(unitizer.prepped@mapping@reviewed))
unitizer.prepped@mapping@review.val <-
  rep("Y", length(unitizer.prepped@mapping@reviewed))

# Assume user accepted all tests

lapply(
  unitizer:::as.list(unitizer:::processInput(unitizer.prepped)), slot, "call"
)
# Assume user accepted all but 1, 4, 6 and 11, note it isn't completely
# obvious what should be kept since an N for anything but a new and passed
# test will result in some object remaining in the list (typically the
# reference copy thereof)
unitizer.prepped@mapping@review.val[] <- "N"
unitizer.prepped@mapping@review.val[c(2, 6, 8, 12)] <- "Y"
lapply(
  unitizer:::as.list(unitizer:::processInput(unitizer.prepped)), slot, "call"
)
# - "unitizerBrowse subsetting works" ------------------------------------------

# note single bracket subsetting for `unitizerBrowse` overrides the `unitizerList`
# subsetting
unitizer:::deparseCalls(unitizer:::extractItems(unitizer.prepped[c(4, 8, 10)]))
unitizer:::deparseCalls(unitizer:::extractItems(unitizer.prepped[c(2, 3, 11)]))

# - "Reference section mapping works" ------------------------------------------

# Copy over just two sections
my.unitizer3 <- new("unitizer", id = 3, zero.env = zero.env) +
    my.unitizer2@items.new[-(2:6)]
# Exclude section two tests
# sections should copy over
my.unitizer3 <- unitizer:::refSections(my.unitizer3, my.unitizer2)
# just copy over 1st and 3rd sections
identical(my.unitizer3@sections.ref, my.unitizer2@sections[-2])
my.unitizer3@section.ref.map

# Make sure "removed" sections are NA when kept
unitizer.prepped@mapping@reviewed <-
  rep(TRUE, length(unitizer.prepped@mapping@reviewed))
# don't delete removed
unitizer.prepped@mapping@review.val <-
  ifelse(unitizer.prepped@mapping@review.type %in% c("Removed"), "N", "Y")
items.processed <- unitizer:::processInput(unitizer.prepped)
vapply(unitizer:::as.list(items.processed), slot, 1L, "section.id")

# Now try to re-establish sections with removed tests
my.unitizer4 <-
  new("unitizer", id = 4, zero.env = zero.env) + items.processed
# sections should copy over
my.unitizer4 <- unitizer:::refSections(my.unitizer4, my.unitizer2)
is(my.unitizer4@sections.ref[[4L]], "unitizerSectionNA")
my.unitizer4@section.ref.map

# - "Item Extraction" ----------------------------------------------------------

items <- unitizer:::extractItems(unitizer.prepped)
item.calls <- vapply(
  unitizer:::as.list(items),
  function(x)
    paste0(deparse(x@call, width.cutoff = 500), collapse = ""), character(1L)
)
item.types <- vapply(unitizer:::as.list(items), slot, FALSE, "reference")
item.ids <- vapply(unitizer:::as.list(items), slot, 1L, "id")
item.df <- data.frame(item.calls, item.types, item.ids, stringsAsFactors = FALSE)

all.equal(item.df[order(item.types, item.ids),], rds("browse_itemord"))
