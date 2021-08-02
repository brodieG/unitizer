source(file.path("_helper", "init.R"))

lst <- new("unitizerList")

# - "unitizerList basic tests" -------------------------------------------------

length(lst) == 0L
is(lst <- unitizer:::append(lst, 5), "unitizerList")
length(lst) == 1L
is(
  lst <- unitizer:::append(
    lst, list("booyah", list(1:3), matrix(1:9, nrow = 3))
  ), "unitizerList"
)
length(lst) == 4L
is(lst[3L], "unitizerList")
is(lst[[3L]], "list")
lst <- unitizer:::append(lst, list(data.frame(a = letters[1:3])), 2L)
is(lst[[3L]], "data.frame")
length(lst[1:4]) == 4L
lst[[4L]] <- "boo"
is(lst[[4L]], "character")
lst[4L:5L] <- letters[1:2]

c(lst[[4L]], lst[[5L]])
lst[[4L]]

is(unitizer:::as.list(lst), "list")
length(unitizer:::as.list(lst)) == 5L
is(unitizer:::as.expression(lst), "expression")

try(unitizer:::getItem(lst))  # error
lst <- unitizer:::nextItem(lst)
unitizer:::getItem(lst)
lst <- unitizer:::nextItem(lst)
unitizer:::getItem(lst)
lst <- unitizer:::prevItem(lst)
unitizer:::getItem(lst)
lst <<- lst  # leftover from testthat testing?

# - "unitizerList pointer seeking" ---------------------------------------------

for (i in 1:10) lst <- unitizer:::nextItem(lst)
try(unitizer:::getItem(lst))
unitizer:::done(lst)
is(lst <- unitizer:::reset(lst, "back"), "unitizerList")
try(unitizer:::reset(lst, letters))
try(unitizer:::reset(lst, NA_character_))
try(unitizer:::getItem(lst))
lst <- unitizer:::prevItem(lst)
unitizer:::getItem(lst) == "b"
while (!unitizer:::done(lst)) {
    item <- unitizer:::getItem(lst)
    lst <- unitizer:::prevItem(lst)
}
item == 5L
try(unitizer:::getItem(lst))
withCallingHandlers(
  lst[[4]] <- "new value",
  warning = function() stop("A Warning!")
)

for (i in 1:5) lst <- unitizer:::nextItem(lst)
lst@.pointer

# - "unitizerList value replacement and pointer adjustments" -------------------

lst[[4]] <- NULL
lst@.pointer
unitizer:::reset(lst, "back")
lst.len <- length(lst)
identical(lst@.pointer, lst.len)
lst[2:3] <- letters[1:2]
identical(lst@.pointer, lst.len)
lst[2:3] <- list(NULL, NULL)
identical(lst@.pointer, lst.len)
lst[2:3] <- NULL
identical(lst@.pointer, lst.len - 2L)

lst <- unitizer:::reset(lst, "front")
for (i in 1:2) lst <- unitizer:::nextItem(lst)
curr.point <- lst@.pointer
lst[[3]] <- NULL
identical(curr.point, lst@.pointer)
lst <- unitizer:::append(lst, list(5, 6, "blaskdjf"), 1L)
identical(curr.point + 3L, lst@.pointer)
lst <- unitizer:::append(lst, list(matrix(1:9, nrow = 3)), 5L)
identical(curr.point + 3L, lst@.pointer)

# - "Append Factors Works" -----------------------------------------------------

vec <- factor(letters[1:3], levels = letters)
vec2 <- factor(letters[10:15], levels = letters)

all.equal(structure(c(1L, 2L, 3L, 10L, 11L, 12L, 13L, 14L,
  15L), .Label = c("a", "b", "c", "d", "e", "f", "g", "h",
  "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
  "u", "v", "w", "x", "y", "z"), class = "factor"), append(vec,
  vec2))
all.equal(structure(c(1L, 2L, 10L, 11L, 12L, 13L, 14L, 15L,
  3L), .Label = c("a", "b", "c", "d", "e", "f", "g", "h", "i",
  "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u",
  "v", "w", "x", "y", "z"), class = "factor"), append(vec,
  vec2, 2))

all.equal(structure(c(10L, 11L, 12L, 13L, 1L, 2L, 3L, 14L,
  15L), .Label = c("a", "b", "c", "d", "e", "f", "g", "h",
  "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
  "u", "v", "w", "x", "y", "z"), class = "factor"), append(vec2,
  vec, 4))
try(append(vec2, vec, 20))
try(append(vec2, vec, -5))

# - "List coersion works even inside apply functions" --------------------------

ulist <- new("unitizerList", .items = list("a", 1, 2, "b"))
identical(lapply(ulist, identity), ulist@.items)

# - "Errors" -------------------------------------------------------------------

setClass("uhtsdfoqiuerhzb", slots = c(a = "integer"))
dummy <- new("uhtsdfoqiuerhzb", a = 1L)
lst2 <- new("unitizerList", .items = list(1, 2, 3))
try(append(lst2, 5, after = -1))
try(append(lst2, dummy))

lst3 <- new("unitizerList", .items = expression(1, 2, 3))
try(append(lst3, dummy))

# - "Set Names" ----------------------------------------------------------------

nlst <- new("unitizerList", .items = list(a = "a", b = "b"))
names(nlst) <- toupper(names(nlst))
as.list(nlst)

