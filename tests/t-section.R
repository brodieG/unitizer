source(file.path("_helper", "init.R"))

expr.1 <- expression(1 + 1, b <- 5, matrix(integer(), nrow = b,
    ncol = b))
expr.2 <- {
    1 + 1
    b <- 5
    matrix(integer(), nrow = b, ncol = b)
}
expr.3 <- quote(expression(1 + 1, b <- 5, matrix(integer(), nrow = b,
    ncol = b)))
expr.4 <- quote({
    1 + 1
    b <- 5
    matrix(integer(), nrow = b, ncol = b)
})
# - "simple tests" -------------------------------------------------------------

try(unitizer_sect(1:3))
try(unitizer_sect(letters))
try(unitizer_sect("mytest", expr.1, 1:3))
# note the following two produce error messages, but it's not actually an error,
# it's just that there are multiple errors and `expect_error` only suppresses
# the last one, not the preceding ones.
try(unitizer_sect("mytest", expr.1, letters, letters))
try(unitizer_sect("mytest", expr.1, letters, identity))
try(unitizer_sect("mytest", expr.2))
try(unitizer_sect("mytest", matrix(1:9, nrow = 3)))
is(sect.1 <- unitizer_sect("mytest", expr.1), "unitizerSectionExpression")
identical(unitizer:::as.expression(sect.1), expr.1)

is(sect.2 <- unitizer_sect("mytest", {
    1 + 1
    b <- 5
    matrix(integer(), nrow = b, ncol = b)
}), "unitizerSectionExpression")
identical(sect.1, sect.2)
is(sect.3 <- unitizer_sect("mytest", expr.3), "unitizerSectionExpression")
identical(sect.1, sect.3)
is(sect.4 <- unitizer_sect("mytest", expr.4), "unitizerSectionExpression")
identical(sect.1, sect.4)
is(sect.5 <- unitizer_sect("mytest", expression(1 + 1,
    b <- 5, matrix(integer(), nrow = b, ncol = b))), "unitizerSectionExpression")
identical(sect.1, sect.5)
is(sect.1 <- unitizer_sect("mytest", expr.1, compare = identical),
    "unitizerSectionExpression")
unitizer_sect("hello")  # warn

# - "Custom Comparison Functions" ----------------------------------------------

# Run expressions with different comparison functions
set.seed(1)
expr.1 <- expression(50 + runif(1)/10^10, message("Hello There",
    runif(1)), cat("Hello there", runif(1)), stop("Yo", runif(1)))
expr.2 <- expression(50 + runif(1)/10^10, message("Hello There",
    runif(1)), cat("Hello there", runif(1)), stop("Yo", runif(1)))
expr.3 <- expression(unitizer_sect("change comp funs", compare = identical,
    {
        50 + runif(1)/10^10
        message("Hello There", runif(1))
        cat("Hello there", runif(1))
        stop("Yo", runif(1))
    }))
expr.4 <- expression(
  unitizer_sect(
    "change comp funs",
    compare = testFuns(
      value = identical, output = all.equal, message = all.equal,
      conditions = function(x, y) TRUE),
      {
        50 + runif(1)/10^10
        message("Hello There", runif(1))
        cat("Hello there", runif(1))
        stop("Yo", runif(1))
}))
my.unitizer <- new("unitizer", id = 1, zero.env = new.env())
coi(my.unitizer <- my.unitizer + expr.1)
my.unitizer2 <- new("unitizer", id = 2, zero.env = new.env())
# make previous items into reference items
coi(my.unitizer2 <- my.unitizer2 + my.unitizer@items.new)
# now add back items to compare
coi(my.unitizer2 <- my.unitizer2 + expr.2)
my.unitizer3 <- new("unitizer", id = 3, zero.env = new.env())
# make previous items into reference items
coi(my.unitizer3 <- my.unitizer3 + my.unitizer@items.new)
# now add back items to compare
coi(my.unitizer3 <- my.unitizer3 + expr.3)
my.unitizer4 <- new("unitizer", id = 4, zero.env = new.env())
# make previous items into reference items
coi(my.unitizer4 <- my.unitizer4 + my.unitizer@items.new)
# now add back items to compare
coi(my.unitizer4 <- my.unitizer4 + expr.4)

my.unitizer2@tests.result
my.unitizer3@tests.result
my.unitizer4@tests.result

